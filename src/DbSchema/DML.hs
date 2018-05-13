{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module DbSchema.DML where
import           Control.Applicative          (ZipList (..))
import           Control.Arrow                ((&&&))
import           Control.Monad.Catch          (MonadMask, finally)
import           Data.Bifunctor               (second)
import           Data.Function                (on)
import           Data.Functor.Compose         (Compose (..))
import           Data.Kind                    (Constraint, Type)
import           Data.List                    (deleteFirstsBy, union, (\\))
import           Data.Proxy                   (Proxy)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Tagged
import qualified Data.Text                    as T
import           GHC.TypeLits                 (Symbol)
import           Lens.Micro

import           DbSchema.Condition           (Cond, dbCond)
import           DbSchema.Db                  (Db (..), MonadIO (..),
                                               SessionMonad)
import           DbSchema.Def
import           DbSchema.Util.Format         (format)
import           DbSchema.Util.RecLens
import           DbSchema.Util.ToStar         (toStar)


type AppTr f = (Applicative f, Traversable f) --, Show1 f)
type MaskIO m = (MonadMask m, MonadIO m)
type AppMon f m = (AppTr f, MaskIO m)

type BothFlds b sch p r =
  Intersect (Map FstSym0 (TRecFlds b sch p)) (Map FstSym0 (TRecFlds b sch r))
type ParConstr b sch p r =
  ( CRecDef b sch p, TRecChilds b sch p ~ '[]
  , DmlChild b sch (TRecChilds b sch r) p r
  , SubRec (BothFlds b sch p r) r
  , SubRec (BothFlds b sch p r) p
  , TSubRec (BothFlds b sch p r) r ~ TSubRec (BothFlds b sch p r) p
  )
class ( Db b, CRecDef b sch r
      , CTabDef sch t
      , SetPK (TTabDef sch t) b r
      , ParConstr b sch p r
      )
    => DML b sch (t::Symbol) p r where
  dmlRecNames :: [T.Text]
  dmlRecNames = map fst $ recDbDef @b @sch @r
  dmlParNames :: [T.Text]
  dmlParNames = map fst $ recDbDef @b @sch @p
  dmlInsert :: ( AppMon f m
               -- проверить наличие обязательных полей!
               )
      => f (p, [r]) -> SessionMonad b m (f [r])
  dmlInsert (vals :: f (p,[r]))
    | all (null . snd) vals = return $ snd <$> vals
    | otherwise             = do
      cmd <- prepareCommand @b sql
      rs <- finally (mapM (run cmd) vals) (finalizePrepared @b cmd)
      childInsert @b @sch @(TRecChilds b sch r)
        $ (,) <$> (fst <$> vals) <*> rs

    where
      td = toStar @_ @(TTabDef sch t)
      autoPK = if tdAutoIns td then tdKey td else []

      sql = format "INSERT INTO {} ({}) VALUES ({})"
                (toStar @_ @t, T.intercalate "," ns, T.intercalate "," ps)
      autoFlds = autoPK ++ dmlParNames @b @sch @t @p @r
      (ps,ns) = unzip
              $ zip (map (paramName @b) [0..])
              $ dmlParNames @b @sch @t @p @r
                ++ (dmlRecNames @b @sch @t @p @r \\ autoFlds)
      rc par vs = let parent = recToDb @b @sch @p par in
        map ( setSub @(BothFlds b sch p r) (getSub @(BothFlds b sch p r) par)
          &&& ( (parent ++)
              . map snd . filter (\(n,_) -> n `notElem` autoFlds)
              . zip (dmlRecNames @b @sch @t @p @r) . recToDb @b @sch @r
              )
          ) vs
      run cmd (par, vs) =
        mapM (\(v,fs) -> runPrepared @b cmd fs >> setPK @(TTabDef sch t) @b v)
          $ rc par vs

  -- dmlDelete сама не делает каскадного удаления. Отдельно dmlDeleteCascade
  -- Варианты удаления:
  -- - по условию
  -- - по условию, с каскадным удалением по каскадным отношениям
  -- - по условию, с каскадным удалением всех потомков
  -- - по образцам
  -- - набор идентификаторов
  dmlDeleteCond :: MaskIO m => Cond b sch t -> SessionMonad b m ()
  dmlDeleteCond cond = do
      cmd <- prepareCommand @b sql
      finally (runPrepared @b cmd ps) (finalizePrepared @b cmd)
    where
      (wh,ps) = dbCond cond
      sql = format "DELETE FROM {} WHERE {}" (toStar @_ @t, wh)

  dmlDelete :: AppMon f m => f (p, [r]) -> SessionMonad b m ()
  dmlDelete (vals :: f (p,[r]))
    | all (null . snd) vals = return ()
    | otherwise             = do
      childDelete @b @sch @(TRecChilds b sch r) vals
      cmd <- prepareCommand @b sql
      finally (mapM_ (mapM_ (runPrepared @b cmd) . rc) vals)
              (finalizePrepared @b cmd)

    where
      rc (par, vs) = let parent = recToDb @b @sch @p par in
        map ((parent ++)
            . map snd
            . filter (\(n,_) -> n `notElem` dmlParNames @b @sch @t @p @r)
            . zip (dmlRecNames @b @sch @t @p @r) . recToDb @b @sch @r) vs

      -- rc p r =
      (ps,ns) = unzip
              $ zip (map (paramName @b) [0..])
              $ dmlParNames @b @sch @t @p @r `union` dmlRecNames @b @sch @t @p @r
      sql = format "DELETE FROM {} WHERE {}"
                  ( toStar @_ @t
                  , T.intercalate " AND "
                      $ zipWith (\n p -> format "{} = {}" (n, p)) ns ps
                  )

  dmlSelect :: AppMon f m => f p -> SessionMonad b m (f [r])
  dmlSelect ps
    | null ps   = return (const [] <$> ps)
    | otherwise = do
      cmd <- prepareCommand @b sql
      rs <- fmap (second $ map (either (error . T.unpack) id
                                . getFromDb @b @sch))
        <$> finally (mapM (\p -> (p,) <$> (runSelect @b cmd
                                            $ recToDb @b @sch p)) ps)
                    (finalizePrepared @b cmd)
      childSelect @b @sch @(TRecChilds b sch r) rs
    where
      sql = format "SELECT {} FROM {} t0 {}"
            ( T.intercalate "," $ dmlRecNames @b @sch @t @p @r
            , tabName @sch @t
            , if T.null whereCond then "" else "WHERE " `mappend` whereCond
            )
      whereCond = T.intercalate " AND "
                $ zipWith (\n s -> format "{} = {}" (s, paramName @b n))
                          [0..] (dmlParNames @b @sch @t @p @r)

class DmlChild b sch (cs :: [(Symbol,Type)]) p r where
  type FstChild b sch cs p r
  fstChild :: p -> r -> FstChild b sch cs p r
  childInsert :: AppMon f m => f (p,[r]) -> SessionMonad b m (f [r])
  childDelete :: AppMon f m => f (p,[r]) -> SessionMonad b m ()
  childSelect :: AppMon f m => f (p,[r]) -> SessionMonad b m (f [r])

instance DmlChild b sch '[] p r where
  type FstChild b sch '[] p r = ()
  fstChild _ _ = ()
  childInsert = return . fmap snd
  childDelete _ = return ()
  childSelect = return . fmap snd

-- class ToMaybe sch t ns v where
--   type TToMaybe sch t ns v
--   toMaybe :: v -> TToMaybe sch t ns v
-- instance ToMaybe sch t '[] () where
--   type TToMaybe sch t ns v = ()
--   toMaybe = id
-- instance ToMaybe sch t '[n] v where
--   type TToMaybe sch t ns v = ()
--   toMaybe = id

type RefCols sch s = RdCols (TRelDef sch s)
type ChildCols sch s = Map FstSym0 (RefCols sch s)
type ChildTab sch s = RdFrom (TRelDef sch s)
instance (DmlChild b sch cs p r, rd ~ TRelDef sch s
        , RecLens s r, TLens s r ~ [r0]
        , Map FstSym0 (RdCols rd) ~ cc
        , Map SndSym0 (RdCols rd) ~ cp
        , SubRec cp (p,r), TSubRec cp (p,r) ~ rp
        , RdFrom rd ~ ct
        , ('(s,t) ': cs) ~ css
        , CTabDef sch ct, TTabRec sch ct ~ rc
        , SubRec cc rc
        , MbMaybe cc rp (TSubRec cc rc)
        , DML b sch ct (Tagged cc (TSubRec cc rc)) r0
        )
      => DmlChild b sch ( '(s,t) ': cs) p r where
  type FstChild b sch ( '(s,t) ': cs) p r =
    ( Tagged  (ChildCols sch s)
              (TSubRec (ChildCols sch s) (TTabRec sch (ChildTab sch s)))
    , TLens s r)
  fstChild p r = ( Tagged @cc $ mbMaybe @cc $ getSub @cp (p,r)
                 , r ^. recLens @s
                 )
  childInsert rs = do
    ch <- (fmap getZipList . getCompose)
      <$> ( dmlInsert @b @sch @ct
          . Compose
          . fmap (ZipList . (\(p,rs') -> map (fstChild @b @sch @css p) rs'))
          ) rs
    childInsert @b @sch @cs $ (,) <$> (fst <$> rs)
                <*> ((zipWith (\c -> recLens @s .~ c)) <$> ch <*> (snd <$> rs))
    -- where
    --   getChild :: p -> r -> (Tagged cc rp, [r0])
    --   getChild p r = (Tagged @cc $ getSub @cp (p,r), r ^. recLens @s)

  childDelete rs = do
    dmlDelete @b @sch @ct
      $ Compose
      $ fmap (ZipList . (\(p,rs') -> map (fstChild @b @sch @css p) rs')) rs
    childDelete @b @sch @cs rs

  childSelect rs
    | all (null . snd) rs = return $ snd <$> rs
    | otherwise           = do
      ch <- (fmap getZipList . getCompose)
        <$> ( dmlSelect @b @sch @ct
            . Compose
            . fmap ( ZipList
                   . (\(p,rs') -> map (fst . fstChild @b @sch @css p) rs')
                   )
            ) rs
      childSelect @b @sch @cs
        $ (,) <$> (fst <$> rs)
              <*> ((zipWith (\c -> recLens @s .~ c)) <$> ch <*> (snd <$> rs))

    -- rs' <- undefined
    -- childSelect @b @sch @cs @p @r rs'


