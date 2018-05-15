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
import           Data.Bifunctor               (bimap, second)
import           Data.Functor.Compose         (Compose (..))
import           Data.Kind                    (Type)
import           Data.List                    ((\\))
import qualified Data.Map                     as M
import           Data.Maybe                   (fromMaybe)
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

parEmpty :: Tagged ('[] :: [Symbol]) ()
parEmpty = Tagged @('[] :: [Symbol]) ()

type BothFlds b sch p r =
  Intersect (Map FstSym0 (TRecFlds b sch p)) (Map FstSym0 (TRecFlds b sch r))
type KeyNames sch t = TdKey (TTabDef sch t)
type Key sch t p r = Tagged (KeyNames sch t) (TSubRec (KeyNames sch t) (p,r))
type ParConstr b sch t p r =
  ( CRecDef b sch p, TRecChilds b sch p ~ '[]
  , DmlChild b sch (TRecChilds b sch r) p r
  , SubRec (BothFlds b sch p r) r
  , SubRec (BothFlds b sch p r) p
  , TSubRec (BothFlds b sch p r) r ~ TSubRec (BothFlds b sch p r) p
  , SubRec (KeyNames sch t) (p,r)
  , Ord (TSubRec (TdKey (TTabDef sch t)) (p,r))
  , Show p, Show r, Show (TSubRec (KeyNames sch t) (p, r))
  , CRecDef b sch (Key sch t p r)
  )
class ( Db b, CRecDef b sch r
      , CTabDef sch t
      , SetPK (TTabDef sch t) b r
      , ParConstr b sch t p r
      )
    => DML b sch (t::Symbol) p r where
  dmlRecNames :: [T.Text]
  dmlRecNames = map fst $ recDbDef @b @sch @r
  dmlParNames :: [T.Text]
  dmlParNames = map fst $ recDbDef @b @sch @p
  dmlKeyNames :: [T.Text]
  dmlKeyNames = map fst $ recDbDef @b @sch @(Key sch t p r)
  dmlKeyVals :: p -> r -> [FieldDB b]
  dmlKeyVals p  = recToDb @b @sch
                . Tagged @(KeyNames sch t) . getSub @(KeyNames sch t) . (p,)
  dmlInsert :: ( AppMon f m
               -- проверить наличие обязательных полей!
               )
      => f (p, [r]) -> SessionMonad b m (f [r])
  dmlInsert = dmlInsertDef (Proxy @'(b,sch,t))
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
  dmlDelete = dmlDeleteDef (Proxy @'(b,sch,t))

  dmlSelect :: AppMon f m => f p -> SessionMonad b m (f [r])
  dmlSelect = dmlSelectDef (Proxy @'(b,sch,t,r))

  dmlUpdate :: AppMon f m => Bool -> f (p,[r],[r]) -> SessionMonad b m (f [r])
  dmlUpdate = dmlUpdateDef (Proxy @'(b,sch,t))

class DmlChild b sch (cs :: [(Symbol,Type)]) p r where
  type FstChild b sch cs p r
  fstChild :: p -> r -> FstChild b sch cs p r
  childInsert :: AppMon f m => f (p,[r]) -> SessionMonad b m (f [r])
  childDelete :: AppMon f m => f (p,[r]) -> SessionMonad b m ()
  childSelect :: AppMon f m => f (p,[r]) -> SessionMonad b m (f [r])
  childUpdate :: AppMon f m => Bool -> f (p,[(r,r)]) -> SessionMonad b m (f [r])

instance DmlChild b sch '[] p r where
  type FstChild b sch '[] p r = ()
  fstChild _ _ = ()
  childInsert = return . fmap snd
  childDelete _ = return ()
  childSelect = return . fmap snd
  childUpdate _ = return . fmap (map fst . snd)

type RefCols sch s = RdCols (TRelDef sch s)
type ChildCols sch s = Map FstSym0 (RefCols sch s)
type ChildTab sch s = RdFrom (TRelDef sch s)
instance (DmlChild b sch cs p r, rd ~ TRelDef sch s
        , RecLens s r, TLens s r ~ [r0]
        , Map FstSym0 (RdCols rd) ~ cc
        , Map SndSym0 (RdCols rd) ~ cp
        , SubRec cp (p,r), TSubRec cp (p,r) ~ rp
        , RdFrom rd ~ ct
        , ('(s,r0) ': cs) ~ css
        , CTabDef sch ct, TTabRec sch ct ~ rc
        , SubRec cc rc
        , MbMaybe cc rp (TSubRec cc rc)
        , DML b sch ct (Tagged cc (TSubRec cc rc)) r0
        )
      => DmlChild b sch ( '(s,r0) ': cs) p r where
  type FstChild b sch ( '(s,r0) ': cs) p r =
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

  childUpdate isDiff us = do
    us' <- (fmap getZipList . getCompose)
        <$> ( dmlUpdate @b @sch @ct isDiff
            . Compose
            . fmap (ZipList . (\(p,rs) ->
                    map ( (\((k,os),(_,ns)) -> (k,os,ns))
                        . bimap (fstChild @b @sch @css p)
                                (fstChild @b @sch @css p)
                        ) rs))
            ) us
    childUpdate @b @sch @cs isDiff $ setUpd <$> us <*> us'
      where
        setUpd (p, par) ch =
          (p, zipWith (\c (o,n) -> (o, n & recLens @s .~ c)) ch par)

--------
dmlInsertDef :: (DML b sch t p r, AppMon f m)
              => Proxy '(b,sch,t) -> f (p, [r]) -> SessionMonad b m (f [r])
dmlInsertDef (_::Proxy '(b,sch,t)) (vals :: f (p,[r]))
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
--
dmlDeleteDef :: (DML b sch t p r, AppMon f m)
            => Proxy '(b,sch,t) -> f (p, [r]) -> SessionMonad b m ()
dmlDeleteDef (_::Proxy '(b,sch,t)) (vals :: f (p,[r]))
  | all (null . snd) vals = return ()
  | otherwise             = do
    childDelete @b @sch @(TRecChilds b sch r) vals
    cmd <- prepareCommand @b sql
    finally (mapM_ (mapM_ (runPrepared @b cmd) . rc) vals)
            (finalizePrepared @b cmd)
  where
    rc (par,vs) = map (dmlKeyVals @b @sch @t par) vs

    -- rc p r =
    (ps,ns) = unzip
            $ zip (map (paramName @b) [0..])
            $ dmlKeyNames @b @sch @t @p @r
    sql = format "DELETE FROM {} WHERE {}"
                ( toStar @_ @t
                , T.intercalate " AND "
                    $ zipWith (\n p -> format "{} = {}" (n, p)) ns ps
                )
--
dmlSelectDef :: (DML b sch t p r, AppMon f m)
            => Proxy '(b,sch,t,r) -> f p -> SessionMonad b m (f [r])
dmlSelectDef (_::Proxy '(b,sch,t,r)) (ps ::f p)
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
--
dmlUpdateDef :: (DML b sch t p r, AppMon f m)
            => Proxy '(b,sch,t) -> Bool -> f (p,[r],[r]) -> SessionMonad b m (f [r])
dmlUpdateDef (_ :: Proxy '(b,sch,t)) isDiff (vals :: f (p,[r],[r])) = do
  -- liftIO $ mapM print vals'
  dmlDelete @b @sch @t ds
  if isDiff then updateDiff else updateFull
  us' <- childUpdate @b @sch @(TRecChilds b sch r) isDiff us
  ins' <- dmlInsert @b @sch @t ins
  return $ setInsUpd <$> vals' <*> ins' <*> us'
  where
    recNames = dmlRecNames @b @sch @t @p @r
    keyNames = dmlKeyNames @b @sch @t @p @r
    sql mbDifs = let names = fromMaybe recNames mbDifs in
      format "UPDATE {} SET {} WHERE {}"
        ( tabName @sch @t
        , T.intercalate ","
            $ zipWith (\n s -> format "{} = {}" (s, paramName @b n))
                      [0..] names
        , T.intercalate " AND "
            $ zipWith (\n s -> format "{} = {}" (s, paramName @b n))
                      [(length names)..] keyNames
        )
    --
    updateDiff = mapM_ (\(p,us') ->
                      mapM_ (\x@(_,n) -> run p n $ diff x) us') us
      where
        run p r difs
          | null difs = return ()
          | otherwise = do
            cmd <- prepareCommand @b $ sql $ Just $ map fst difs
            finally (runPrepared @b cmd $ map snd difs
                                        ++ dmlKeyVals @b @sch @t p r)
                    (finalizePrepared @b cmd)
        diff = map (second snd)
              . filter (uncurry (/=) . snd)
              . zip recNames
              . uncurry zip
              . bimap (recToDb @b @sch) (recToDb @b @sch)
    --
    updateFull
      | all (null . snd) us = mapM_ (\_ -> return ()) us
      | otherwise = do
        cmd <- prepareCommand @b $ sql Nothing
        finally (mapM_ (\(p,us') -> mapM_ (\(_,n) -> run cmd p n) us') us)
                (finalizePrepared @b cmd)
        where
          run cmd p n = runPrepared @b cmd
                          (recToDb @b @sch n ++ dmlKeyVals @b @sch @t p n)
    --
    key = getSub @(KeyNames sch t)
    mkMap p = M.fromList . map (\x -> (key (p,x), x))
    vals' = fmap (\(p,olds,news) -> (p, mkMap p olds, mkMap p news)) vals
    ds = fmap (\(p,olds,news) -> (p, M.elems $ M.difference olds news)) vals'
    ins = fmap (\(p,olds,news) -> (p, M.elems $ M.difference news olds)) vals'
    setInsUpd (p,_,news) news' upd =
      M.elems $ M.unions [mkMap p news', mkMap p upd, news]
    us  = fmap (\(p,olds,news)
              -> (p, M.elems $ M.intersectionWith (,) olds news)) vals'


