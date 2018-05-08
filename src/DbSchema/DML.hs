{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module DbSchema.DML where
import           Control.Applicative     (ZipList (..))
import           Control.Monad.Catch     (MonadMask, finally)
-- import           Control.Monad.IO.Class  (MonadIO (..))
import           Data.Function           (on)
-- import           Data.Functor.Classes    (Show1 (..))
import           Data.Functor.Compose    (Compose (..))
import           Data.Kind               (Constraint, Type)
import           Data.List               (deleteFirstsBy, (\\))
import           Data.Proxy              (Proxy)
import           Data.Singletons.Prelude
import           Data.Tagged
import qualified Data.Text               as T
import           GHC.TypeLits            (Symbol)
import           Lens.Micro

import           DbSchema.Db             (Db (..), MonadIO (..), SessionMonad)
import           DbSchema.Def
-- (CRecDef (..), CTabDef (..),
--                                           SetPK (..), TabDef (..))
import           DbSchema.Util.Format    (format)
import           DbSchema.Util.RecLens
import           DbSchema.Util.ToStar    (toStar)


type AppTr f = (Applicative f, Traversable f) --, Show1 f)
type MaskIO m = (MonadMask m, MonadIO m)
type AppMon f m = (AppTr f, MaskIO m)

-- deriving instance Show1 ZipList

class ( Db b, CRecDef b sch r
      , CTabDef sch t
      , CRecDef b sch p, TRecChilds b sch p ~ '[]
      , SetPK (TTabDef sch t) b r
      , DmlChild b sch (TRecChilds b sch r) r
      )
    => DML b sch (t::Symbol) p r where
  dmlInsert :: ( AppMon f m
               -- , Show (f (p,[r]))
               -- проверить наличие обязательных полей!
               )
      => f (p, [r]) -> SessionMonad b m (f [r])
  dmlInsert (vals :: f (p,[r]))
    | all (null . snd) vals = return $ snd <$> vals
    | otherwise = do
    -- liftIO $ print vals
    cmd <- prepareCommand @b sql
    rs <- finally (mapM (run cmd) vals) (finalizePrepared @b cmd)
    childInsert @b @sch @(TRecChilds b sch r) rs

    where
      td = toStar @_ @(TTabDef sch t)
      autoPK = if tdAutoIns td then tdKey td else []

      sql = format "INSERT INTO {} ({}) VALUES ({})"
                (toStar @_ @t, T.intercalate "," ns, T.intercalate "," ps)
      parentNames = map fst $ recDbDef @b @sch @p
      autoFlds = autoPK ++ parentNames
      rd = map fst $ recDbDef @b @sch @r
      (ps,ns) = unzip
              $ zip (map (paramName @b) [0..])
              $ parentNames ++ (rd \\ autoFlds)
      rc (par,vs) = let parent = recToDb @b @sch @p par in
          zip vs $ map ((parent ++)
              . map snd
              . filter (\(n,v) -> n `notElem` autoFlds)
              . zip rd
              . recToDb @b @sch @r
            ) vs
      -- run :: PrepCmd b -> (p,[r]) -> SessionMonad b m [r]
      run cmd (par, vs) =
        mapM (\(v,fs) -> runPrepared @b cmd fs >> setPK @(TTabDef sch t) @b v)
          $ rc (par,vs)

class DmlChild b sch (cs :: [(Symbol,Type)]) r where
  childInsert :: AppMon f m => f [r] -> SessionMonad b m (f [r])

instance DmlChild b sch '[] r where
  childInsert = return

instance (DmlChild b sch cs r, rd ~ TRelDef sch s
        , RecLens s r, TLens s r ~ [r0]
        , Map FstSym0 (RdCols rd) ~ cc
        , Map SndSym0 (RdCols rd) ~ cp
        , SubRec cp r, TSubRec cp r ~ rp
        , DML b sch (RdFrom rd) (Tagged cc rp) r0
        -- , Show rp, Show r0
        )
      => DmlChild b sch ( '(s,t) ': cs) r where
  childInsert rs = do
    ch <- (fmap getZipList . getCompose)
      <$> ( dmlInsert @b @sch @(RdFrom rd)
          . Compose . fmap (ZipList . map getChild)
          ) rs
    childInsert @b @sch @cs $ (zipWith (\c -> recLens @s .~ c)) <$> ch <*> rs
    where
      -- getChild :: r -> (Tagged cc rp, TLens s r)
      getChild r = (Tagged @cc $ getSub @cp r, r ^. recLens @s)
