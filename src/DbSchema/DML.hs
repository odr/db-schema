{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module DbSchema.DML where
import           Control.Monad.Catch  (MonadMask, finally)
import           Data.Function        (on)
import           Data.Kind            (Constraint, Type)
import           Data.List            (deleteFirstsBy, (\\))
import           Data.Proxy           (Proxy)
import           Data.Tagged          (Tagged)
import qualified Data.Text            as T
import           GHC.TypeLits         (Symbol)

import           DbSchema.Db          (Db (..), MonadIO, SessionMonad)
import           DbSchema.Def         (CRecDef (..), CTabDef (..), SetPK (..),
                                       TabDef (..))
import           DbSchema.Util.Format (format)
import           DbSchema.Util.ToStar (toStar)

type AppTr f = (Applicative f, Traversable f)
type MaskIO m = (MonadMask m, MonadIO m)
type AppMon f m = (AppTr f, MaskIO m)

class ( Db b, CRecDef b sch r
      , CTabDef sch t
      , CRecDef b sch p, TRecChilds b sch p ~ '[]
      , SetPK (TTabDef sch t) b r
      )
    => DML b sch (t::Symbol) r p where
  dmlInsert :: ( AppMon f m
               -- , CTabDef sch t
               -- , CRecDef b sch (Tagged k v)
               -- , SetPK (TTabDef sch t) b a
               -- проверить наличие обязательных полей!
               )
      => p -> f r -> SessionMonad b m (f r)
  dmlInsert par vals = do
    cmd <- prepareCommand @b sql
    rs <- finally (mapM (run cmd) vals) (finalizePrepared @b cmd)
    return rs
    where
      td = toStar @_ @(TTabDef sch t)
      autoPK = if tdAutoIns td then tdKey td else []

      sql = format "INSERT INTO {} ({}) VALUES ({})"
                (toStar @_ @t, T.intercalate "," ns, T.intercalate "," ps)
      parentNames = map fst $ recDbDef @b @sch @p
      autoFlds = autoPK ++ parentNames
      parent = recToDb @b @sch @p par :: [FieldDB b]
      rd = map fst $ recDbDef @b @sch @r
      (ps,ns) = unzip
              $ zip (map (paramName @b) [0..])
              $ parentNames ++ (rd \\ autoFlds)
      vss = fmap rc vals
      rc  = (parent ++)
          . map snd
          . filter (\(n,v) -> n `notElem` autoFlds)
          . zip rd
          . recToDb @b @sch @r
      run cmd v = do
        runPrepared @b cmd (rc v)
        setPK @(TTabDef sch t) @b v
--
-- class DmlChild b sch a (cs :: [(Symbol,Type)]) where
--   childInsert :: AppMon f m => f a -> SessionMonad b m (f a)
--
-- instance DmlChild b sch a '[] where
--   childInsert = return
--
-- instance DmlChild b sch a cs => DmlChild b sch a (c ': cs) where
--   childInsert r = do
--     r' <- undefined
--     childInsert @b @sch @a @cs r'
