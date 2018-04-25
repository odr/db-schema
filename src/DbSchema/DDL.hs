{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module DbSchema.DDL where
import           Control.Monad.IO.Class (MonadIO)
import           Data.Proxy             (Proxy (..))
import qualified Data.Text              as T
import           GHC.TypeLits           (Symbol)

import           DbSchema.Db
import           DbSchema.Def
import           DbSchema.Util.Format   (Only (..), format)
import           DbSchema.Util.ToStar

class (Db b, CSchema sch, CTabDef sch s
      , ToStar (DbFldTypes b (TFldTypes sch s))) => DDLTab b sch s where
  ddlCreateTableText :: T.Text
  ddlCreateTableText
    = createTableText @b
        (tabName @sch @s)
        (zip (tdFlds td) (toStar @_ @(DbFldTypes b (TFldTypes sch s))))
        (tdKey td)
        (tdUniq td)
    where
      td = tabDef @sch @s

  createTable :: MonadIO m => SessionMonad b m ()
  createTable = execCommand @b (ddlCreateTableText @b @sch @s)

class DDLTabs b sch (ss::[Symbol]) where
  createTables :: MonadIO m => SessionMonad b m ()

instance DDLTabs b sch '[] where
  createTables = return ()

instance (DDLTab b sch s, DDLTabs b sch ss) => DDLTabs b sch (s:ss) where
  createTables = createTable @b @sch @s >> createTables @b @sch @ss

class (Db b, CSchema sch, CRelDef sch s) =>  DDLRel b sch s where
  ddlCreateRelText :: T.Text
  ddlCreateRelText
    = createRelText @b (relName @sch @s) <$> rdFrom <*> rdTo <*> rdCols
    $ relDef @sch @s

  createRel :: MonadIO m => SessionMonad b m ()
  createRel = execCommand @b (ddlCreateRelText @b @sch @s)

class DDLRels b sch (ss::[Symbol]) where
  createRels :: MonadIO m => SessionMonad b m ()

instance DDLRels b sch '[] where
  createRels = return ()

instance (DDLRel b sch s, DDLRels b sch ss) => DDLRels b sch (s:ss) where
  createRels = createRel @b @sch @s >> createRels @b @sch @ss

class (CSchema sch, DDLTabs b sch (TTables sch), DDLRels b sch (TRels sch))
    => DDLSchema b sch where
  createSchema :: MonadIO m => SessionMonad b m ()
  createSchema = do
    createTables @b @sch @(TTables sch)
    createRels @b @sch @(TRels sch)


-- class CSchema sch => DDL b sch where
--   createSchema
--     :: (MonadIO m, DDLList b sch (TTables sch), DDLList b sch (TRels sch))
--     => SessionMonad b m ()
--   createSchema = do
--     createTables @b @sch @(TTables sch)
--     createRels @b @sch @(TRels sch)
--   createTable   :: MonadIO m => Proxy# (s::Symbol) -> SessionMonad b m ()
--   createRel     :: MonadIO m => Proxy# (s::Symbol) -> SessionMonad b m ()
--
--   -- createTextTable :: Proxy# (s::Symbol) -> Text
--   -- createTextTable
--   --   = formatS "CREATE TABLE {} {} ({}, PRIMARY KEY ({}) {})"
--   --       ( afterCreateTableText @b
--   --       , fromSing (sing :: )@s
--   --       , T.intercalate ","
--   --           $ zipWith (\n (t,b) -> formatS "{} {} {} NULL"
--   --                                           (n,t, if b then T.empty else "NOT"))
--   --                     undefined -- (fldNames @NoLstFld @(DsRec ds))
--   --                     (fromSing (sing :: Sing (DbFieldTypes b (DsRec ds))))
--   --       , T.intercalate "," $ diKey di
--   --       , foldMap (formatS ",UNIQUE ({})" . Only . T.intercalate ",")
--   --           $ diUniq di
--   --       )
--   --     where
--   --       (Just sds) = sGetDataStruct st ssch
--   --       srs = sGetCreateRef st ssch
--   --       di = fromSing $ sGetDsInfo sds
--
-- class DDLList b sch (ss::[Symbol]) where
--   createTables  :: MonadIO m => SessionMonad b m ()
--   createRels    :: MonadIO m => SessionMonad b m ()
--
-- instance DDLList b sch '[] where
--   createTables = return ()
--   createRels = return ()
--
-- instance (DDLList b sch ss, DDL b sch) => DDLList b sch (s ': ss) where
--   createTables = do
--     createTable  @b @sch (proxy# ::Proxy# s)
--     createTables @b @sch @ss
--   createRels = do
--     createRel  @b @sch (proxy# ::Proxy# s)
--     createRels @b @sch @ss
--
-- -- createTextTable :: (GetDataStruct t sch ~ Just ds, DDLCons b ds)
-- --                 => Proxy# b -> Sing (t::Symbol) -> Sing (sch::Schema) -> Text
-- -- createTextTable (_ :: Proxy# b) (st :: Sing t) ssch
-- -- createTextTable
-- --   = formatS "CREATE TABLE {} {} ({}, PRIMARY KEY ({}) {})"
-- --       ( afterCreateTableText @b
-- --       , fromSing $ getDsName sds
-- --       , T.intercalate ","
-- --           $ zipWith (\n (t,b) -> formatS "{} {} {} NULL"
-- --                                           (n,t, if b then T.empty else "NOT"))
-- --                     undefined -- (fldNames @NoLstFld @(DsRec ds))
-- --                     (fromSing (sing :: Sing (DbFieldTypes b (DsRec ds))))
-- --       , T.intercalate "," $ diKey di
-- --       , foldMap (formatS ",UNIQUE ({})" . Only . T.intercalate ",")
-- --           $ diUniq di
-- --       )
-- --     where
-- --       (Just sds) = sGetDataStruct st ssch
-- --       srs = sGetCreateRef st ssch
-- --       di = fromSing $ sGetDsInfo sds
-- --
-- -- , foldMap (formatS ",FOREIGN KEY ({}) REFERENCES {} ({}) {} "
-- --           . ((,,,)  <$> T.intercalate "," . map fst . refCols
-- --                     <*> refTo
-- --                     <*> T.intercalate "," . map snd . refCols
-- --                     <*> deleteConstraintText @b . refDelCons
-- --             )
-- --           ) $ fromSing srs
