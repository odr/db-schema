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

class (Db b, CSchema sch, CTabDef sch s, CRecDef b sch (TRec sch s))
    => DDLTab b sch s where
  ddlCreateTableText :: T.Text
  ddlCreateTableText
    = createTableText @b (tabName @sch @s) (recDbDef @b @sch @(TRec sch s))
                      <$> tdKey <*> tdUniq $ tabDef @sch @s

  createTable :: MonadIO m => SessionMonad b m ()
  createTable = execCommand @b (ddlCreateTableText @b @sch @s)

  ddlDropTableText :: T.Text
  ddlDropTableText = dropTableText @b (tabName @sch @s)

  dropTable :: MonadIO m => SessionMonad b m ()
  dropTable = execCommand @b (ddlDropTableText @b @sch @s)


class DDLTabs b sch (ss::[Symbol]) where
  createTables :: MonadIO m => SessionMonad b m ()
  dropTables   :: MonadIO m => SessionMonad b m ()

instance DDLTabs b sch '[] where
  createTables = return ()
  dropTables   = return ()

instance (DDLTab b sch s, DDLTabs b sch ss) => DDLTabs b sch (s:ss) where
  createTables = createTable @b @sch @s >> createTables @b @sch @ss
  dropTables = dropTable @b @sch @s >> dropTables @b @sch @ss


class (Db b, CSchema sch, CRelDef sch s) =>  DDLRel b sch s where
  ddlCreateRelText :: T.Text
  ddlCreateRelText
    = createRelText @b (relName @sch @s) <$> rdFrom <*> rdTo <*> rdCols
    $ relDef @sch @s

  createRel :: MonadIO m => SessionMonad b m ()
  createRel | T.null rt = return ()
            | otherwise = execCommand @b rt
    where
      rt = ddlCreateRelText @b @sch @s

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

  dropSchema :: MonadIO m => SessionMonad b m ()
  dropSchema = dropTables @b @sch @(TTables sch)
