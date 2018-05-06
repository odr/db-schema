{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module DbSchema.Db.Db (module DbSchema.Db.Db) where

import           Control.Monad.Catch        (MonadCatch)
import           Control.Monad.IO.Class     as DbSchema.Db.Db
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           DbSchema.Util.Format



data DelCons = DcRestrict | DcCascade | DcSetNull
  deriving (Show, Eq, Ord, Read)

type SessionMonad b m = ReaderT (Conn b) m

-- | Options for backend
class Db back where
  type FieldDB back
  type Conn back
  type SessionParams back
  type PrepCmd back
  type GenKey back         -- set to () if generation is impossible
  paramName :: Int -> Text -- ^ How to create param name (like "?1") from param num

  createTableText :: Text -> [(Text,(Text,Bool))] -> [Text] -> [[Text]] -> Text
  createTableText t fs pk = format "CREATE TABLE {} ({}, PRIMARY KEY ({}) {})"
                          . createTableParams @back t fs pk
  createTableParams :: Text -> [(Text,(Text,Bool))] -> [Text] -> [[Text]]
                    -> (Text,Text,Text,Text)
  createTableParams t fs pk uk =
    ( t, T.intercalate "," $ map rc fs, T.intercalate "," pk
    , foldMap (format ", UNIQUE ({})" . Only . T.intercalate ",") uk
    )
    where
      rc (n, (t,b)) = format "{} {} {} NULL" (n,t, if b then T.empty else "NOT")

  createRelText :: Text -> Text -> Text -> [(Text,Text)] -> Text
  createRelText c f t =
    format "ALTER TABLE {} ADD CONSTRAINT {} FOREIGN KEY ({}) REFERENCES {}({})"
    . createRelParams @back c f t
  createRelParams :: Text -> Text -> Text -> [(Text,Text)]
                  -> (Text, Text, Text, Text, Text)
  createRelParams name from to cols =
    ( from, name, T.intercalate "," $ map fst cols
    , to, T.intercalate "," $ map snd cols
    )

  runSession :: (MonadIO m, MonadCatch m)
          => SessionParams back -> SessionMonad back m a -> m a
  prepareCommand :: MonadIO m => Text -> SessionMonad back m (PrepCmd back)
  -- | Executed before runPrepared in insertAuto operation.
  --   We can get there new key and put it into monad.
  preRunInAuto :: MonadIO m => SessionMonad back m ()
  runPrepared :: MonadIO m
              => PrepCmd back -> [FieldDB back] -> SessionMonad back m ()
  runSelect :: MonadIO m => PrepCmd back -> [FieldDB back]
                         -> SessionMonad back m [[FieldDB back]]
  finalizePrepared :: MonadIO m => PrepCmd back -> SessionMonad back m ()
  getLastKey :: MonadIO m => SessionMonad back m (GenKey back)
  execCommand :: MonadIO m => Text -> SessionMonad back m ()
  execCommand t = runCommand @back t []
  deleteConstraintText :: DelCons -> Text
  deleteConstraintText DcRestrict = "ON DELETE RESTRICT"
  deleteConstraintText DcCascade  = "ON DELETE CASCADE"
  deleteConstraintText DcSetNull  = "ON DELETE SET NULL"

  runCommand :: MonadIO m => Text -> [FieldDB back] -> SessionMonad back m ()
  runCommand sql pars = do
    cmd <- prepareCommand @back sql
    runPrepared @back cmd pars

  condLike :: Text -> Text -> Text
  condLike name par
      = format "lower({}) LIKE '%' + lower({}) + '%'" (name, par)
