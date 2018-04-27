{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}
module DbSchema.Db.Sqlite
    ( Sqlite, SQLData
    )
    where

import           Control.Monad.Catch        (SomeException, catch, throwM)
-- import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ReaderT (..), ask)
import           Data.Bifunctor             (first)
import           Data.ByteString            (ByteString (..))
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy (..))
import qualified Data.Text                  as T
import           Data.Text.Format           (Only (..))
import           Data.Time
import           Database.SQLite3           (Database, SQLData (..), Statement,
                                             StepResult (..), bind, close,
                                             columns, exec, finalize,
                                             lastInsertRowId, open, prepare,
                                             reset, step)
import           GHC.TypeLits               (KnownSymbol)

import           DbSchema.Db
import           DbSchema.Def

data Sqlite

type instance DbTypeName Sqlite Int64      = "INTEGER"
type instance DbTypeName Sqlite Int        = "INTEGER"
type instance DbTypeName Sqlite Bool       = "INTEGER"
type instance DbTypeName Sqlite Text       = "TEXT"
type instance DbTypeName Sqlite Double     = "FLOAT"
type instance DbTypeName Sqlite ByteString = "BLOB"
type instance DbTypeName Sqlite Day = "TEXT"
type instance DbTypeName Sqlite (Fixed a) = "INTEGER"
type instance DbTypeName Sqlite UTCTime = "TEXT"

-- type instance DbTypeName Sqlite (DBEnum a) = "TEXT"

-- instance Convert [SQLData] (Int64,[SQLData]) where
--   convert (SQLInteger x : xs) = (x,xs)
--   convert x = error $ "Error on convert " ++ show x ++ " to Int64"
--
-- instance Convert Int64 [SQLData]  where
--   convert = (:[]) . SQLInteger
--
-- instance Convert [SQLData] (Double,[SQLData]) where
--   convert (SQLFloat x : xs) = (x,xs)
--   convert x = error $ "Error on convert " ++ show x ++ " to Double"
--
-- instance Convert Double [SQLData]  where
--   convert = (:[]) . SQLFloat
--
-- instance Convert [SQLData] (Text,[SQLData]) where
--   convert (SQLText x : xs) = (x,xs)
--   convert x = error $ "Error on convert " ++ show x ++ " to Text"
--
-- instance Convert Text [SQLData]  where
--   convert = (:[]) . SQLText
--
-- instance Convert [SQLData] (ByteString,[SQLData]) where
--   convert (SQLBlob x : xs) = (x,xs)
--   convert x = error $ "Error on convert " ++ show x ++ " to ByteString"
--
-- instance Convert ByteString [SQLData]  where
--   convert = (:[]) . SQLBlob
--
-- instance Convert (Maybe Int64) [SQLData] where
--   convert = maybe [SQLNull] convert
-- instance Convert (Maybe Double) [SQLData] where
--   convert = maybe [SQLNull] convert
-- instance Convert (Maybe Text) [SQLData] where
--   convert = maybe [SQLNull] convert
-- instance Convert (Maybe ByteString) [SQLData] where
--   convert = maybe [SQLNull] convert
--
-- instance Convert [SQLData] (Maybe Int64,[SQLData]) where
--   convert = convToMaybe SQLNull
-- instance Convert [SQLData] (Maybe Double,[SQLData]) where
--   convert = convToMaybe SQLNull
-- instance Convert [SQLData] (Maybe Text,[SQLData]) where
--   convert = convToMaybe SQLNull
-- instance Convert [SQLData] (Maybe ByteString,[SQLData]) where
--   convert = convToMaybe SQLNull
--
-- instance SConvNames AllFld s Int64
-- instance SConvNames AllFld s Text
-- instance SConvNames AllFld s Double
-- instance SConvNames AllFld s ByteString
-- type instance GPlus Int64 = False
-- type instance GPlus Text = False
-- type instance GPlus Double = False
-- type instance GPlus ByteString = False
-- type instance GPlus (Maybe a) = False

instance Db Sqlite where
    type SessionParams Sqlite = Text
    type Conn Sqlite          = Database
    type FieldDB Sqlite       = SQLData
    type PrepCmd Sqlite       = Statement
    type GenKey Sqlite        = Int64

    paramName                 = format "?{}" . Only . (+1)
    createTableText t fs pk
      = format "CREATE TABLE IF NOT EXISTS {} ({}, PRIMARY KEY ({}) {})"
      . createTableParams @Sqlite t fs pk
    createRelText _ _ _ = mempty

    deleteConstraintText _    = ""
    runSession par sm         = do
      liftIO $ print @String "Make Sqlite Connection!"
      conn <- liftIO $ open par
      -- liftIO $ catch (exec conn "PRAGMA foreign_keys = ON;")
      --             (\(_::SomeException) -> return ())
      catch (runReaderT sm conn
                <* liftIO (close conn >> print @String "closed!!!"))
            (\(e::SomeException) ->
                liftIO (close conn >> print @String "closed!!!") >> throwM e)
    prepareCommand cmd = do
      liftIO $ print $ "prepareCommand: " <> cmd
      ask >>= \conn -> liftIO (prepare conn cmd)
    preRunInAuto = return ()
    runPrepared stat ps = liftIO $ do
      putStrLn "runPrepared"
      print ps
      reset stat
      bind stat ps
      _ <- step stat
      return ()
    finalizePrepared = liftIO . finalize
    runSelect p ps = liftIO $ do
      putStrLn "runSelect"
      print ps
      reset p
      bind p ps
      loop id
     where
      loop frs = do
        res <- step p
        if res == Done
          then return (frs [])
          else fmap (\r -> frs . (r:)) (columns p) >>= loop

    getLastKey = ask >>= \conn -> liftIO (lastInsertRowId conn)
    execCommand cmd = do
      liftIO $ print $ "execCommand: " <> cmd
      ask >>= \conn -> liftIO (exec conn cmd)

instance KnownSymbol name => CFldDef Sqlite name Int64 where
  fldToDb   = defToDb @_ @Sqlite SQLInteger
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name)
                                   (\case {SQLInteger v -> Just v;_ -> Nothing})
--
instance KnownSymbol name => CFldDef Sqlite name T.Text where
  fldToDb   = defToDb @_ @Sqlite  SQLText
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name)
                                   (\case {SQLText v -> Just v;_ -> Nothing})
--
instance KnownSymbol name => CFldDef Sqlite name Double where
  fldToDb   = defToDb @_ @Sqlite SQLFloat
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name)
                                   (\case {SQLFloat v -> Just v;_ -> Nothing})
--
instance KnownSymbol name => CFldDef Sqlite name ByteString where
  fldToDb   = defToDb @_ @Sqlite SQLBlob
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name)
                                   (\case {SQLBlob v -> Just v;_ -> Nothing})
--
instance CFldDef Sqlite name val
        => CFldDef Sqlite name (Maybe val) where
  fldToDb   = defMbToDb (Proxy @Sqlite) (Proxy @name) SQLNull
  fldFromDb = defMbFromDb (Proxy @Sqlite) (Proxy @name)
                          (\case {SQLNull -> True; _ -> False})
--
instance KnownSymbol name => CFldDef Sqlite name Int where
  fldToDb   = fldToDb @Sqlite @name @Int64 . fromIntegral
  fldFromDb = fmap (first fromIntegral) . fldFromDb @Sqlite @name @Int64
--
instance KnownSymbol name => CFldDef Sqlite name (Fixed n) where
  fldToDb   = fldToDb @Sqlite @name @Int64 . fromIntegral . (\(MkFixed v) -> v)
  fldFromDb = fmap (first (MkFixed . fromIntegral))
            . fldFromDb @Sqlite @name @Int64

instance KnownSymbol name => CFldDef Sqlite name Bool where
  fldToDb   = fldToDb @Sqlite @name @Int64 . (\b -> if b then 1 else 0)
  fldFromDb = fmap (first (==1)) . fldFromDb @Sqlite @name @Int64

instance KnownSymbol name => CFldDef Sqlite name Day where
  fldToDb   = fldToDb @Sqlite @name @T.Text
                    . T.pack . formatTime defaultTimeLocale "%F"
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name) (\case
          SQLText v -> parseTimeM True defaultTimeLocale "%F" $ T.unpack v
          _         -> Nothing
        )
-- UTCTime
instance KnownSymbol name => CFldDef Sqlite name UTCTime where
  fldToDb   = fldToDb @Sqlite @name @T.Text
                    . T.pack . formatTime defaultTimeLocale "%FT%X"
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name) (\case
          SQLText v -> parseTimeM True defaultTimeLocale "%FT%X" $ T.unpack v
          _         -> Nothing
        )
