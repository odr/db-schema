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
module DbSchema.Db.Sqlite
    ( Sqlite, SQLData
    )
    where

import           Control.Monad.Catch        (SomeException, catch, throwM)
import           Control.Monad.Trans.Reader (ReaderT (..), ask)
import           Data.Bifunctor             (second)
import           Data.ByteString            (ByteString)
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

import           DbSchema.Db
import           DbSchema.Def
import           DbSchema.Util.ToStar

data Sqlite

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

instance ToStar name => CFldDef Sqlite name Int64 where
  fldDbDef = [(toStar @_ @name, ("INTEGER", False))]
  fldToDb   = defToDb @_ @Sqlite SQLInteger
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name)
                                   (\case {SQLInteger v -> Just v;_ -> Nothing})
--
instance ToStar name => CFldDef Sqlite name T.Text where
  fldDbDef = [(toStar @_ @name, ("TEXT", False))]
  fldToDb   = defToDb @_ @Sqlite  SQLText
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name)
                                   (\case {SQLText v -> Just v;_ -> Nothing})
--
instance ToStar name => CFldDef Sqlite name Double where
  fldDbDef = [(toStar @_ @name, ("FLOAT", False))]
  fldToDb   = defToDb @_ @Sqlite SQLFloat
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name)
                                   (\case {SQLFloat v -> Just v;_ -> Nothing})
--
instance ToStar name => CFldDef Sqlite name ByteString where
  fldDbDef = [(toStar @_ @name, ("BLOB", False))]
  fldToDb   = defToDb @_ @Sqlite SQLBlob
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name)
                                   (\case {SQLBlob v -> Just v;_ -> Nothing})
--
instance CFldDef Sqlite name val => CFldDef Sqlite name (Maybe val) where
  fldDbDef = map (second (second $ const True)) (fldDbDef @Sqlite @name @val)
  fldToDb   = defMbToDb (Proxy @Sqlite) (Proxy @name) SQLNull
  fldFromDb = defMbFromDb (Proxy @Sqlite) (Proxy @name)
                          (\case {SQLNull -> True; _ -> False})
--
instance ToStar name => CFldDef Sqlite name Int where
  fldDbDef = [(toStar @_ @name, ("INTEGER", False))]
  fldToDb   = fldToDb @Sqlite @name @Int64 . fromIntegral
  fldFromDb = fmap fromIntegral $ fldFromDb @Sqlite @name @Int64
--
instance ToStar name => CFldDef Sqlite name (Fixed n) where
  fldDbDef = [(toStar @_ @name, ("INTEGER", False))]
  fldToDb   = fldToDb @Sqlite @name @Int64 . fromIntegral . (\(MkFixed v) -> v)
  fldFromDb = fmap (MkFixed . fromIntegral) $ fldFromDb @Sqlite @name @Int64

instance ToStar name => CFldDef Sqlite name Bool where
  fldDbDef = [(toStar @_ @name, ("INTEGER", False))]
  fldToDb   = fldToDb @Sqlite @name @Int64 . (\b -> if b then 1 else 0)
  fldFromDb = fmap (==1) $ fldFromDb @Sqlite @name @Int64

instance ToStar name => CFldDef Sqlite name Day where
  fldDbDef = [(toStar @_ @name, ("TEXT", False))]
  fldToDb   = fldToDb @Sqlite @name @T.Text
                    . T.pack . formatTime defaultTimeLocale "%F"
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name) (\case
          SQLText v -> parseTimeM True defaultTimeLocale "%F" $ T.unpack v
          _         -> Nothing
        )
-- UTCTime
instance ToStar name => CFldDef Sqlite name UTCTime where
  fldDbDef = [(toStar @_ @name, ("TEXT", False))]
  fldToDb   = fldToDb @Sqlite @name @T.Text
                    . T.pack . formatTime defaultTimeLocale "%FT%X"
  fldFromDb = defFromDb @_ @Sqlite (Proxy @name) (\case
          SQLText v -> parseTimeM True defaultTimeLocale "%FT%X" $ T.unpack v
          _         -> Nothing
        )
