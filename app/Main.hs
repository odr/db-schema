{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Data.Proxy   (Proxy (..))
import           Data.Tagged  (Tagged (..))
import           DbSchema.DML
import           GHC.TypeLits (Symbol)

import           CustomerView
import           Model

main :: IO ()
main = runSession @Dbs "test.db" $ do
  createSchema @Dbs @Sch
  rs <- dmlInsert @Dbs @Sch @"Customer" (Tagged @('[]::[Symbol]) ())
    [ CustomerT 1 "Lena" [ AddressT 1 "SPb" True ] "Test" [] []
    ]
  liftIO $ print rs
