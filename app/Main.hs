{-# LANGUAGE DataKinds           #-}
-- {-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Applicative   (ZipList (..))
import           Data.Proxy            (Proxy (..))
import           Data.Tagged           (Tagged (..))
import           DbSchema.Condition
import           DbSchema.DML
import           GHC.TypeLits          (Symbol)

import           DbSchema.Util.RecLens

import           CustomerView
import           Model

customers d =
  [ CustomerT 0 "Lena" [ AddressT 0 "SPb" True ] "Test" [] []
  , CustomerT 0 "Dima" [ AddressT 0 "SPb" True
                      , AddressT 0 "Msk" False
                      , AddressT 0 "ירושלים" False
                      ] "Some text"
    [ OrderT 0 "1" 0 Nothing (utctDay d) [] []
    , OrderT 0 "2" 0 Nothing (utctDay d) [] []
    ] []
  ]

parEmpty = Tagged @('[] :: [Symbol]) ()

test :: IO ([CustomerT], [CustomerT])
test = do
  d <- getCurrentTime
  runSession @Dbs "test.db" $ do
    dropSchema @Dbs @Sch
    createSchema @Dbs @Sch
    rs <- dmlInsert @Dbs @Sch @"Customer" $ ZipList [(parEmpty, customers d)]
    (rs' :: ZipList [CustomerT]) <- dmlSelect @Dbs @Sch @"Customer"
        $ ZipList $ map (Tagged @'["id"]) [1..3::Int64]
    return (concat $ getZipList rs, concat $ getZipList rs')


main :: IO ()
main = do
  (rs,rs') <- test
  print rs
  print rs'
  print $ rs == rs'

