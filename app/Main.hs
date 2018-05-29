{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Applicative (ZipList (..))
import           Data.Tagged         (Tagged (..))
import           DbSchema.DML

import           CustomerView
import           Dbs
-- import           MasterData
import           Model

customers :: UTCTime -> [CustomerT]
customers d =
  [ CustomerT 0 "Lena" [ ] "Test"
    [ OrderT 0 "3" 0 Nothing (utctDay d) [] [] ] []
  , CustomerT 0 "Dima" [ ] "Some text"
    [ OrderT 0 "1" 0 Nothing (utctDay d) [] []
    , OrderT 0 "2" 0 Nothing (utctDay d) [] []
    ] []
  ]

customers2 :: UTCTime -> [CustomerT]
customers2 d =
  [ CustomerT 2 "Dima" [] "Some text2"
    [ OrderT 2 "1" 2 Nothing (addDays 1 $ utctDay d) [] []
    , OrderT 0 "4" 0 Nothing (addDays 1 $ utctDay d) [] []
    ] []
  , CustomerT 0 "Лена" [ ] "Test"
    [ OrderT 1 "3" 0 Nothing (addDays 1 $ utctDay d) [] [] ] []
  ]

test :: UTCTime -> SessionMonad Dbs IO ([CustomerT], [CustomerT])
test d = do
  dropSchema @Dbs @Sch
  createSchema @Dbs @Sch
  rs <- dmlInsert @Dbs @Sch @"Customer" $ ZipList [((), customers d)]
  (rs' :: ZipList [CustomerT]) <- dmlSelect @Dbs @Sch @"Customer"
      $ ZipList $ map ((,()) . (Tagged @'["id"])) [1..3::Int64]
  return (concat $ getZipList rs, concat $ getZipList rs')
--
test2 :: UTCTime -> [CustomerT] -> SessionMonad Dbs IO ([CustomerT], [CustomerT])
test2 d cs = do
  rs <- dmlUpdate @Dbs @Sch @"Customer" False $ ZipList [((), cs, customers2 d)]
  (rs' :: ZipList [CustomerT]) <- dmlSelect @Dbs @Sch @"Customer"
      $ ZipList $ map ((,()) . (Tagged @'["id"])) [1..3::Int64]
  return (concat $ getZipList rs, concat $ getZipList rs')


main :: IO ()
main = do
  d <- getCurrentTime
  (rs,rs1) <- runSession @Dbs "test.db" $ test d
  putStrLn "----- rs -------"
  print rs
  putStrLn "----- rs1 -------"
  print rs1
  putStrLn "------------"
  print $ rs == rs1
  (rs2,rs3) <- runSession @Dbs "test.db" $ test2 d rs1
  putStrLn "------- rs2 -----"
  print rs2
  putStrLn "------- rs3 -----"
  print rs3
  putStrLn "------------"
  print $ rs2 == rs3

