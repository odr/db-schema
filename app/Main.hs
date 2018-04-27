{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Model

main :: IO ()
main = runSession @Dbs "test.db" $ do
  createSchema @Dbs @Sch
