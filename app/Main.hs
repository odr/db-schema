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
  dropSchema @Dbs @Sch
  createSchema @Dbs @Sch
  d <- liftIO $ getCurrentTime
  rs <- dmlInsert @Dbs @Sch @"Customer"
    [( Tagged @('[]::[Symbol]) ()
    ,[ CustomerT 0 "Lena" [ AddressT 0 "SPb" True ] "Test" [] []
     , CustomerT 0 "Dima" [ AddressT 0 "SPb" True
                          , AddressT 0 "Msk" False
                          , AddressT 0 "ירושלים" False
                          ] "Some text"
        [ OrderT 0 "1" 0 Nothing (utctDay d) [] []
        , OrderT 0 "2" 0 Nothing (utctDay d) [] []
        ] []
     ]

    )]
  liftIO $ print rs
