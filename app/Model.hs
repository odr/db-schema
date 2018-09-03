{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Model(module Model) where

import Data.Fixed as Model
import Data.Generics.Product (field)
import Data.Int as Model (Int64)
-- import           Data.Text
import Data.Time as Model
import GHC.Generics (Generic)
import Lens.Micro ((^.))

import DbSchema.Db as Model
import DbSchema.Db.Sqlite as Model (Sqlite)
import DbSchema.DDL as Model
import DbSchema.Def as Model
import DbSchema.TH.MkSchema (mkSchema)

type Dbs = Sqlite

data Customer = Customer  { id   :: Int64
                          , name :: Text
                          , note :: Text
                          } deriving (Show,Eq,Ord,Generic)

data Address = Address  { id         :: Int64
                        , customerId :: Int64
                        , val        :: Text
                        , isActive   :: Bool
                        } deriving (Show,Eq,Ord,Generic)

data Article = Article  { id    :: Int64
                        , name  :: Text
                        , price :: Fixed E2
                        } deriving (Show,Eq,Ord,Generic)

data ArticlePrice = ArticlePrice { articleId :: Int64
                                 , dayBegin  :: Day
                                 , dayEnd    :: Maybe Day
                                 , val       :: Fixed E2
                                 } deriving (Show,Eq,Ord,Generic)

data Orders = Orders  { id         :: Int64
                      , num        :: Text
                      , customerId :: Int64
                      , payerId    :: Maybe Int64
                      , day        :: Day
                      } deriving (Show,Eq,Ord,Generic)

data OrderPosition = OrderPosition  { orderId      :: Int64
                                    , num          :: Int
                                    , articleId    :: Int64
                                    , quantity     :: Int
                                    , price        :: Fixed E2
                                    , currencyCode :: Text
                                    } deriving (Show,Eq,Ord,Generic)

data Payment = Payment  { id           :: Int64
                        , orderId      :: Int64
                        , dt           :: UTCTime
                        , val          :: Fixed E2
                        , currencyCode :: Text
                        , note         :: Text
                        } deriving (Show,Eq,Ord,Generic)

data Currency = Currency { code :: Text
                         , name :: Text
                         } deriving (Show,Eq,Ord,Generic)

data CurrRate = CurrRate { currencyCode :: Text
                         , day          :: Day
                         , coeff        :: Fixed E6
                         } deriving (Show,Eq,Ord,Generic)

data Sch

mkSchema ''Dbs ''Sch [t|
  '[TPD Customer '["id"] '[ '["name"]] True '[]

  , TPD Address '["id"] '[] True
      '[ RT "addrCust" "Customer" '[ '("customerId","id")] DcCascade
      ]

  , TPD Article '["id"] '[ '["name"]] False '[]

  , TPD ArticlePrice '["articleId", "dayBegin"] '[] False
      '[ RT "artPrice" "Article" '[ '("articleId","id")] DcCascade
      ]

  , TPD Orders '["id"] '[ '["num"]] True
      '[RT "ordCust" "Customer" '[ '("customerId","id")] DcCascade
      , RT "ordPayer" "Customer" '[ '("payerId","id")] DcRestrict
      ]

  , TPD OrderPosition '["orderId","num"] '[] False
      '[RT "opOrd" "Orders" '[ '("orderId","id")] DcCascade
      , RT "opCurr" "Currency" '[ '("currencyCode", "code")] DcRestrict
      , RT "opArt" "Article" '[ '("articleId", "id")] DcRestrict
      ]

  , TPD Payment '["id"] '[] True
      '[RT "paymOrd"  "Orders"   '[ '("orderId","id")]    DcRestrict
      , RT "paymCurr" "Currency" '[ '("currencyCode", "code")] DcRestrict
      ]
  , TPD Currency '["code"] '[ '["name"]] False '[]

  , TPD CurrRate '["currencyCode","day"] '[] False
      '[RT "currRateCurr" "Currency" '[ '("currencyCode", "code")] DcCascade
      ]
  ]
  |]
