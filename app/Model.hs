{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Model(module Model) where

import           Data.Fixed            as Model
import           Data.Generics.Product
import           Data.Int              as Model (Int64)
import           Data.Text
import           Data.Time             as Model
import           GHC.Generics
import           Lens.Micro

import           DbSchema.Db           as Model
import           DbSchema.Db.Sqlite    as Model (Sqlite)
import           DbSchema.DDL          as Model
import           DbSchema.Def          as Model
import           DbSchema.TH           (mkSchema)

type Dbs = Sqlite

data Customer = Customer  { id   :: Int
                          , name :: Text
                          , note :: Text
                          } deriving (Show,Eq,Ord,Generic)

data Address = Address  { id         :: Int
                        , customerId :: Int
                        , val        :: Text
                        , isActive   :: Bool
                        } deriving (Show,Eq,Ord,Generic)

data Article = Article  { id    :: Int
                        , name  :: Text
                        , price :: Fixed E2
                        } deriving (Show,Eq,Ord,Generic)

data ArticlePrice = ArticlePrice { articleId :: Int
                                 , dayBegin  :: Day
                                 , dayEnd    :: Maybe Day
                                 , val       :: Fixed E2
                                 } deriving (Show,Eq,Ord,Generic)

data Orders = Orders  { id         :: Int
                      , num        :: Text
                      , customerId :: Int
                      , payerId    :: Maybe Int
                      , day        :: Day
                      } deriving (Show,Eq,Ord,Generic)

data OrderPosition = OrderPosition  { orderId      :: Int
                                    , num          :: Int
                                    , articleId    :: Int
                                    , quantity     :: Int
                                    , price        :: Fixed E2
                                    , currencyCode :: Text
                                    } deriving (Show,Eq,Ord,Generic)

data Payments = Payments  { id           :: Int
                          , orderId      :: Int
                          , customerId   :: Int
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

mkSchema ''Sqlite ''Sch [t|
  '[TPD Customer '["id"] '[ '["name"]] True '[]

  , TPD Address '["id"] '[] True
      '[ RT "AddrCust" "Customer" '[ '("customerId","id")] DcCascade
      ]

  , TPD Article '["id"] '[ '["name"]] False '[]

  , TPD ArticlePrice '["articleId", "dayBegin"] '[] False
      '[ RT "ArtPrice" "Article" '[ '("articleId","id")] DcCascade
      ]

  , TPD Orders '["id"] '[ '["num"]] True
      '[RT "OrdCust" "Customer" '[ '("customerId","id")] DcCascade
      , RT "OrdPayer" "Customer" '[ '("payerId","id")] DcRestrict
      ]

  , TPD OrderPosition '["orderId","num"] '[] True
      '[RT "OpOrd" "Orders" '[ '("orderId","id")] DcCascade
      , RT "OpCurr" "Currency" '[ '("currencyCode", "code")] DcRestrict
      ]

  , TPD Payments '["id"] '[ '["orderId", "customerId", "dt"]] True
      '[RT "PaymOrd"  "Orders"   '[ '("orderId","id")]    DcRestrict
      , RT "PaymCust" "Customer" '[ '("customerId","id")] DcCascade
      , RT "PaymCurr" "Currency" '[ '("currencyCode", "code")] DcRestrict
      ]
  , TPD Currency '["code"] '[ '["name"]] False '[]

  , TPD CurrRate '["currencyCode","day"] '[] False
      '[RT "CurrRateCurr" "Currency" '[ '("currencyCode", "code")] DcCascade
      ]
  ]
  |]
