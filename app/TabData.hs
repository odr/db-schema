{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module TabData(module TabData) where

import           Data.Generics.Product (field)
import           Data.Int              as Model (Int64)
import           GHC.Generics          (Generic)

import           DbSchema.Db           as TabData
import           DbSchema.TH.MkView




data Customer = Customer  { id   :: Int64
                          , name :: Text
                          , note :: Text
                          } deriving (Show,Eq,Ord,Generic)

data CustomerAddress = CA { customerId :: Int64
                          , addressId  :: Int64
                          , isActive   :: Bool
                          } deriving (Show,Eq,Ord,Generic)

data Address = Address  { id  :: Int64
                        , val :: Text
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

data Company = Company  { id        :: Int64
                        , name      :: Text
                        , addressId :: Int64
                        } deriving (Show,Eq,Ord,Generic)

mkRecs [t| '[Customer, CustomerAddress, Address, Article, ArticlePrice, Orders
        , OrderPosition, Payment, Currency, CurrRate, Company] |]
