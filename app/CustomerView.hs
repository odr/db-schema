{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module CustomerView where

import Data.Generics.Product (field)
import GHC.Generics
import Lens.Micro ((^.))


import DbSchema.TH.MkView

import Model

{-
Требуются валидации.
У кастомера не больше одного активного адреса.
Ордера с одинаковым id из разных списков должны совпадать
(или отличаться от исходного разными полями (в частности, для нового - совпадать)?)

Альтернатива:
- в ordCust все ордера кастомера
- в ordPayer - только те, которых нет в ordCust

Альтернатива

-}
data CustomerT = CustomerT  { id       :: Int64
                            , name     :: Text
                            , addrCust :: [AddressT]
                            , note     :: Text
                            , ordCust  :: [OrderT]
                            , ordPayer :: [OrderT]
                            } deriving (Show,Eq,Ord,Generic)

data AddressT = AddressT  { id       :: Int64
                          , val      :: Text
                          , isActive :: Bool
                          } deriving (Show,Eq,Ord,Generic)

data OrderT = OrderT  { id         :: Int64
                      , num        :: Text
                      , customerId :: Int64
                      , payerId    :: Maybe Int64
                      , day        :: Day
                      , opOrd      :: [OrderPosT]
                      , paymOrd    :: [PaymentT]
                      } deriving (Show,Eq,Ord,Generic)

data OrderPosT = OrderPosT  { num          :: Int
                            , articleId    :: Int64
                            , quantity     :: Int
                            , price        :: Fixed E2
                            , currencyCode :: Text
                            } deriving (Show,Eq,Ord,Generic)

data PaymentT = PaymentT  { id           :: Int64
                          , dt           :: UTCTime
                          , val          :: Fixed E2
                          , currencyCode :: Text
                          , note         :: Text
                          } deriving (Show,Eq,Ord,Generic)


mkView ''Dbs ''Sch "Customer" (rels @Sch) ''CustomerT
