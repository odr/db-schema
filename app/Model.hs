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
import Data.Time as Model
import GHC.Generics (Generic)
import Lens.Micro ((^.))

import DbSchema.Db as Model
import DbSchema.Db.Sqlite as Model (Sqlite)
import DbSchema.DDL as Model
import DbSchema.Def as Model
import DbSchema.TH.MkSchema (mkSchema)
import           Dbs
import           TabData

data Sch

mkSchema ''Dbs ''Sch [t|
  '[TPD Customer '["id"] '[ '["name"]] True '[]

  , TPD Address '["id"] '[] True '[]
    --   '[ RT "addrCust" "Customer" '[ '("customerId","id")] DcCascade
    --   ]

  , TPD CustomerAddress '["customerId", "addressId"] '[] False
    '[ RT "caCustomer" "Customer" '[ '("customerId", "id")] DcCascade
    ,  RT "caAddress" "Address" '[ '("addressId", "id")] DcCascade
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
