{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module MasterData where

import           DbSchema.Def
import           DbSchema.TH.MkSchema
import           Lens.Micro           ((^.))

import           Dbs
import           TabData

data MasterData

mkSchema ''Dbs ''MasterData [t|
    '[TPD Customer '["id"] '[ '["name"]] True '[]
    , TPD Address '["id"] '[] True '[]
    , TPD Article '["id"] '[ '["name"]] False '[]
    , TPD Currency '["code"] '[ '["name"]] False '[]
    ] |]
