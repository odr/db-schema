{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module DbSchema.DML where
import           Data.Kind    (Type)
import qualified Data.Text    as T
import           GHC.TypeLits

import           DbSchema.Db
import           DbSchema.Def
