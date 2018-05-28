{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module DbSchema.Util.ToStar where
import           Data.Kind       (Type)
import           Data.Proxy      (Proxy (..))
import qualified Data.Text       as T
import           GHC.TypeLits    (KnownSymbol, Symbol, symbolVal)
import           Type.Reflection (Typeable, typeRep)

type family TStar k :: Type

type instance TStar Symbol = T.Text
type instance TStar Bool  = Bool
type instance TStar [k] = [TStar k]
type instance TStar (k1,k2) = (TStar k1, TStar k2)

class ToStar (a::k) where
  toStar :: TStar k

instance KnownSymbol s => ToStar (s::Symbol) where
  toStar = T.pack $ symbolVal $ Proxy @s

instance Typeable s => ToStar (s::Bool) where
  toStar = read $ tail $ show $ typeRep @s

instance ToStar '[] where
  toStar = []

instance (ToStar x, ToStar xs)
      => ToStar (x ': xs) where
  toStar = toStar @_ @x : toStar @_ @xs

instance (ToStar x, ToStar y) => ToStar '(x,y) where
  toStar = (toStar @_ @x, toStar @_ @y)
