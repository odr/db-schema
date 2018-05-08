{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}
module DbSchema.Util.RecLens where

import           Data.Bifunctor     (first)
import           Data.Tagged        (Tagged (..), untag)
import           Data.Type.Equality
import           GHC.TypeLits       (Symbol)
import           Lens.Micro         ((&), (.~), (^.))

-- Simple lens by (fldName :: Symbol)
class RecLens (s :: Symbol) b where
  type TLens s b
  recLens :: Functor f => (TLens s b -> f (TLens s b)) -> b -> f b

instance RecLens s (Tagged '[s] v) where
  type TLens s (Tagged '[s] v) = v
  recLens f = fmap Tagged . f . untag
--
instance RecLensB (s == s1) s (Tagged (s1 ': s2 ': ss) (v1,v2))
      => RecLens s (Tagged (s1 ': s2 ': ss) (v1,v2)) where
  type TLens s (Tagged (s1 ': s2 ': ss) (v1,v2)) =
    TLensB (s==s1) s (Tagged (s1 ': s2 ': ss) (v1,v2))
  recLens = recLensB @(s == s1) @s @(Tagged (s1 ': s2 ': ss) (v1,v2))
--
class RecLensB (b::Bool) (s::Symbol) v where
  type TLensB  b s v
  recLensB :: Functor f => (TLensB b s v -> f (TLensB b s v)) -> v -> f v

instance RecLensB True s (Tagged (s ': ss) (v1,v2)) where
  type TLensB True s (Tagged (s ': ss) (v1,v2)) = v1
  recLensB f (Tagged (v1,v2))= (Tagged @(s ': ss) . (,v2)) <$> f v1
--
instance RecLens s (Tagged ss v2)
      => RecLensB False s (Tagged (s1 ': ss) (v1,v2)) where
  type TLensB False s (Tagged (s1 ': ss) (v1,v2)) = TLens s (Tagged ss v2)
  recLensB f (Tagged (v1,v2)) = (Tagged @(s1 ': ss) . (v1,) . untag)
                            <$> recLens @s @(Tagged ss v2) f (Tagged @ss v2)
--
-- class Rec r where
--   type TRec r :: [(Symbol,Type)]
--
-- instance RecLens s (v1, v2) where
--   type TLens s (v1,v2) = TLensB (IsJust (Lookup s (TRec v1))) (v1,v2)
--   recLens = recLensB
--
type family Untag a where
  Untag (Tagged a b) = b

class SubRec (ns :: [Symbol]) r where
  type TSubRec ns r
  getSub :: r -> TSubRec ns r

instance SubRec '[] r where
  type TSubRec '[] r = ()
  getSub _ = ()

instance (RecLens n1 r, SubRec (n2 ': ns) r)
      => SubRec (n1 ': n2 ': ns) r where
  type TSubRec (n1 ': n2 ': ns) r = (TLens n1 r, TSubRec (n2 ': ns) r)
  getSub r = (r ^. (recLens @n1), getSub @(n2 ': ns) r)

instance RecLens n1 r => SubRec '[n1] r where
  type TSubRec '[n1] r = TLens n1 r
  getSub r = (r ^. (recLens @n1))
