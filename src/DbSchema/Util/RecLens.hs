{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}
module DbSchema.Util.RecLens where

import           Data.Kind                     (Type)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Maybe (IsJust)
import           Data.Tagged                   (Tagged (..), untag)
import           Data.Type.Equality
import           GHC.TypeLits                  (Symbol)
import           Lens.Micro                    (lens, (.~), (^.))

-- Simple lens by (fldName :: Symbol)
--  (a -> f a) -> b -> f b
-- (a -> f a) -> Maybe a -> f (Maybe a)
-- g afa = \case
    -- Nothing -> const Nothing <$> afa undefined
    -- Just a -> Just <$> afa a
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

instance RecLensB 'True s (Tagged (s ': ss) (v1,v2)) where
  type TLensB 'True s (Tagged (s ': ss) (v1,v2)) = v1
  recLensB f (Tagged (v1,v2))= (Tagged @(s ': ss) . (,v2)) <$> f v1
--
instance RecLens s (Tagged ss v2)
      => RecLensB 'False s (Tagged (s1 ': ss) (v1,v2)) where
  type TLensB 'False s (Tagged (s1 ': ss) (v1,v2)) = TLens s (Tagged ss v2)
  recLensB f (Tagged (v1,v2)) = (Tagged @(s1 ': ss) . (v1,) . untag)
                            <$> recLens @s @(Tagged ss v2) f (Tagged @ss v2)

class Rec r where
  type TRec r :: [(Symbol,Type)]

instance Rec ()   where
  type TRec () = '[]

instance Rec (Tagged ('[] :: [Symbol]) ())   where
  type TRec (Tagged ('[] :: [Symbol]) ()) = '[]

instance Rec (Tagged '[(n::Symbol)] v)   where
  type TRec (Tagged '[n] v) = '[ '(n,v)]

instance Rec (Tagged (n2 ': ns) v2)
      => Rec (Tagged (n1 ': n2 ': ns :: [Symbol]) (v1,v2))   where
  type TRec (Tagged (n1 ': n2 ': ns) (v1,v2)) =
    '(n1,v1) ': TRec (Tagged (n2 ': ns) v2)

instance (Rec v1, IsJust (Lookup s (TRec v1)) ~ b , RecLensB b s (v1,v2))
      => RecLens s (v1, v2) where
  type TLens s (v1,v2) = TLensB (IsJust (Lookup s (TRec v1))) s (v1,v2)
  recLens = recLensB @b @s @(v1,v2)
--
instance RecLens s v1 => RecLensB 'True s (v1,v2) where
  type TLensB 'True s (v1,v2) = TLens s v1
  recLensB f (v1,v2) = (,v2) <$> (recLens @s @v1) f v1
--
instance RecLens s v2 => RecLensB 'False s (v1,v2) where
  type TLensB 'False s (v1,v2) = TLens s v2
  recLensB f (v1,v2) = (v1,) <$> (recLens @s @v2) f v2

type family Untag a where
  Untag (Tagged x y) = y

class SubRec (ns :: [Symbol]) r where
  type TSubRec ns r
  getSub :: r -> TSubRec ns r
  setSub :: TSubRec ns r -> r -> r
  subLens :: Functor f => (TSubRec ns r -> f (TSubRec ns r)) -> r -> f r
  subLens = lens (getSub @ns) (flip (setSub @ns))
          -- or use alongside?
instance SubRec '[] r where
  type TSubRec '[] r = ()
  getSub _ = ()
  setSub _ = id

instance (RecLens n1 r, SubRec (n2 ': ns) r)
      => SubRec (n1 ': n2 ': ns) r where
  type TSubRec (n1 ': n2 ': ns) r = (TLens n1 r, TSubRec (n2 ': ns) r)
  getSub = (,) <$> (^. recLens @n1) <*> getSub @(n2 ': ns)
  setSub (a,b) = setSub @(n2 ': ns) b . (recLens @n1 .~ a)
  -- subLens = alongside (recLens @n1) (subLens @(n2 ': ns))

instance RecLens n1 r => SubRec '[n1] r where
  type TSubRec '[n1] r = TLens n1 r
  getSub = (^. recLens @n1)
  setSub = (recLens @n1 .~)

--
class MbMaybeB b x y where mbMaybeB :: x -> y
instance MbMaybeB 'True x x where mbMaybeB = id
instance MbMaybeB 'False x (Maybe x) where mbMaybeB = Just

class MbMaybe (ns :: [Symbol]) x y where mbMaybe :: x -> y
instance MbMaybe '[] () () where mbMaybe = id
instance MbMaybeB (a==b) a b => MbMaybe '[n] a b where
  mbMaybe = mbMaybeB @(a==b)
instance (MbMaybeB (a1==b1) a1 b1, MbMaybe (n2 ':ns) as bs)
      => MbMaybe (n1 ':n2 ':ns) (a1,as) (b1,bs) where
  mbMaybe (a1,as) = (mbMaybeB @(a1==b1) a1, mbMaybe @(n2 ':ns) as)

