{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}
module DbSchema.Condition where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.Bifunctor
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           GHC.OverloadedLabels       (IsLabel (..))
import           GHC.TypeLits               (Symbol)

import           DbSchema.Db
import           DbSchema.Def


data CmpSimple = (:==) | (:<=) | (:>=) | (:>) | (:<) deriving (Show, Eq, Generic)

data Cmp = CmpS CmpSimple | Like deriving (Show, Eq, Generic)

instance FromJSON CmpSimple
instance ToJSON CmpSimple

instance FromJSON Cmp
instance ToJSON Cmp

data BoolOp = And | Or deriving (Show, Eq, Generic)
instance FromJSON BoolOp
instance ToJSON BoolOp

showCmp :: CmpSimple -> Text
showCmp = \case
  (:==) -> "="
  (:<=) -> "<="
  (:>=) -> ">="
  (:<)  -> "<"
  (:>)  -> ">"

data Cond db sch (t::Symbol)
  = Empty
  | forall (n::Symbol). (TabFld sch t n, CFldDef db n (TabFldType sch t n))
    => Cmp  (Proxy n) Cmp (TabFldType sch t n)
  | forall (n::Symbol). (TabFld sch t n, CFldDef db n (TabFldType sch t n))
    => Null (Proxy n)
  | Not (Cond db sch t)
  | BoolOp BoolOp (Cond db sch t) (Cond db sch t)
  -- condition "EXIST"
  | forall (ref::Symbol) rel . (rel ~ TRelDef sch ref, t ~ RdTo rel)
    => Child (Proxy ref) (Cond db sch (RdFrom rel))
--
deriving instance Show (Cond db sch t)

--
class CCond (n::Symbol) db sch (t::Symbol) where
  pcmp    :: (TabFld sch t n, CFldDef db n (TabFldType sch t n))
          => Cmp -> (TabFldType sch t n) -> Cond db sch t
  pnull   :: (TabFld sch t n, CFldDef db n (TabFldType sch t n)) => Cond db sch t
  pchild  :: forall rel . (rel ~ TRelDef sch n, t ~ RdTo rel)
          => Cond db sch (RdFrom rel) -> Cond db sch t

instance CCond n db sch t where
  pcmp = Cmp (Proxy @n)
  pnull = Null (Proxy @n)
  pchild = Child (Proxy @n)

--
instance (TabFld sch t n, v ~ TabFldType sch t n, CFldDef db n v)
      => IsLabel n (Cmp -> v -> Cond db sch t) where
  fromLabel = Cmp (Proxy @n)

instance (rel ~ TRelDef sch n, p ~ RdTo rel, c ~ RdFrom rel)
      => IsLabel n (Cond db sch c -> Cond db sch p) where
  fromLabel = Child (Proxy @n)

pnot :: Cond db b a -> Cond db b a
pnot = Not

(&&&), (|||) :: Cond db b a -> Cond db b a -> Cond db b a
(&&&) = BoolOp And
(|||) = BoolOp Or
infixl 2 |||
infixl 3 &&&

(<?),(>?),(<=?),(>=?),(==?),(~?)
  :: (Cmp -> v -> Cond db sch t) -> v -> Cond db sch t
x <? b  = x (CmpS (:<))  b
x >? b  = x (CmpS (:>))  b
x <=? b = x (CmpS (:<=)) b
x >=? b = x (CmpS (:>=)) b
x ==? b = x (CmpS (:==)) b
x ~? b  = x Like  b
infix 4 <?, >?, <=?, >=?, ==?, ~?
--
-- pcmp @"id" @Dbs @Sch @"Customer"  ==? 1
-- ||| pchild @"ordCust" (
--    #id ==? 1
--    ||| pnot (pchild @"opOrd" (#price >? 5))
-- )

-- номер таблицы родителя (номер дочерней таблицы, номер параметра)
type CondMonad = ReaderT Int (State (Int, Int))
runCond :: CondMonad a -> a
runCond x = evalState (runReaderT x 0) (0,0)

withPar :: Db b => Proxy b -> (Text -> a) -> CondMonad a
withPar (_::Proxy b) f = lift $ do
  n <- snd <$> get
  modify $ second (+1)
  return $ f (paramName @b n)

convCond :: Cond db sch t -> CondMonad (Text, [FieldDB b])
convCond (condition :: Cond db sch t) = case condition of
  Empty -> return ("1=1",[])
  -- (Cmp (_::Proxy n) cmp v -> do
  --   ntab <- lift $ fst <$> get
  --   withPar pb $ \par
  --     -> let nm = format "t{}.{}" (ntab, symbolVal (Proxy @s)) in
  --          (case cmp of
  --            Like     -> condLike @b nm par
  --            CmpS op  -> formatS "{}{}{}" (nm,showCmp op,par)
  --          , fldToDb @b @n v)
  -- | forall (n::Symbol). TabFld sch t n => Null (Proxy n)
--   Null p -> return (pack (symbolVal p) `mappend` " IS NULL", [])
--   -- | Not (Cond sch t)
--   Not c -> first (formatS "NOT ({})" . Only) <$> convCond c
--   -- | BoolOp BoolOp (Cond sch t) (Cond sch t)
--   BoolOp bo c1 c2 ->
--     (\(cc1,d1) (cc2,d2) -> (formatS "({}) {} ({})" (cc1,show bo,cc2),d1++d2))
--       <$> convCond c1 <*> convCond c2
--   -- | forall (ref::Symbol) rel . (rel ~ TRelDef sch ref, t ~ RdTo rel)
--   -- => Child (Proxy ref) (Cond sch (RdFrom rel))
--   Child (Tagged cond :: Tagged r (Cond '(b,sch) TRec)) -> do
--     pnum <- ask
--     cnum <- lift $ do
--       modify (first (+1))
--       fst <$> get
--     case sGetRef (sing @_ @r) (sing @_ @sch) of
--       SJust rf -> fmap (first (\c ->
--             formatS "EXISTS (SELECT 1 FROM {} t{} WHERE {}{}{})"
--               ( fromSing (sGetRefFrom rf)
--               , cnum
--               , T.intercalate " AND "
--                   $ map (\(ch,pr) -> formatS "t{}.{} = t{}.{}" (cnum,ch,pnum,pr))
--                   $ fromSing (sGetRefCols rf)
--               , if T.null c then (""::T.Text) else " AND "
--               , c)
--           ))
--         $ local (const cnum)
--         $ convCond cond
