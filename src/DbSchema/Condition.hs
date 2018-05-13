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
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}
module DbSchema.Condition where

import           Control.Monad              (zipWithM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.Bifunctor
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Tuple                 (swap)
import           GHC.Generics               (Generic)
import           GHC.OverloadedLabels       (IsLabel (..))
import           GHC.TypeLits               (Symbol)

import           DbSchema.Db
import           DbSchema.Def
import           DbSchema.Util.ToStar       (ToStar (..))


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
  | forall (ref::Symbol) rel. ( rel ~ TRelDef sch ref, t ~ RdTo rel
                              , CTabDef sch (RdFrom rel), CRelDef sch ref
                              )
    => Child (Proxy ref) (Cond db sch (RdFrom rel))
  --
  | forall (ref::Symbol) rel. ( rel ~ TRelDef sch ref, t ~ RdFrom rel
                              , CTabDef sch (RdTo rel), CRelDef sch ref
                              )
    => Parent (Proxy ref) (Cond db sch (RdTo rel))
--
deriving instance Show (Cond db sch t)

--
class CCond (n::Symbol) db sch (t::Symbol) where
  pcmp    :: (TabFld sch t n, CFldDef db n (TabFldType sch t n))
          => Cmp -> (TabFldType sch t n) -> Cond db sch t
  pnull   :: (TabFld sch t n, CFldDef db n (TabFldType sch t n)) => Cond db sch t
  pchild  :: forall rel . (rel ~ TRelDef sch n, t ~ RdTo rel
                          , CTabDef sch (RdFrom rel), CRelDef sch n
                          )
          => Cond db sch (RdFrom rel) -> Cond db sch t
  --
  pparent :: forall rel . (rel ~ TRelDef sch n, t ~ RdFrom rel
                          , CTabDef sch (RdTo rel), CRelDef sch n
                          )
          => Cond db sch (RdTo rel) -> Cond db sch t


instance CCond n db sch t where
  pcmp = Cmp (Proxy @n)
  pnull = Null (Proxy @n)
  pchild = Child (Proxy @n)
  pparent = Parent (Proxy @n)
--
instance (TabFld sch t n, v ~ TabFldType sch t n, CFldDef db n v)
      => IsLabel n (Cmp -> v -> Cond db sch t) where
  fromLabel = Cmp (Proxy @n)

-- instance  (rel ~ TRelDef sch n, p ~ RdTo rel, c ~ RdFrom rel
--           , CTabDef sch (RdFrom rel), CRelDef sch n
--           )
--       => IsLabel n (Cond db sch c -> Cond db sch p) where
--   fromLabel = Child (Proxy @n)

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

convCond :: Cond db sch t -> CondMonad (Text, [FieldDB db])
convCond (condition :: Cond db sch t) = case condition of
  Empty -> return ("1=1",[])
  (Cmp (_::Proxy n) cmp v) -> do
    let nvs = zip (map fst $ fldDbDef @db @n @(TabFldType sch t n))
            $ fldToDb @db @n v
    ntab <- lift $ fst <$> get
    (first (T.intercalate " AND ") . unzip) <$> mapM (\(n,vdb) ->
      let nm = format "t{}.{}" (ntab, n) in
        withPar (Proxy @db) $ \par ->
            (case cmp of
              Like    -> condLike @db nm par
              CmpS op -> format "{}{}{}" (nm,showCmp op,par)
            , vdb
            )
      ) nvs
  Null (_::Proxy n) -> return $ (,[])
                            $ T.intercalate " AND "
                            $ map ((`mappend` " IS NULL") . fst)
                            $ fldDbDef @db @n @(TabFldType sch t n)
  Not c -> first (format "NOT ({})" . Only) <$> convCond c
  BoolOp bo c1 c2 ->
    (\(cc1,d1) (cc2,d2) -> (format "({}) {} ({})" (cc1,show bo,cc2),d1++d2))
      <$> convCond c1 <*> convCond c2
  Child (_ :: Proxy ref) (cond::Cond db sch (RdFrom (TRelDef sch ref))) ->
    getRef  True
            (tabName @sch @(RdFrom (TRelDef sch ref)))
            (toStar @_ @(TRelDef sch ref))
            (convCond cond)
  Parent (_ :: Proxy ref) (cond::Cond db sch (RdTo (TRelDef sch ref))) ->
    getRef  False
            (tabName @sch @(RdTo (TRelDef sch ref)))
            (toStar @_ @(TRelDef sch ref))
            (convCond cond)
  where
    getRef  :: Bool -> Text -> RelDef Text -> CondMonad (Text, [FieldDB db])
            -> CondMonad (Text, [FieldDB db])
    getRef isChild tn rd cc = do
      pnum <- ask
      cnum <- lift $ do
        modify (first (+1))
        fst <$> get

      first (\c ->
          format "EXISTS (SELECT 1 FROM {} t{} WHERE {}{}({}))"
            ( tn
            , cnum
            , T.intercalate " AND "
                $ map ( (\(ch,pr) -> format "t{}.{} = t{}.{}" (cnum,ch,pnum,pr))
                      . (if isChild then id else swap)
                      )
                $ rdCols rd
            , if T.null c then (""::T.Text) else " AND "
            , c)
        ) <$> local (const cnum) cc
--
-- > dbCond $ pcmp @"id" @Dbs @Sch @"Customer" ==? 1 ||| pchild @"ordCust" (#id ==? 2 ||| pnull @"num")
-- ("(t0.id=?1) Or (EXISTS (SELECT 1 FROM Orders t1 WHERE t1.customerId = t0.id AND ((t1.id=?2) Or (num IS NULL))))",[SQLInteger 1,SQLInteger 2])
-- > dbCond $ pcmp @"id" @Dbs @Sch @"Orders" ==? 1 &&& pparent @"ordCust" (#id ==? 2) &&& pchild @"opOrd" (#num >? 3)
-- ("((t0.id=?1) And (EXISTS (SELECT 1 FROM Customer t1 WHERE t1.id = t0.customerId AND (t1.id=?2)))) And (EXISTS (SELECT 1 FROM OrderPosition t2 WHERE t2.orderId = t0.id AND (t2.num>?3)))",[SQLInteger 1,SQLInteger 2,SQLInteger 3])
-- > dbCond $ #id ==? 1 &&& pparent @"ordCust" @Dbs @Sch @"Orders" (#id ==? 2) &&& pchild @"opOrd" (#num >? 3)
-- ("((t0.id=?1)
  -- And (EXISTS (SELECT 1 FROM Customer t1 WHERE t1.id = t0.customerId AND (t1.id=?2))))
  -- And (EXISTS (SELECT 1 FROM OrderPosition t2 WHERE t2.orderId = t0.id AND (t2.num>?3)))"
  -- ,[SQLInteger 1,SQLInteger 2,SQLInteger 3])


dbCond :: Cond db sch t -> (Text, [FieldDB db])
dbCond = runCond . convCond
