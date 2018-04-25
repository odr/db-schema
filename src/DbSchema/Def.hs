{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}
module DbSchema.Def where

import           Data.Kind                    (Constraint, Type)
import           Data.List                    (group, sort, (\\))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH           (promote)
import qualified Data.Text                    as T
import           GHC.TypeLits                 (Symbol)
import           Type.Reflection              (Typeable, typeRep)

import           DbSchema.Util.ToStar         (TStar, ToStar (..))

data DelCons = DcRestrict | DcCascade | DcSetNull
  deriving (Show, Eq, Ord, Read)

promote [d|
  data TabDef s = TabDefC { tdFlds    :: [s]
                          , tdKey     :: [s]
                          , tdUniq    :: [[s]]
                          , tdAutoIns :: Bool
                          } deriving Show

  data RelDef s = RelDefC { rdFrom    :: s
                          , rdTo      :: s
                          , rdCols    :: [(s,s)]
                          , rdDelCons :: DelCons
                          } deriving Show

  isSet :: Ord a => [a] -> Bool
  isSet = null . filter ((==2) . length . take 2) . group . sort

  isSubSet :: Eq a => [a] -> [a] -> Bool
  isSubSet xs = null . (xs \\)

  checkTabDef :: (Eq s, Ord s) => TabDef s -> Bool
  checkTabDef (TabDefC fs pk uk _)
    = isSet fs && isSet pk && all isSet uk
    && isSubSet pk fs && all (`isSubSet` fs) uk


  isRefToKey :: Eq s => RelDef s -> TabDef s -> Bool
  isRefToKey rd td = map snd (rdCols rd) `elem` tdKey td : tdUniq td

  isRefFromEx :: Eq s => RelDef s -> TabDef s -> Bool
  isRefFromEx rd td = null $ (map fst (rdCols rd)) \\ tdFlds td

  lookups :: Eq s => [(s,t)] -> [s] -> [Maybe t]
  lookups sts = map (`lookup` sts)

  |]

class ( ToStar name
      , ToStar (TRelFrom sch name)
      , ToStar (TRelTo sch name)
      , ToStar (TTabDef sch name)
      -- дополнительные проверки
      -- все наборы в TTabDef без повторений, ключи корректные
      , CheckTabDef (TTabDef sch name) ~ True
      ) => CTabDef sch (name::Symbol) where
  type TTabDef sch name                 :: TabDef Symbol
  type TRelFrom sch name                :: [Symbol]
  type TRelTo sch name                  :: [Symbol]
  type TFldTypes sch name               :: [Type]
  type TFldType sch name (fld::Symbol)  :: Type
  type TFlds sch name                   :: [(Symbol,Type)]
  -- type TFlds sch name = Zip (TdFlds (TTabDef sch name)) (TFldTypes sch names)
  tabName :: T.Text
  tabName = toStar @_ @name
  tabDef                                :: TabDef T.Text
  tabDef = toStar @_ @(TTabDef sch name)
  relFrom                               :: [T.Text]
  relFrom = toStar @_ @(TRelFrom sch name)

  relTo                                 :: [T.Text]
  relTo   = toStar @_ @(TRelTo sch name)

class (ToStar name
      , ToStar (TRelDef sch name)
      -- дополнительные проверки
      -- существуют таблицы
      , CTabDef sch (RdFrom (TRelDef sch name))
      , CTabDef sch (RdTo   (TRelDef sch name))
      -- -- + проверить, что ссылка на PK или UK
      , IsRefToKey (TRelDef sch name)
                   (TTabDef sch (RdTo   (TRelDef sch name))) ~ True
      -- + проверить, что ссылка на PK или UK
      , IsRefFromEx (TRelDef sch name)
                   (TTabDef sch (RdFrom   (TRelDef sch name))) ~ True
      -- + проверить совпадение типов (с точностью до Maybe) from и to
      , RefTypesConstr
          ( Lookups (TFlds sch (RdFrom   (TRelDef sch name)))
                    (Map FstSym0 (RdCols (TRelDef sch name)))
          )
          ( Lookups (TFlds sch (RdTo     (TRelDef sch name)))
                    (Map SndSym0 (RdCols (TRelDef sch name)))
          )
      )
    => CRelDef sch (name::Symbol) where
  type TRelDef sch name :: RelDef Symbol
  relName :: T.Text
  relName = toStar @_ @name
  relDef :: RelDef T.Text
  relDef = toStar @_ @(TRelDef sch name)

class ( ToStar (TTables sch)
      , ToStar (TRels sch)
      ) => CSchema sch where
  type TTables sch  :: [Symbol]
  type TRels sch    :: [Symbol]
  tables            :: [T.Text]
  tables  = toStar @_ @(TTables sch)
  rels              :: [T.Text]
  rels    = toStar @_ @(TRels sch)

type family Constraints (cs::[Constraint]) :: Constraint where
  Constraints '[] = ()
  Constraints (x ': xs) = (x,Constraints xs)

-- Можно сделать особый тип для автоинсерта.
-- Тогда не надо признака у таблицы.
-- Но тогда портится этот констрейнт
-- Тип для автоинсерта зависит от БД, т.е. его нельзя определить здесь.

type family RefTypeConstraint a b :: Constraint where
  RefTypeConstraint a a = ()
  RefTypeConstraint (Maybe a) a = ()

type family RefTypesConstr (a :: [Maybe Type]) (b :: [Maybe Type]) :: Constraint where
  RefTypesConstr '[] '[] = ()
  RefTypesConstr ('Just a : as) ('Just b : bs)
    = (RefTypeConstraint a b, RefTypesConstr as bs)

--------------------------
-- User uses that types to define schema.
-- Then convert it by TH
data RelTo s = RT { rtName    :: s
                  , rtTo      :: s
                  , rtCols    :: [(s,s)]
                  , rtDelCons :: DelCons
                  } deriving Show

data TabPreDef s t = TPD  { tpdRec     :: t
                          , tpdKey     :: [s]
                          , tpdUniq    :: [[s]]
                          , tpdAutoIns :: Bool
                          , tpdRelTo   :: [RelTo s]
                          } deriving Show

------------
type instance TStar DelCons = DelCons
type instance TStar (TabDef Symbol) = TabDef T.Text
type instance TStar (RelDef Symbol) = RelDef T.Text

instance Typeable s => ToStar (s::DelCons) where
  toStar = read $ tail $ show $ typeRep @s

instance (ToStar fs, ToStar pk, ToStar uk, ToStar ai)
      => ToStar (TabDefC fs pk uk ai :: TabDef Symbol) where
  toStar = TabDefC (toStar @_ @fs) (toStar @_ @pk)
                   (toStar @_ @uk) (toStar @_ @ai)

instance (ToStar f, ToStar t, ToStar cs, ToStar cons)
      => ToStar (RelDefC f t cs cons :: RelDef Symbol) where
  toStar = RelDefC (toStar @_ @f) (toStar @_ @t)
                   (toStar @_ @cs) (toStar @_ @cons)
