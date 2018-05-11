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
module DbSchema.Def where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.State.Strict (State, evalState, get, put)
import           Data.Kind                        (Constraint, Type)
import           Data.List                        (group, sort, (\\))
import qualified Data.Map                         as M
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH               (promote)
import           Data.Tagged                      (Tagged (..), untag)
import qualified Data.Text                        as T
import           Lens.Micro                       ((&), (.~), (^.))
import           Type.Reflection                  (Typeable, typeRep)

import           DbSchema.Db                      (Db (..), DelCons, MonadIO,
                                                   SessionMonad)
import           DbSchema.Util.RecLens
import           DbSchema.Util.ToStar             (TStar, ToStar (..))


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
  isRefFromEx rd td = null $ map fst (rdCols rd) \\ tdFlds td

  lookups :: Eq s => [(s,t)] -> [s] -> [Maybe t]
  lookups sts = map (`lookup` sts)

  |]

class ( ToStar name
      , ToStar (TRelFrom sch name)
      , ToStar (TRelTo   sch name)
      , ToStar (TTabDef  sch name)
      -- дополнительные проверки
      -- все наборы в TTabDef без повторений, ключи корректные
      , CheckTabDef (TTabDef sch name) ~ True
      ) => CTabDef sch (name::Symbol) where
  type TTabDef   sch name               :: TabDef Symbol
  type TTabRec   sch name               :: Type
  type TRelFrom  sch name               :: [Symbol]
  type TRelTo    sch name               :: [Symbol]
  -- type TFldTypes sch name               :: [Type]
  -- type TFldType  sch name (fld::Symbol) :: Type
  type TFlds     sch name               :: [(Symbol,Type)]
  tabName :: T.Text
  tabName = toStar @_ @name
  tabDef                                :: TabDef T.Text
  tabDef  = toStar @_ @(TTabDef sch name)
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
      -- + проверить, что ссылка на PK или UK
      , IsRefToKey (TRelDef sch name)
                   (TTabDef sch (RdTo   (TRelDef sch name))) ~ True
      -- + проверить, что ссылка на PK или UK
      , IsRefFromEx (TRelDef sch name)
                   (TTabDef sch (RdFrom (TRelDef sch name))) ~ True
      -- + проверить совпадение типов (с точностью до Maybe) from и to
      , RefTypesConstr
          ( Lookups (TFlds sch   (RdFrom (TRelDef sch name)))
                    (Map FstSym0 (RdCols (TRelDef sch name)))
          )
          ( Lookups (TFlds sch   (RdTo   (TRelDef sch name)))
                    (Map SndSym0 (RdCols (TRelDef sch name)))
          )
      )
    => CRelDef sch (name::Symbol) where
  type TRelDef sch name :: RelDef Symbol
  relName :: T.Text
  relName = toStar @_ @name
  relDef :: RelDef T.Text
  relDef = toStar @_ @(TRelDef sch name)

type family TabDefs sch (xs::[Symbol]) :: [(Symbol,TabDef Symbol)] where
  TabDefs sch '[] = '[]
  TabDefs sch (x ': xs) = '(x, TTabDef sch x) ': TabDefs sch xs

type family RelDefs sch (xs::[Symbol]) :: [(Symbol,RelDef Symbol)] where
  RelDefs sch '[] = '[]
  RelDefs sch (x ': xs) = '(x, TRelDef sch x) ': RelDefs sch xs

class (ToStar (TabDefs sch (TTables sch)), ToStar (RelDefs sch (TRels sch)))
    => CSchema sch where
  type TTables sch  :: [Symbol]
  type TRels sch    :: [Symbol]
  tables :: M.Map T.Text (TabDef T.Text)
  tables = M.fromList (toStar @_ @(TabDefs sch (TTables sch)))
  rels :: M.Map T.Text (RelDef T.Text)
  rels = M.fromList (toStar @_ @(RelDefs sch (TRels sch)))

type family RefTypeConstraint a b :: Constraint where
  RefTypeConstraint a a = ()
  RefTypeConstraint (Maybe a) a = ()

type family RefTypesConstr (a :: [Maybe Type]) (b :: [Maybe Type]) :: Constraint where
  RefTypesConstr '[] '[] = ()
  RefTypesConstr ('Just a : as) ('Just b : bs)
    = (RefTypeConstraint a b, RefTypesConstr as bs)

class Show (TabFldType sch t s) => TabFld sch t s where
  type TabFldType sch t s

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

----------- Db-aware -------------
type FromDbMonad db = ExceptT T.Text (State [FieldDB db])

-- CFldDef are not generated by TH
-- class Db db => CFldDef db val where
--   -- один Haskell-тип может быть конвертирован в несколько полей БД
--   fldDbDef :: [(T.Text,Bool)]
--   fldToDb :: T.Text -> val -> [FieldDB db]
--   -- fldFromDb :: [FieldDB db] -> Either T.Text (val, [FieldDB db])
--   fldFromDb :: T.Text -> FromDbMonad db val

class (Db db, ToStar name) => CFldDef db (name::Symbol) val where
  -- один Haskell-тип может быть конвертирован в несколько полей БД
  fldDbDef :: [(T.Text, (T.Text,Bool))]
  fldToDb :: val -> [FieldDB db]
  -- fldFromDb :: [FieldDB db] -> Either T.Text (val, [FieldDB db])
  fldFromDb :: FromDbMonad db val

defToDb :: (val -> FieldDB db) -> val -> [FieldDB db]
defToDb f v = [f v]

defFromDb :: ToStar name
          => Proxy (name :: Symbol) -> (FieldDB db -> Maybe val)
          -> FromDbMonad db val
defFromDb (_:: Proxy name) f = lift get >>= \case
    (x:xs) -> maybe err (\r -> lift (put xs) >> return r) $ f x
    _ -> err
    where
      err = throwE $ toStar @_ @name

defMbToDb :: CFldDef db name val
          => Proxy db -> Proxy name -> FieldDB db -> Maybe val
          -> [FieldDB db]
defMbToDb (_::Proxy db) (p::Proxy name) df v
  = maybe (defToDb @_ @db (const df) v) (fldToDb @db @name) v

defMbFromDb :: CFldDef db name val
            => Proxy db -> Proxy name -> (FieldDB db -> Bool)
            -> FromDbMonad db (Maybe val)
defMbFromDb (_::Proxy db) (p::Proxy name) isNull
  = defFromDb @name @db p
         (\case
            v | isNull v  -> Just Nothing
              | otherwise -> either (const Nothing) (Just . Just)
                           $ evalState (runExceptT (fldFromDb @db @name)) [v])

type family RecC db (fs :: [(Symbol,Type)]) :: Constraint where
  RecC db '[] = ()
  RecC db ( '(s, t) ': xs) = (CFldDef db s t, RecC db xs)

type family ChildsC db sch (cs :: [(Symbol,Type)]) :: Constraint where
  ChildsC db sch '[] = ()
  ChildsC db sch ( '(s, t) ': cs) = ( CRelDef sch s
                                    , CRecDef db sch t
                                    , ChildsC db sch cs
                                    )

-- type family TabByRels sch (cs :: [(Symbol,Type)]) :: Maybe Symbol where
--   TabByRels sch '[ '(s,t)] = Just (RdFrom (TRelDef sch s))
--   TabByRels sch (x1 ': x2 ': xs) =
--     If (TabByRels sch '[x1] == TabByRels sch (x2 ': xs))
--       (TabByRels sch '[x1]) 'Nothing

class ( RecC db (TRecFlds db sch a)
      , ChildsC db sch (TRecChilds db sch a)
      -- , CTabDef sch (TRecTab db sch a)
      ) => CRecDef db (sch::Type) (a::Type) where
  -- type TRecTab db sch a  :: Symbol
  type TRecFlds db sch a :: [(Symbol,Type)]
  type TRecChilds db sch a :: [(Symbol,Type)]
  recDbDef :: [(T.Text, (T.Text,Bool))]
  recToDb :: a -> [FieldDB db]
  recFromDb :: FromDbMonad db a
  getFromDb :: [FieldDB db] -> Either T.Text a
  getFromDb = evalState (runExceptT (recFromDb @db @sch))

instance CRecDef db sch (Tagged ('[]::[Symbol]) ()) where
  type TRecFlds db sch (Tagged ('[]::[Symbol]) ()) = '[]
  type TRecChilds db sch (Tagged ('[]::[Symbol]) ()) = '[]
  recDbDef = []
  recToDb _ = []
  recFromDb = return $ Tagged ()

instance CFldDef db n a
      => CRecDef db sch (Tagged ('[n]::[Symbol]) a) where
  type TRecFlds db sch (Tagged '[n] a) = '[ '(n,a)]
  type TRecChilds db sch (Tagged '[n] a) = '[]
  recDbDef = fldDbDef @db @n @a
  recToDb (Tagged x) = fldToDb @db @n @a x
  recFromDb = Tagged <$> fldFromDb @db @n @a
--------------------------------------------
instance (CRecDef db sch (Tagged '[x1] a), CRecDef db sch (Tagged (x2 ': xs) b))
      => CRecDef db sch (Tagged (x1 ': x2 ': xs ::[Symbol]) (a,b)) where
  type TRecFlds db sch (Tagged (x1 ': x2 ': xs :: [Symbol]) (a,b))
    = TRecFlds db sch (Tagged '[x1] a) :++ TRecFlds db sch (Tagged (x2 ': xs) b)
  type TRecChilds db sch (Tagged (x1 ': x2 ': xs :: [Symbol]) (a,b)) = '[]
  recDbDef = recDbDef @db @sch @(Tagged '[x1] a)
          ++ recDbDef @db @sch @(Tagged (x2 ': xs) b)
  recToDb (Tagged (a,b)) = recToDb @db @sch @(Tagged '[x1] a) (Tagged a)
                        ++ recToDb @db @sch @(Tagged (x2 ': xs) b) (Tagged b)
  recFromDb = (\(Tagged a) (Tagged b) -> Tagged (a,b))
          <$> recFromDb @db @sch @(Tagged '[x1] a)
          <*> recFromDb @db @sch @(Tagged (x2 ': xs) b)
--
class SetPK (td :: TabDef Symbol) db r where
  setPK :: MonadIO m => r -> SessionMonad db m r

instance SetPK (TabDefC f k u False) db r where
  setPK = return

instance (Db db, RecLens k r, GenKey db ~ TLens k r)
      => SetPK (TabDefC f '[k] u True) db r where
  setPK r = do
    k <- getLastKey @db
    return $ r & recLens @k .~ k
