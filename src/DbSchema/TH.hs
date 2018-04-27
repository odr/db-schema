{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module DbSchema.TH where

import           Data.Bifunctor      (second)
-- import           Data.Generics.Product.Fields
import qualified Data.Text           as T
import           Language.Haskell.TH

import           DbSchema.Db
import           DbSchema.DDL
import           DbSchema.Def
-- import           Lens.Micro


mkSchema :: Name -> Name -> Q Type -> Q [Dec]
mkSchema db sch qt
  = fmap fromPromotedList qt
  >>= fmap (second concat . unzip) . mapM tabPreToTabRel
  >>= mkInsts db sch

mkInsts :: Name -> Name
        -> ([(Type,Name,Type,[(Type,Type)])],[((Type, Type), (Type,Type))])
        -> Q [Dec]
mkInsts db sch (tabs, rels) = concat <$> sequence
  [ concat <$> mapM (instTab db sch rels) tabs
  , concat <$> mapM (\(_,(n,d)) -> let nQ = return n in
          [d| instance CRelDef $(schQ) $(nQ) where
                type TRelDef $(schQ) $(nQ) = $(return d)
              instance DDLRel $(conT db) $(schQ) $(nQ)
          |]
        ) rels
  , [d| instance CSchema $(schQ) where
          type TTables $(schQ) = $(return tabNames)
          type TRels   $(schQ) = $(return relNames)
          -- tables               = $(return $ symLtoTextL tabL)
          -- rels                 = $(return $ symLtoTextL relL)
        instance DDLSchema $(conT db) $(schQ)
    |]
  , concat <$> mapM (instRec db sch rels) tabs
  ]
  where
    schQ = conT sch
    tabL = map (\(tn,_,_,_)->tn) tabs
    relL = map (\(_,(rn,_))->rn) rels
    tabNames = toPromotedList tabL
    relNames = toPromotedList relL

instTab :: Name -> Name -> [((Type,Type),(Type, Type))]
        -> (Type, Name, Type, [(Type,Type)])
        -> Q [Dec]
instTab db sch rels (tabName,rc,tabDef,flds) = do
  concat <$> sequence [inst >>= mapM (\i -> setInsts i <$> insts), instDDLT]
  where
    schQ = conT sch
    tabQ = return tabName
    instTabDef = [d|
      type instance TTabDef $(schQ) $(tabQ) = $(return tabDef)
      type instance TFlds $(schQ) $(tabQ)
        = $(return $ toPromotedList $ map toPromotedPair flds)
      type instance TFldTypes $(schQ) $(tabQ)
        = $(return $ toPromotedList $ map snd flds)
      |]
    instRel isFrom
      = (\ns -> map ((\(TySynInstD _ eqn) ->
                   TySynInstD (if isFrom then ''TRelFrom else ''TRelTo) eqn))
            <$> [d|type instance TRelFrom $(schQ) $(tabQ) = $(return ns)|])
      $ toPromotedList
      $ map (\(_,(n,_)) -> n)
      $ filter (\((from,to),_) -> tabName == if isFrom then from else to) rels
    instFldType = concat <$> mapM (\(fld,ft) ->
        [d| type instance TFldType $(schQ) $(tabQ) $(return fld) = $(return ft) |]
      ) flds
    instDDLT = [d| instance DDLTab $(conT db) $(schQ) $(tabQ) |]

    inst :: Q [Dec]
    inst = [d| instance CTabDef $(schQ) $(tabQ) |]

    insts = concat <$> sequence
          [ instTabDef
          , concat <$> mapM instRel [True,False]
          , instFldType
          ]
    setInsts (InstanceD a b c _) = InstanceD a b c

instRec :: Name -> Name -> [((Type,Type),(Type, Type))]
        -> (Type, Name, Type, [(Type,Type)])
        -> Q [Dec]
instRec db sch rels (tabName,rc,tabDef,flds)
  = concat <$> sequence [concat <$> mapM decLens flds, inst]
  where
    [dbQ, schQ, rcQ] = map conT [db,sch,rc]
    toDb = map snd flds
    par = mkName "x"
    inst = [d|
      instance CRecDef $(dbQ) $(schQ) $(rcQ) where
        type TRecTab $(dbQ) $(schQ) $(rcQ) = $(return tabName)
        type TRecFlds $(dbQ) $(schQ) $(rcQ)
          = $(return $ toPromotedList $ map toPromotedPair flds)
        type TRecChilds $(dbQ) $(schQ) $(rcQ) = $(promotedNilT)
        recFldNames = $(listE $ map (symToStrEQ . fst) flds)
        recToDb c = concatMap ($ c) $(expLstToDb)
        recFromDb = $(expFromDb)
      |]
    decLens (s,t) =
      [d| instance RecLens $(return s) $(rcQ) $(return t) where
            recLens = field @($(return s))
      |]

    expLstToDb = listE $ map expFldToDb flds
      where
        expFldToDb (s,t) = [e| fldToDb @($(dbQ)) @($(return s)) @($(return t))
                             . (^. $(rl)) |]
           where
     -- через quotation не получается, похоже на баг (из-за type appl, ghc-8.2)
             rl = appTypeE (appTypeE (appTypeE
                           [e| recLens |] (return s)) rcQ) (return t)
    expFromDb =
      case map (\(s,t) -> [e| fldFromDb @($(dbQ)) @($(return s))
                                        @($(return t)) |]) flds of
        (x:xs) -> foldl (\b a -> [e| (<*>) $(b) $(a) |])
                        [e| $(conByType rc) <$> $(x)|] xs
        _ -> fail $ "Error for fldFromDb for " ++ pprint db ++ ", "
                  ++ pprint sch ++ ", " ++ pprint rc

conByType :: Name -> ExpQ
conByType n = reify n >>= \case
  TyConI (DataD _ _ _ _ [RecC c _] _) -> conE c
  _ -> fail $ "Error on getting single constructor for type " ++ pprint n

tabPreToTabRel
  :: Type -> Q ((Type, Name, Type, [(Type,Type)]), [((Type,Type),(Type,Type))])
tabPreToTabRel (AppT (AppT (AppT (AppT (AppT _ (ConT rec)) pk) uk) ai) relTo) = do
  flds <- recToFlds rec
  return
    ( ( tabName
      , rec
      , AppT (AppT (AppT (AppT (PromotedT 'TabDefC)
                               (toPromotedList $ map fst flds)) pk) uk) ai
      , flds
      )
    , map (relToToRelDef tabName) $ fromPromotedList relTo
    )
  where
    tabName = nameToSym rec

-- Tab -> rel -> ((from,to),(rel,def))
relToToRelDef :: Type -> Type -> ((Type,Type),(Type,Type))
relToToRelDef rfrom (AppT (AppT (AppT (AppT _ (LitT (StrTyLit rname))) rto) rcols) rdc) =
  ( (rfrom,rto)
  , (strToSym rname, AppT (AppT (AppT (AppT (PromotedT 'RelDefC) rfrom) rto) rcols) rdc)
  )
relToToRelDef rfrom x = error "err in relToToRelDef"

recToFlds :: Name -> Q [(Type,Type)]
recToFlds n = fmap (\case
    TyConI (DataD _ _ _ _ [RecC _ flds] _) ->
      map (\(name,_,typ) -> (nameToSym name,typ)) flds
    x -> error $ "invalid info in recToFld: " ++ show x
  ) $ reify n

nameToSym :: Name -> Type
nameToSym = strToSym . nameBase

strToSym :: String -> Type
strToSym = LitT . StrTyLit

fromPromotedList :: Type -> [Type]
fromPromotedList = \case
  PromotedNilT -> []
  AppT (AppT PromotedConsT x) xs -> x : fromPromotedList xs
  _ -> error "Invalid promoted list"

toPromotedList :: [Type] -> Type
toPromotedList = \case
  [] -> PromotedNilT
  (x:xs) -> AppT (AppT PromotedConsT x) (toPromotedList xs)

toPromotedPair :: (Type,Type) -> Type
toPromotedPair (x,y) = AppT (AppT (PromotedTupleT 2) x) y

symToStrEQ :: Type -> ExpQ
symToStrEQ (LitT (StrTyLit v)) = sigE (litE $ stringL v) [t| T.Text |]
