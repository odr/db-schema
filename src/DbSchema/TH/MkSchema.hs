{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module DbSchema.TH.MkSchema where

import           Data.Bifunctor      (second)
import qualified Data.Text           as T
import           Language.Haskell.TH (Dec (..), ExpQ, Name, Q (..), TyLit (..),
                                      Type (..), conT, litE, sigE, stringL)

import           DbSchema.DDL        (DDLRel (..), DDLSchema (..), DDLTab (..))
import           DbSchema.Def        (CRelDef (..), CSchema (..), CTabDef (..),
                                      RelDef (..), TabDef (..), TabFld (..))
import           DbSchema.TH.MkView  (mkView, nameToSym, recToFlds, strToSym,
                                      toPromotedList, toPromotedPair)


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
        instance DDLSchema $(conT db) $(schQ)
    |]
  , concat <$> mapM (\(LitT (StrTyLit tn),rc,_,_) -> 
                        mkView db sch tn mempty rc) tabs
  ]
  where
    schQ = conT sch
    tabNames = toPromotedList $ map (\(tn,_,_,_)->tn) tabs
    relNames = toPromotedList $ map (\(_,(rn,_))->rn) rels

instTab :: Name -> Name -> [((Type,Type),(Type, Type))]
        -> (Type, Name, Type, [(Type,Type)])
        -> Q [Dec]
instTab db sch rels (tabName,rc,tabDef,flds) =
  concat <$> sequence ( [ inst >>= mapM (\i -> setInsts i <$> insts), instDDLT]
                      ++ map tabFld flds
                      )

  where
    schQ = conT sch
    tabQ = return tabName
    instTabDef = [d|
      type instance TTabRec $(schQ) $(tabQ) = $(conT rc)
      type instance TTabDef $(schQ) $(tabQ) = $(return tabDef)
      type instance TFlds $(schQ) $(tabQ) = $(fldsQ)
      |]
      where
        fldsQ = return $ toPromotedList $ map toPromotedPair flds
    instRel isFrom
      = (\ns -> map (\(TySynInstD _ eqn) ->
                   TySynInstD (if isFrom then ''TRelFrom else ''TRelTo) eqn)
            <$> [d|type instance TRelFrom $(schQ) $(tabQ) = $(return ns)|])
      $ toPromotedList
      $ map (\(_,(n,_)) -> n)
      $ filter (\((from,to),_) -> tabName == if isFrom then from else to) rels
    instDDLT = [d| instance DDLTab $(conT db) $(schQ) $(tabQ) |]

    inst :: Q [Dec]
    inst = [d| instance CTabDef $(schQ) $(tabQ) |]

    insts = concat <$> sequence
          [ instTabDef
          , concat <$> mapM instRel [True,False]
          ]
    setInsts (InstanceD a b c _) = InstanceD a b c
    tabFld (fn,ft) = [d|
      instance TabFld $(schQ) $(tabQ) $(return fn) where
        type TabFldType $(schQ) $(tabQ) $(return fn) = $(return ft)
      |]

tabPreToTabRel
  :: Type -> Q ((Type, Name, Type, [(Type,Type)]), [((Type,Type),(Type,Type))])
tabPreToTabRel (AppT (AppT (AppT (AppT (AppT _ (ConT rc)) pk) uk) ai) relTo) = do
  flds <- recToFlds rc
  return
    ( ( tabName
      , rc
      , AppT (AppT (AppT (AppT (PromotedT 'TabDefC)
                               (toPromotedList $ map fst flds)) pk) uk) ai
      , flds
      )
    , map (relToToRelDef tabName) $ fromPromotedList relTo
    )
  where
    tabName = nameToSym rc

-- Tab -> rel -> ((from,to),(rel,def))
relToToRelDef :: Type -> Type -> ((Type,Type),(Type,Type))
relToToRelDef rfrom (AppT (AppT (AppT (AppT _ (LitT (StrTyLit rname))) rto) rcols) rdc) =
  ( (rfrom,rto)
  , (strToSym rname, AppT (AppT (AppT (AppT (PromotedT 'RelDefC) rfrom) rto) rcols) rdc)
  )
relToToRelDef rfrom x = error "err in relToToRelDef"

fromPromotedList :: Type -> [Type]
fromPromotedList = \case
  PromotedNilT -> []
  AppT (AppT PromotedConsT x) xs -> x : fromPromotedList xs
  _ -> error "Invalid promoted list"

symToStrEQ :: Type -> ExpQ
symToStrEQ (LitT (StrTyLit v)) = sigE (litE $ stringL v) [t| T.Text |]
