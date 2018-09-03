{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module DbSchema.TH.MkSchema where

import           Data.Bifunctor (second)
import qualified Data.Text as T
import           Language.Haskell.TH
  (Dec(..), ExpQ, Name, Q, TyLit(..), Type(..), conT, litE, reifyInstances,
  sigE, stringL)

import           DbSchema.DDL (DDLRel(..), DDLSchema(..), DDLTab(..))
import           DbSchema.Def
  (CRelDef(..), CSchema(..), CTabDef(..), RelDef(..), TabDef(..), TabFld(..))
import           DbSchema.TH.MkView
  (mkView, nameToSym, recToFlds, strToSym, toPromotedList, toPromotedPair)

mkSchema :: Name -> Name -> Q Type -> Q [Dec]
mkSchema db sch qt
  = fmap fromPromotedList qt
  >>= fmap (second concat . unzip) . mapM tabPreToTabRel
  >>= mkInsts db sch

mkInst :: Q Type -> [Type] -> Q [Dec] -> Q [Dec]
mkInst qt ts qd = do
  (ConT c) <- qt
  ri <- reifyInstances c ts
  if null ri then qd else return []

mkInsts :: Name -> Name
        -> ([(Type,Name,Type,[(Type,Type)])],[((Type, Type), (Type,Type))])
        -> Q [Dec]
mkInsts db sch (tabs, rls) = concat <$> sequence
  [ concat <$> mapM (instTab db sch rls) tabs
  , concat <$> mapM (\(_,(n,d)) -> let nQ = return n in (++)
          <$> mkInst [t|CRelDef|] [ConT sch, n]
                [d| instance CRelDef $(schQ) $(nQ) where
                      type TRelDef $(schQ) $(nQ) = $(return d)
                |]
          <*> mkInst [t|DDLRel|] [ConT db, ConT sch, n]
                [d| instance DDLRel $(conT db) $(schQ) $(nQ) |]

        ) rls
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
    relNames = toPromotedList $ map (\(_,(rn,_))->rn) rls

instTab :: Name -> Name -> [((Type,Type),(Type, Type))]
        -> (Type, Name, Type, [(Type,Type)])
        -> Q [Dec]
instTab db sch rls (tabNm,rc,tabDf,flds) =
  concat <$> sequence ( [ inst >>= mapM (\i -> setInsts i <$> insts), instDDLT]
                      ++ map tabFld flds
                      )

  where
    schQ = conT sch
    tabQ = return tabNm
    instTabDef = [d|
      type instance TTabRec $(schQ) $(tabQ) = $(conT rc)
      type instance TTabDef $(schQ) $(tabQ) = $(return tabDf)
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
      $ filter (\((from,to),_) -> tabNm == if isFrom then from else to) rls
    instDDLT = mkInst [t|DDLTab|] [ConT db, ConT sch, tabNm]
                      [d| instance DDLTab $(conT db) $(schQ) $(tabQ) |]

    inst :: Q [Dec]
    inst = mkInst [t|CTabDef|] [ConT sch, tabNm]
                  [d| instance CTabDef $(schQ) $(tabQ) |]

    insts = concat <$> sequence
          [ instTabDef
          , concat <$> mapM instRel [True,False]
          ]
    setInsts (InstanceD a b c _) = InstanceD a b c
    setInsts _ = error "invalid instance definition in setInsts"
    tabFld (fn,ft) = mkInst [t|TabFld|] [ConT sch, tabNm, fn]
      [d| instance TabFld $(schQ) $(tabQ) $(return fn) where
            type TabFldType $(schQ) $(tabQ) $(return fn) = $(return ft)
      |]

tabPreToTabRel
  :: Type -> Q ((Type, Name, Type, [(Type,Type)]), [((Type,Type),(Type,Type))])
tabPreToTabRel (AppT (AppT (AppT (AppT (AppT _ (ConT rc)) pk) uk) ai) rlTo) = do
  flds <- recToFlds rc
  return
    ( ( tabNm
      , rc
      , AppT (AppT (AppT (AppT (PromotedT 'TabDefC)
                               (toPromotedList $ map fst flds)) pk) uk) ai
      , flds
      )
    , map (relToToRelDef tabNm) $ fromPromotedList rlTo
    )
  where
    tabNm = nameToSym rc
tabPreToTabRel _ = error "Invalid type in tabPreToTabRel"
-- Tab -> rel -> ((from,to),(rel,def))
relToToRelDef :: Type -> Type -> ((Type,Type),(Type,Type))
relToToRelDef rfrom (AppT (AppT (AppT (AppT _ (LitT (StrTyLit rname))) rto) rcols) rdc) =
  ( (rfrom,rto)
  , (strToSym rname, AppT (AppT (AppT (AppT (PromotedT 'RelDefC) rfrom) rto) rcols) rdc)
  )
relToToRelDef _ _ = error "err in relToToRelDef"

symToStrEQ :: Type -> ExpQ
symToStrEQ (LitT (StrTyLit v)) = sigE (litE $ stringL v) [t| T.Text |]
symToStrEQ _                   = error "Invalid type in symToStrEQ"
