{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module DbSchema.TH.MkView where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import           Data.Bifunctor            (second)
import           Data.List                 (partition, (\\))
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import           Language.Haskell.TH

import           DbSchema.Db
import           DbSchema.DDL
import           DbSchema.Def
-- import           DbSchema.TH.MkSchema

type MkViewMonad = RWST (Name,Name,S.Set Name) [Dec] (S.Set Name) Q

mkView :: Name -> Name -> Name -> DecsQ
mkView db sch rc = do
  (ClassI _ ri) <- reify ''CRecDef
  crd <- [t|CRecDef|]
  snd <$> execRWST  (mkViewM rc)
                    (db, sch
                      , S.fromList $ map getInst $ filter (isInst crd) ri)
                    mempty
  -- InstanceD Nothing [] (AppT (AppT (AppT (ConT DbSchema.Def.CRecDef)
  --      (ConT Model.Dbs)) (ConT Model.Sch)) (ConT Model.CurrRate)) []
  where
    isInst crd (InstanceD _ _ (AppT (AppT (AppT c t1) t2) (ConT _)) _)
      = c == crd && t1 == ConT db && t2 == ConT sch
    isInst crd _ = False
    getInst (InstanceD _ _ (AppT _ (ConT t)) _) = t

mkViewM :: Name -> MkViewMonad ()
mkViewM rc = do
  flds <- lift $ recToFlds rc
  let (fs, cs) = partition (isFld . snd) flds
  (db,sch,ss1) <- ask
  ss2 <- get
  let newInst = S.fromList (map (getListType . snd) cs) S.\\ ss1 S.\\ ss2
  put newInst
  mapM_ mkViewM newInst
  let [dbQ, schQ, rcQ] = map conT [db,sch,rc]
  lift (concat <$> mapM (decLens rcQ) flds) >>= tell
  rs <- lift $ [d|
    instance CRecDef $(dbQ) $(schQ) $(rcQ) where
      -- type TRecTab $(dbQ) $(schQ) $(rcQ) = $(tabq)
      type TRecFlds $(dbQ) $(schQ) $(rcQ)
        = $(return $ toPromotedList $ map toPromotedPair fs)
      type TRecChilds $(dbQ) $(schQ) $(rcQ)
        = $(return $ toPromotedList
                   $ map (toPromotedPair . second (ConT . getListType)) cs)

      recDbDef  = concat $(expLstFldDbDef dbQ fs)
      recToDb c = concatMap ($ c) $(expLstToDb dbQ rcQ fs)
      recFromDb = $(expFromDb db sch flds)
    |]
  tell rs
  where
    isFld = \case
      (AppT ListT (ConT _))  -> False
      _                      -> True
    getListType (AppT ListT (ConT t)) = t

    decLens rcQ (s,t) =
      --  TH плохо работает с DuplicateRecordFields. Через generics - работает.
      --  Пока так...
      [d| instance RecLens $(return s) $(rcQ) $(return t) where
            recLens = field @($(return s))
      |]

    expLstFldDbDef dbQ = listE . map getFld
      where
        getFld (s,t) = [e| fldDbDef @($(dbQ)) @($(return s)) @($(return t)) |]

    expLstToDb dbQ rcQ = listE . map expFldToDb
      where
        expFldToDb (s,t) = [e| fldToDb @($(dbQ)) @($(return s)) @($(return t))
                             . (^. $(rl)) |]
           where
     -- через quotation не получается, похоже на баг (из-за type appl, ghc-8.2)
             rl = appTypeE (appTypeE (appTypeE
                           [e| recLens |] (return s)) rcQ) (return t)
    expFromDb db sch flds =
      case map (\(s,t) -> if isFld t
                  then [e| fldFromDb @($(dbQ)) @($(return s)) @($(return t)) |]
                  else [e| pure [] |]
               ) flds of
        (x:xs) -> foldl (\b a -> [e| (<*>) $(b) $(a) |])
                        [e| $(conByType rc) <$> $(x)|] xs
        _ -> fail $ "Error for fldFromDb for " ++ pprint db ++ ", "
                  ++ pprint sch ++ ", " ++ pprint rc
      where
        [dbQ, schQ, rcQ] = map conT [db,sch,rc]
--
recToFlds :: Name -> Q [(Type,Type)]
recToFlds n = fmap (\case
    TyConI (DataD _ _ _ _ [RecC _ flds] _) ->
      map (\(name,_,typ) -> (nameToSym name,typ)) flds
    x -> error $ "invalid info in recToFld: " ++ show x
  ) $ reify n

toPromotedList :: [Type] -> Type
toPromotedList = \case
  [] -> PromotedNilT
  (x:xs) -> AppT (AppT PromotedConsT x) (toPromotedList xs)

toPromotedPair :: (Type,Type) -> Type
toPromotedPair (x,y) = AppT (AppT (PromotedTupleT 2) x) y

nameToSym :: Name -> Type
nameToSym = strToSym . nameBase

strToSym :: String -> Type
strToSym = LitT . StrTyLit

conByType :: Name -> ExpQ
conByType n = reify n >>= \case
  TyConI (DataD _ _ _ _ [RecC c _] _) -> conE c
  _ -> fail $ "Error on getting single constructor for type " ++ pprint n
