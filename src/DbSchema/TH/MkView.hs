{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module DbSchema.TH.MkView where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import           Data.Bifunctor            (second)
import           Data.List                 (nub, partition, (\\))
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import           Language.Haskell.TH

import           DbSchema.Db
import           DbSchema.DDL
import           DbSchema.Def
import           DbSchema.DML
import           DbSchema.Util.RecLens

-- import           DbSchema.TH.MkSchema

reifyShow :: Name -> ExpQ
reifyShow n = reify n >>= stringE . show

data ViewEnv = VE { veDb   :: Name
                  , veSch  :: Name
                  -- , veTabs :: M.Map T.Text (TabDef T.Text)
                  , veRels :: M.Map T.Text (RelDef T.Text)
                  , veInst :: S.Set (Type, Name)
                  }
type MkViewMonad = RWST ViewEnv [Dec] (S.Set (Type, Name)) Q

mkView :: Name -> Name -> String -> M.Map T.Text (RelDef T.Text) -> Name -> DecsQ
mkView db sch sTabName rels rc = do
  (ClassI _ ri) <- reify ''CRecDef
  crd <- [t|CRecDef|]
  snd <$> execRWST (mkViewM tn rc)
                   (VE db sch rels
                     $ S.fromList $ map getInst $ filter (isInst crd) ri)
                   mempty
  where
    tn = LitT (StrTyLit sTabName)
    isInst crd (InstanceD _ _
                  (AppT (AppT (AppT (AppT (AppT c t1) t2) _) (ConT _)) _) _)
      = c == crd && t1 == ConT db && t2 == ConT sch
    isInst crd _ = False
    getInst (InstanceD _ _ (AppT (AppT (AppT _ t3) (ConT t4)) _) _) = (t3,t4)

mkViewM :: Type -> Name -> MkViewMonad ()
mkViewM tn rc = do
  flds <- lift $ recToFlds rc
  let (fs, cs) = partition (isFld . snd) flds
  (VE db sch rls ss1) <- ask
  let [dbQ, schQ, rcQ] = map conT [db,sch,rc]
  ss2 <- get
  -- addInst <- mapM (\(a,AppT ListT (ConT t)) -> (,t) <$> childTab a rels) cs
        -- fromMaybe (error "") $ (\ins -> S.fromList ins S.\\ ss1 S.\\ ss2)
  let newInst =
        S.fromList (map (
            \(LitT (StrTyLit s),AppT ListT (ConT t)) ->
              maybe (error
                    $ "There is no relation who corresponding to the field '"
                      ++ s ++ "'"
                    )
                    ((,t) . strToSym . T.unpack . rdFrom)
                    $ M.lookup (T.pack s) rls) cs
                )
                S.\\ ss1 S.\\ ss2

  put newInst
  mapM_ (uncurry mkViewM) newInst
  lift (concat <$> mapM (decLens rcQ) flds) >>= tell
  -- lift $ reportWarning $ "instances for " ++ pprint tn ++ ", " ++ pprint rc
  rs <- lift $ [d|
    instance Rec $(rcQ) where
      type TRec $(rcQ) = $(return (toPromotedList $ map toPromotedPair fs))

    instance CRecDef $(dbQ) $(schQ) $(rcQ) where
      -- type TRecTab $(dbQ) $(schQ) $(rcQ) = $(tabq)
      type TRecFlds $(dbQ) $(schQ) $(rcQ)
        = $(return (toPromotedList $ map toPromotedPair fs))
      type TRecChilds $(dbQ) $(schQ) $(rcQ)
        = $(return (toPromotedList
                      $ map (toPromotedPair . second (ConT . getListType)) cs))

      recDbDef  = concat $(expLstFldDbDef dbQ fs)
      recToDb c = concatMap ($ c) $(expLstToDb dbQ rcQ fs)
      recFromDb = $(expFromDb db sch flds)

    instance ( CRecDef $(dbQ) $(schQ) p, TRecChilds $(dbQ) $(schQ) p ~ '[]
             , DmlChild $(dbQ) $(schQ) (TRecChilds $(dbQ) $(schQ) $(rcQ)) p $(rcQ)
             )
          => DML $(dbQ) $(schQ) $(return tn) p $(rcQ)
    |]
  tell rs
  where
    isFld = \case
      (AppT ListT (ConT _))  -> False
      _                      -> True
    getListType (AppT ListT (ConT t)) = t

    decLens rcQ (s,t) =
      --  TH не работает с DuplicateRecordFields. Через generics - работает.
      --  Пока так...
      [d| instance RecLens $(return s) $(rcQ) where
            type TLens $(return s) $(rcQ) = $(return t)
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
             rl = appTypeE (appTypeE [e| recLens |] (return s)) rcQ
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
recToFlds n = (\case
    TyConI (DataD _ _ _ _ [RecC _ flds] _) ->
      map (\(name,_,typ) -> (nameToSym name,typ)) flds
    x -> error $ "invalid info in recToFld: " ++ show x
  ) <$> reify n

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
