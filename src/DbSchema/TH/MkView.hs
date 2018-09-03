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

import           Control.Monad (when)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import           Data.Bifunctor (second)
import           Data.List (partition)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S
import           Data.Tagged (Tagged)
import qualified Data.Text as T
-- import           Debug.Trace
import           Language.Haskell.TH

import           DbSchema.Def
import           DbSchema.DML
import           DbSchema.Util.RecLens

-- showReify :: Name -> ExpQ
-- showReify n = reify n >>= stringE . show

data ViewEnv = VE { veDb   :: Name
                  , veSch  :: Name
                  , veRels :: M.Map T.Text (RelDef T.Text)
                  }
data ViewStat = VS  { vsRecDef  :: S.Set Name
                    , vsRec     :: S.Set Name
                    , vsDml     :: S.Set (Type, Type, Name)
                    , vsRecLens :: S.Set (Type, Name)
                    }
type MkViewMonad = RWST ViewEnv [Dec] ViewStat Q

mkView :: Name -> Name -> String -> M.Map T.Text (RelDef T.Text) -> Name -> DecsQ
mkView db sch sTabName rls rc = do
  (ClassI _ riRecDef) <- reify ''CRecDef
  crd <- [t|CRecDef|]
  (ClassI _ riDml) <- reify ''DML
  dml <- [t|DML|]
  (ClassI _ riRec) <- reify ''Rec
  trec <- [t|Rec|]
  (ClassI _ riRecLens) <- reify ''RecLens
  recLs <- [t|RecLens|]

  p <- [t|()|]
  snd <$> execRWST
    (mkViewM tn p rc)
    (VE db sch rls)
    (VS { vsRecDef = S.fromList $ mapMaybe (mbRecDef crd) riRecDef
        , vsRec = S.fromList $ mapMaybe (mbTrec trec) riRec
        , vsDml = S.fromList $ mapMaybe (mbDml dml) riDml
        , vsRecLens = S.fromList $ mapMaybe (mbRecLens recLs) riRecLens
        }
    )
  -- reportWarning (pprint ds)
  -- return ds
  where
    tn = LitT (StrTyLit sTabName)
    mbRecDef cls (InstanceD _ _ (AppT (AppT (AppT c t1) t2) (ConT t3)) _)
      | c == cls && t1 == ConT db && t2 == ConT sch = Just t3
    mbRecDef _ _ = Nothing
    mbDml cls
      (InstanceD _ _ (AppT (AppT (AppT (AppT (AppT c t1) t2) t3) t4) (ConT t5)) _)
      | c == cls && t1 == ConT db && t2 == ConT sch = Just (t3,t4,t5)
    mbDml _ _ = Nothing
    mbTrec cls (InstanceD _ _ (AppT c (ConT t1)) _) | c == cls = Just t1
    mbTrec _ _                                      = Nothing
    mbRecLens cls (InstanceD _ _ (AppT (AppT c t1) (ConT t2)) _)
      | c == cls = Just (t1,t2)
    mbRecLens _ _ = Nothing

-- trace ("mkViewM: " ++ pprint tn ++ ", " ++ pprint par ++ ", " ++ pprint rc) $
isFld :: Type -> Bool
isFld = \case
  (AppT ListT (ConT _))  -> False
  _                      -> True

mkRecs :: TypeQ -> DecsQ
mkRecs tq = fmap fromPromotedList tq >>= fmap concat . mapM mkRec
  where
    mkRec (ConT rc) = do
      flds <- recToFlds rc

      let (fs, _) = partition (isFld . snd) flds
          rcQ = conT rc
      (++) <$>
        [d|
          instance Rec $(rcQ) where
            type TRec $(rcQ) = $(return (toPromotedList $ map toPromotedPair fs))
          |] <*>
        (concat <$> mapM (\(s,t) ->
            [d| instance RecLens $(return s) $(rcQ) where
                  type TLens $(return s) $(rcQ) = $(return t)
                  recLens = field @($(return s))
              |]
          ) flds)
    mkRec x = error $ "Invalid type " ++ pprint x ++ " in mkRecs"

mkViewM :: Type -> Type -> Name -> MkViewMonad ()
mkViewM tn par rc = do
  flds <- lift $ recToFlds rc
  let (fs, cs) = partition (isFld . snd) flds

  (VE db sch rls) <- ask
  mapM_ (\(LitT (StrTyLit s),AppT ListT (ConT t)) -> maybe
      ( error $ "There is no relation who corresponding to the field '"
            ++ s ++ "'"
      )
      (\rd -> do  chPar <- lift $ getPar rd
                  mkViewM (strToSym $ T.unpack $ rdFrom rd) chPar t)
      $ M.lookup (T.pack s) rls
    ) cs

  let [dbQ, schQ, rcQ] = map conT [db,sch,rc]
  vs <- get
  when (rc `S.notMember` vsRec vs) $ lift [d|
    instance Rec $(rcQ) where
      type TRec $(rcQ) = $(return (toPromotedList $ map toPromotedPair fs))
    |] >>= tell >> modify (\s -> s {vsRec = rc `S.insert` vsRec s})
  when (rc `S.notMember` vsRecDef vs) $ lift [d|
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
    |] >>= tell >> modify (\s -> s {vsRecDef = rc `S.insert` vsRecDef s})
  mapM_ (\(s,t) ->
    when ((s,rc) `S.notMember` vsRecLens vs) $
      -- TH не работает с DuplicateRecordFields. Через generics - работает.
      -- Ambiguous record updates not (yet) handled by Template Haskell
      -- Пока так...
      lift [d| instance RecLens $(return s) $(rcQ) where
                type TLens $(return s) $(rcQ) = $(return t)
                recLens = field @($(return s))
             |] >>= tell
                >> modify (\x -> x {vsRecLens = (s,rc) `S.insert` vsRecLens x})
    ) flds
  when ((tn,par,rc) `S.notMember` vsDml vs) $ lift [d|
    instance DML $(dbQ) $(schQ) $(return tn) $(return par) $(rcQ)
    |] >>= tell >> modify (\x -> x {vsDml = (tn,par,rc) `S.insert` vsDml x})

  where
    getListType (AppT ListT (ConT t)) = t
    getListType _                     = error "Error in mkViewM.getListType"

    expLstFldDbDef dbQ = listE . map getFld
      where
        getFld (s,t) = [e| fldDbDef @($dbQ) @($(return s)) @($(return t)) |]

    expLstToDb dbQ rcQ = listE . map expFldToDb
      where
        expFldToDb (s,t) = [e| fldToDb @($dbQ) @($(return s)) @($(return t))
                             . (^. $(rl)) |]
           where
     -- через quotation не получается, похоже на баг (из-за type appl, ghc-8.2)
             rl = appTypeE (appTypeE [e| recLens |] (return s)) rcQ
    expFromDb db sch flds =
      case map (\(s,t) -> if isFld t
                  then [e| fldFromDb @($(conT db)) @($(return s)) @($(return t)) |]
                  else [e| pure [] |]
               ) flds of
        (x:xs) -> foldl (\b a -> [e| (<*>) $(b) $(a) |])
                        [e| $(conByType rc) <$> $(x)|] xs
        _ -> fail $ "Error for fldFromDb for " ++ pprint db ++ ", "
                  ++ pprint sch ++ ", " ++ pprint rc
    --
    -- зная тип дочернего поля и RelDef получить тип
    -- Tagged <имена полей FK> <2-Tuple соответствующих типов полей дочерней записи>
    getPar rd = do
      mbTabRec <- lookupTypeName $ T.unpack $ rdFrom rd
      maybe (error $ "Couldn't find type with name" ++ T.unpack (rdFrom rd))
        (\t -> do
          flds <- recToFlds t
          [t|Tagged $(return tagType) $(return $ tup flds)|]
        )
        mbTabRec
      where
        tagType = toPromotedList $ map (strToSym . T.unpack . fst) $ rdCols rd
        tup fs = tupTypes [ft | rfn <- map fst $ rdCols rd
                              , (LitT (StrTyLit fn),ft) <- fs
                              , T.pack fn == rfn]

    tupTypes []     = TupleT 0
    tupTypes [x]    = x
    tupTypes (x:xs) = AppT (AppT (TupleT 2) x) (tupTypes xs)
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
--
fromPromotedList :: Type -> [Type]
fromPromotedList = \case
  PromotedNilT -> []
  AppT (AppT PromotedConsT x) xs -> x : fromPromotedList xs
  _ -> error "Invalid promoted list"

