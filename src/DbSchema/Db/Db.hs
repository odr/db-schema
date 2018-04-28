{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module DbSchema.Db.Db (module DbSchema.Db.Db) where

import           Control.Monad.Catch        (MonadCatch, MonadMask)
import           Control.Monad.IO.Class     as DbSchema.Db.Db
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Kind                  (Constraint, Type)
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy                 (Proxy (..))
import           Data.Singletons.Prelude
import           Data.String                (IsString)
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           GHC.TypeLits               (KnownSymbol, symbolVal)

-- import           DbSchema.Def
import           DbSchema.Util.Format



data DelCons = DcRestrict | DcCascade | DcSetNull
  deriving (Show, Eq, Ord, Read)

type SessionMonad b m = ReaderT (Conn b) m

-- | Options for backend
class Db back where
  type FieldDB back
  type Conn back
  type SessionParams back
  type PrepCmd back
  type GenKey back         -- set to () if generation is impossible
  paramName :: Int -> Text -- ^ How to create param name (like "?1") from param num

  createTableText :: Text -> [(Text,(Text,Bool))] -> [Text] -> [[Text]] -> Text
  createTableText t fs pk = format "CREATE TABLE {} ({}, PRIMARY KEY ({}) {})"
                          . createTableParams @back t fs pk
  createTableParams :: Text -> [(Text,(Text,Bool))] -> [Text] -> [[Text]]
                    -> (Text,Text,Text,Text)
  createTableParams t fs pk uk =
    ( t, T.intercalate "," $ map rc fs, T.intercalate "," pk
    , foldMap (format ", UNIQUE ({})" . Only . T.intercalate ",") uk
    )
    where
      rc (n, (t,b)) = format "{} {} {} NULL" (n,t, if b then T.empty else "NOT")

  createRelText :: Text -> Text -> Text -> [(Text,Text)] -> Text
  createRelText c f t =
    format "ALTER TABLE {} ADD CONSTRAINT {} FOREIGN KEY ({}) REFERENCES {}({})"
    . createRelParams @back c f t
  createRelParams :: Text -> Text -> Text -> [(Text,Text)]
                  -> (Text, Text, Text, Text, Text)
  createRelParams name from to cols =
    ( from, name, T.intercalate "," $ map fst cols
    , to, T.intercalate "," $ map snd cols
    )

  runSession :: (MonadIO m, MonadCatch m)
          => SessionParams back -> SessionMonad back m a -> m a
  prepareCommand :: MonadIO m => Text -> SessionMonad back m (PrepCmd back)
  -- | Executed before runPrepared in insertAuto operation.
  --   We can get there new key and put it into monad.
  preRunInAuto :: MonadIO m => SessionMonad back m ()
  runPrepared :: MonadIO m
              => PrepCmd back -> [FieldDB back] -> SessionMonad back m ()
  runSelect :: MonadIO m => PrepCmd back -> [FieldDB back]
                         -> SessionMonad back m [[FieldDB back]]
  finalizePrepared :: MonadIO m => PrepCmd back -> SessionMonad back m ()
  getLastKey :: MonadIO m => SessionMonad back m (GenKey back)
  execCommand :: MonadIO m => Text -> SessionMonad back m ()
  execCommand t = runCommand @back t []
  deleteConstraintText :: DelCons -> Text
  deleteConstraintText DcRestrict = "ON DELETE RESTRICT"
  deleteConstraintText DcCascade  = "ON DELETE CASCADE"
  deleteConstraintText DcSetNull  = "ON DELETE SET NULL"

  runCommand :: MonadIO m => Text -> [FieldDB back] -> SessionMonad back m ()
  runCommand sql pars = do
    cmd <- prepareCommand @back sql
    runPrepared @back cmd pars

  condLike :: Text -> Text -> Text
  condLike name par
      = format "lower({}) LIKE '%' + lower({}) + '%'" (name, par)

-- instances should be implemented for concrete Db
type family DbTypeName b a :: Symbol

type family Nullable a :: (Type, Bool) where
  Nullable (Maybe x) = '(x, 'True)
  Nullable x = '(x, 'False)

type DbFldType b (a::Type)
    = If (Snd (Nullable a)) '(DbTypeName b (Fst (Nullable a)), 'True)
                            '(DbTypeName b a, 'False)

type family DbFldTypes b (a::[Type]) :: [(Symbol,Bool)] where
  DbFldTypes b '[] = '[]
  DbFldTypes b (a ': as) = DbFldType b a ': DbFldTypes b as


-- genDefunSymbols [''DbTypeName, ''Nullable]

-- type DbFieldTypes b a = DbFldTypes b (FldTypes NoLstFld a)

-- type DataStructDSCons b ds =
--   ( ConvNames NoLstFld (DsRec ds)
--   , CheckDataStruct ds ~ 'True
--   , Take 0 (DbFieldTypes b (DsRec ds)) ~ '[] -- проверка, что все поля конвертируются
--   )
--
-- genDefunSymbols [''DataStructDSCons]

-- type CheckDbSchema b (sch :: Schema)
--   = Constraints (Map (DataStructDSConsSym1 b) (SchDS sch))
--
-- type MonadCons m = (MonadIO m, MonadMask m)
--
-- newtype DBEnum (a :: [Symbol]) = DBEnum { getDBEnum :: Text }
--   deriving (Show, Eq, Ord, Generic, IsString)
--
-- instance (Convert a Text, SingI ss) => Convert a (DBEnum ss) where
--   convert = DBEnum . check . convert
--     where
--       ss = showProxy (Proxy :: Proxy ss)
--       check s
--         = fromMaybe (error $ "Invalid value '" ++ unpack s ++ "' for DBEnum" ++ show ss)
--         $ find (== s) ss
--
-- instance Convert Text a => Convert (DBEnum ss) a where
--   convert = convert . getDBEnum
--
-- type AppCons f = (Applicative f, Traversable f)
--
-- -----
-- type FldCons b v = ( Show v, FromJSON v, ToJSON v
--                    , DbOption b, Convert v [FieldDB b]
--                    )
--
-- data SomeFldType b where
--   SomeFldType :: FldCons b v => Proxy v -> SomeFldType b
--
-- class FldInfo a b where
--   fldInfo :: M.Map (Text, Text) (SomeFldType b)
--
-- instance (KnownSymbol x, KnownSymbol s, FldCons b v) => FldInfo '(x,'(s,v)) b where
--   fldInfo = M.singleton ( pack $ symbolVal (Proxy @x)
--                         , pack $ symbolVal (Proxy @s)
--                         )
--                         (SomeFldType (Proxy @v))
--
-- instance FldInfo '(tab,'[]) b where fldInfo = mempty
--
-- instance (FldInfo '(tab,x) b, FldInfo '(tab,xs) b) => FldInfo '(tab,x ': xs) b where
--   fldInfo = M.union (fldInfo @'(tab,x)) (fldInfo @'(tab,xs))
--
-- instance ( ConvNames NoLstFld (DsRec ds)
--          , FldInfo '(DdName t, FldNamesTypes NoLstFld (DsRec ds)) b
--          )
--       => FldInfo (ds :: DataStruct) b where
--   fldInfo = fldInfo @'(DdName t, FldNamesTypes NoLstFld (DsRec ds))
--
-- instance FldInfo '[] b where fldInfo = mempty
-- instance (FldInfo ds b, FldInfo dss b) => FldInfo (ds ': dss) b where
--   fldInfo = M.union (fldInfo @ds) (fldInfo @dss)
--
-- instance FldInfo (SchDS sch) b => FldInfo (sch :: Schema) b where
--   fldInfo = fldInfo @(SchDS sch)
