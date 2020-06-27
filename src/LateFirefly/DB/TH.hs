module LateFirefly.DB.TH
  ( DeriveDbConfig(..)
  , deriveDb'
  , deriveDb
  , deriveUUID
  , deriveDbUUID
  , deriveDbPrio
  , mkDatabaseSetup
  ) where

import Control.Applicative
import Control.Lens
import Data.String as S
import Data.List as L
import Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Flat as FL
import LateFirefly.DB.Base
import LateFirefly.Prelude
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude as P
import Text.Inflections

data DeriveDbConfig = DeriveDbConfig
  { tableName :: Maybe Text
  , primary   :: Maybe Text
  , prio      :: Int
  } deriving (Generic)

deriveDb' :: DeriveDbConfig -> Name -> Q [Dec]
deriveDb' DeriveDbConfig{..} n = do
  info <- reify n
  let
    occName (Name occ _) = coerce @_ @String occ
    varName (Name occ _, _, _) = coerce @_ @String occ
    varType (_, _, ty) = ty
  case info of
    TyConI (DataD _ _ _ _ [RecC name vars] _) -> do
      patNames <- forM vars \_ -> newName "a"
      vName <- newName "v"
      let
        priFld = flip L.find vars \case
          (name, _, AppT (ConT idCon) (ConT conTy)) ->
            (idCon == ''Id || idCon == ''UUID5) && conTy == n
          _                                         -> False
        verFld = L.find ((=="version") . varName) vars
        tblName = textE $ T.unpack $ flip fromMaybe tableName
          $ fromRight (T.pack (occName name))
          $ underscore <$> parseCamelCase [] (T.pack (occName name))
        textE s = VarE 'T.pack `AppE` LitE (StringL s)
        infixMap l r = InfixE (Just l) (VarE '(<$>)) (Just r)
        infixAp l r = InfixE (Just l) (VarE '(<*>)) (Just r)
        dbTableInst = InstanceD Nothing [] (ConT ''DbTable `AppT` ConT n) [hasVersionD, tableDescD, pkInfoD]
        columns = vars <&> \v -> TupE [AppE (VarE 'recordFieldToDb) (textE (varName v)), (VarE 'columnInfo `AppE` AppTypeE (ConE 'Proxy) (varType v))]
        tableDescE = ConE 'TableInfo `AppE` tblName `AppE` primaryListE `AppE` ListE columns `AppE` LitE (IntegerL (fromIntegral prio))
        tableDescD = FunD 'tableInfo [Clause [] (NormalB tableDescE) []]
        hasVersionD = TySynInstD ''HasVersion (TySynEqn [(ConT n)] (PromotedT (bool 'False 'True (isJust verFld))))
        pkInfoD = FunD 'pkInfo [Clause [] (NormalB pkInfoE) []]
        pkInfoE = ConE $ bool 'NoVersion 'HasVersion (isJust verFld)
        fromRowInst = InstanceD Nothing [] (ConT ''FromRow `AppT` ConT n) [fromRowD]
        fromRowD = FunD 'fromRow [Clause [] (NormalB fromRowE) []]
        fields = fmap (const (VarE 'field)) vars
        fromRowE = let (x:xs) = fields in P.foldl infixAp (infixMap (ConE name) x) xs
        toRowInst = InstanceD Nothing [] (ConT ''ToRow `AppT` ConT n) [toRowD]
        toRowD = FunD 'toRow [Clause [ConP name (VarP <$> patNames)] (NormalB toRowE) []]
        toFields = fmap ((VarE 'toField `AppE`) . VarE) patNames
        toRowE = ListE toFields
        primaryE = textE $ maybe "rowid" varName priFld
        primaryListE = ListE $ [primaryE] <> maybe [] (pure . textE . varName) verFld
      pure [fromRowInst, toRowInst, dbTableInst]
    _ -> do
      [] <$ reportError "deriveDb: unsupported data declaration"

deriveDb :: Name -> Q [Dec]
deriveDb = deriveDb' (DeriveDbConfig Nothing Nothing 0)

deriveDbPrio :: Int -> Name -> Q [Dec]
deriveDbPrio = deriveDb' . DeriveDbConfig Nothing Nothing

deriveUUID :: [String] -> Name -> Q [Dec]
deriveUUID flds tcName = reify tcName >>= \case
  TyConI (DataD _ _ _ _ [RecC dcName vars] _) -> do
    let occName (Name occ _) = coerce @_ @String occ
    patNames <- forM vars \(n, _, _) ->
      if L.any (==occName n) flds
      then fmap Just (newName "a") else pure Nothing
    tcNameBS <- [|FL.flat $(lift tcName)|]
    let
      instD = InstanceD Nothing [] (ConT ''DeriveUUID `AppT` ConT tcName) [uuidSaltD]
      uuidSaltD = FunD 'uuidSalt [Clause [ConP dcName (fmap (maybe WildP VarP) patNames)] (NormalB uuidSaltE) []]
      uuidSaltE = VarE 'foldMap `AppE` (VarE 'id) `AppE` ListE ([tcNameBS] <> (fmap (AppE (VarE 'FL.flat) . VarE) (catMaybes patNames)))
    pure [instD]
  _ -> do
    [] <$ reportError "deriveUUID: unsupported data declaration"

deriveDbUUID :: [String] -> Name -> Q [Dec]
deriveDbUUID flds tcName =
  liftA2 (<>) (deriveDb tcName) (deriveUUID flds tcName)

recordFieldToDb :: Text -> Text
recordFieldToDb n = fromRight n $ parseCamelCase [] n <&> underscore

-- | Make expression of type [Query] applying 'createTableStmt' to all
-- the instances of typeclass 'DbTable'
mkDatabaseSetup :: Q Exp
mkDatabaseSetup = do
  ClassI _ instances <- reify ''DbTable
  ins <- catMaybes <$> forM instances \case
    InstanceD _ _ (AppT _ insTy) _ -> do
      pure $ Just $ AppTypeE (VarE 'mkSetupPrio) insTy
    _                              -> pure Nothing
  [|joinSetupPrio $(pure (ListE ins))|]

mkSetupPrio :: forall t. DbTable t => (Int, [Query])
mkSetupPrio = (getField @"prio" ti, createTableStmt @t) where
  ti = tableInfo @t

joinSetupPrio :: [(Int, [Query])] -> [Query]
joinSetupPrio xs = L.foldl' (<>) [] (fmap snd (L.sortOn fst xs ))
