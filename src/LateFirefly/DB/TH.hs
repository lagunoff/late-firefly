module LateFirefly.DB.TH
  ( DeriveDbConfig(..)
  , deriveDbDef
  , deriveDb
  , deriveRow
  , deriveRowDef
  , deriveDbPrio
  , mkDatabaseSetup
  , ColumnStrategy(..)
  , def
  , underscore
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.String as S
import Data.List as L
import Data.Text as T
import Data.Default
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Flat as FL
import LateFirefly.DB.Base
import LateFirefly.Prelude
import LateFirefly.DB.QQ
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude as P
import qualified Text.Inflections as Inf

data DeriveDbConfig = DeriveDbConfig
  { tableName :: Maybe String
  , fields    :: [(String, ColumnStrategy)]
  , renameField :: String -> String
  , pkeys      :: [String]
  , ukeys      :: [[String]]
  , prio      :: Int }
  deriving stock (Generic)

data ColumnStrategy
  = CstgDefault | CstgJson | CstgRow
  deriving stock (Eq, Show, Generic)

deriveDb :: Name -> DeriveDbConfig -> Q [Dec]
deriveDb n cfg@DeriveDbConfig{..} = reify n >>= \case
  TyConI (DataD _ _ _ _ [RecC name vars] _) -> do
    patNames <- forM vars \_ -> newName "a"
    vName <- newName "v"
    let textE (s::String) = [|T.pack $(lift s)|]
    let tblName = textE $ flip fromMaybe tableName $ underscore (nameBase name)
    let colInfo = [|columnsInfo|] `appTypeE` conT n
    let pks = bool (listE (fmap textE pkeys)) (listE [textE "rowid"]) (pkeys == [])
    let uks = listE (fmap (listE . fmap textE) ukeys)
    let tableDescE = [|TableInfo $(tblName) $(colInfo) $(pks) $(uks) $(lift prio)|]
    tableDescD <- tableDescE <&> \x -> FunD 'tableInfo [Clause [] (NormalB x) []]
    let dbTableInst = InstanceD Nothing [] (ConT ''DbTable `AppT` ConT n) [tableDescD]
    rowInsts <- deriveRow n cfg
    pure $ dbTableInst:rowInsts
  _ -> do
    [] <$ reportError "deriveDb: unsupported data declaration"

deriveCols :: Name -> DeriveDbConfig -> Q [Dec]
deriveCols n cfg@DeriveDbConfig{..} = reify n >>= \case
  TyConI (DataD _ _ _ _ [RecC name vars] _) -> do
    let columnsE = deriveColumns cfg n
    let columnsD = funD 'columnsInfo [clause [] (normalB columnsE) []]
    dbTableInst <- instanceD (pure []) (conT ''DbColumns `appT` conT n) [columnsD]
    pure $ [dbTableInst]
  _ -> do
    [] <$ reportError "deriveDb: unsupported data declaration"

deriveRow :: Name -> DeriveDbConfig -> Q [Dec]
deriveRow n cfg@DeriveDbConfig{..} = reify n >>= \case
  TyConI (DataD _ _ _ _ [RecC name vars] _) -> do
    patNames <- forM vars \_ -> newName "a"
    let infixMap l r = InfixE (Just l) (VarE '(<$>)) (Just r)
    let infixAp l r = InfixE (Just l) (VarE '(<*>)) (Just r)
    let
      fieldsE = vars <&> \(n,_,ty) -> case L.lookup (nameBase n) fields of
        Just CstgRow  -> case ty of
          AppT (ConT ((==''Maybe) -> True)) (ConT tyName) ->
            VarE 'fromRow
          ConT tyName -> VarE 'fromRow
          _           -> error "unsupported field type"
        Just CstgJson -> error "CstgJson: unimplemented"
        _             -> VarE 'field
    let fromRowE = let (x:xs) = fieldsE in P.foldl infixAp (infixMap (ConE name) x) xs
    let fromRowD = FunD 'fromRow [Clause [] (NormalB fromRowE) []]
    let fromRowInst = InstanceD Nothing [] (ConT ''FromRow `AppT` ConT n) [fromRowD]
    let
      toRowE = AppE (VarE 'join) $ ListE $ L.zip patNames vars <&> \(x, (n,_,ty)) -> case L.lookup (nameBase n) fields of
        Just CstgRow  -> case ty of
          AppT (ConT ((==''Maybe) -> True)) (ConT tyName) ->
            VarE 'toRow `AppE` VarE x
          ConT tyName -> VarE 'toRow `AppE` VarE x
          _           -> error "unsupported field type"
        Just CstgJson -> error "CstgJson: unimplemented"
        _             -> ListE [VarE 'toField `AppE` VarE x]
    let toRowD = FunD 'toRow [Clause [ConP name (VarP <$> patNames)] (NormalB toRowE) []]
    let toRowInst = InstanceD Nothing [] (ConT ''ToRow `AppT` ConT n) [toRowD]
    colsInst <- deriveCols n cfg
    pure $ [fromRowInst, toRowInst] <> colsInst
  _ -> do
    [] <$ reportError "deriveRow: unsupported data declaration"

deriveColumns :: DeriveDbConfig -> Name -> Q Exp
deriveColumns DeriveDbConfig{..} x = reify x >>= \case
  TyConI (DataD _ _ _ _ [RecC name vars] _) -> do
    let textE s = [|T.pack $(lift s)|]
    let
      columns = vars <&> \(n,_,ty) -> case L.lookup (nameBase n) fields of
        Just CstgRow  -> case ty of
          AppT (ConT ((==''Maybe) -> True)) (ConT tyName) -> do
            let cols = [|columnsInfo|] `appTypeE` conT tyName
            [|fmap (\(a, b) -> ($(lift (renameField (nameBase n))) <> "_" <> a, makeColumnNullable b)) $(cols)|]
          ConT tyName -> do
            let cols = [|columnsInfo|] `appTypeE` conT tyName
            [|fmap (\(a, b) -> ($(lift (renameField (nameBase n))) <> "_" <> a, b)) $(cols)|]
          _           -> error "unsupported field type"
        Just CstgJson -> error "CstgJson: unimplemented"
        _             -> [|[($(textE (renameField (nameBase n))), columnInfo $(appTypeE (conE 'Proxy) (pure ty)))]|]
    [|join $(listE columns)|]

deriveDbDef :: Name -> Q [Dec]
deriveDbDef = flip deriveDb def

deriveRowDef :: Name -> Q [Dec]
deriveRowDef = flip deriveRow def

deriveDbPrio :: Int -> Name -> Q [Dec]
deriveDbPrio = flip deriveDb . DeriveDbConfig Nothing [] id def def

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

mkSetupPrio :: forall t. DbTable t => (Int, [Sql])
mkSetupPrio = (getField @"prio" ti, createTableStmt @t) where
  ti = tableInfo @t

joinSetupPrio :: [(Int, [Sql])] -> [Sql]
joinSetupPrio xs = L.foldl' (<>) [] (fmap snd (L.sortOn fst xs ))

makeColumnNullable :: ColumnInfo -> ColumnInfo
makeColumnNullable ci = ci {nullable = True}

underscore :: String -> String
underscore n = fromRight n
  $ fmap (T.unpack . Inf.underscore)
  $ Inf.parseCamelCase [] (T.pack n)

instance Default DeriveDbConfig where
  def = DeriveDbConfig def def id def def def
