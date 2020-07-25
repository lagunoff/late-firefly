module LateFirefly.DB.TH
  ( DeriveDbConfig(..)
  , deriveDb'
  , deriveDb
  , deriveRow
  , deriveRow'
  , deriveDbPrio
  , mkDatabaseSetup
  , ColumnStrategy(..)
  , def
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
import Text.Inflections

data DeriveDbConfig = DeriveDbConfig
  { tableName :: Maybe String
  , fields    :: [(String, ColumnStrategy)]
  , prio      :: Int }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

data ColumnStrategy
  = CstgDefault | CstgJson | CstgRow
  deriving stock (Eq, Show, Generic)

deriveDb' :: Name -> DeriveDbConfig -> Q [Dec]
deriveDb' n cfg@DeriveDbConfig{..} = reify n >>= \case
  TyConI (DataD _ _ _ _ [RecC name vars] _) -> do
    patNames <- forM vars \_ -> newName "a"
    vName <- newName "v"
    let textE s = [|T.pack $(lift s)|]
    let
      tblName = textE $ flip fromMaybe tableName
        $ fromRight (nameBase name)
        $ fmap T.unpack
        $ underscore
        <$> parseCamelCase [] (T.pack (nameBase name))
    let columns = deriveColumns cfg n
    let tableDescE = [|TableInfo $(tblName) $(columns) $(lift prio)|]
    tableDescD <- tableDescE <&> \x -> FunD 'tableInfo [Clause [] (NormalB x) []]
    let dbTableInst = InstanceD Nothing [] (ConT ''DbTable `AppT` ConT n) [tableDescD]
    rowInsts <- deriveRow' n cfg
    pure $ dbTableInst:rowInsts
  _ -> do
    [] <$ reportError "deriveDb: unsupported data declaration"

deriveRow' :: Name -> DeriveDbConfig -> Q [Dec]
deriveRow' n DeriveDbConfig{..} = reify n >>= \case
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
    pure [fromRowInst, toRowInst]
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
            let cols = deriveColumns def tyName
            [|fmap (\(a, b) -> ($(lift (nameBase n)) <> "_" <> a, makeColumnNullable b)) $(cols)|]
          ConT tyName -> do
            let cols = deriveColumns def tyName
            [|fmap (\(a, b) -> ($(lift (nameBase n)) <> "_" <> a, b)) $(cols)|]
          _           -> error "unsupported field type"
        Just CstgJson -> error "CstgJson: unimplemented"
        _             -> [|[($(textE (nameBase n)), columnInfo $(appTypeE (conE 'Proxy) (pure ty)))]|]
    [|join $(listE columns)|]

deriveDb :: Name -> Q [Dec]
deriveDb = flip deriveDb' def

deriveRow :: Name -> Q [Dec]
deriveRow = flip deriveRow' def

deriveDbPrio :: Int -> Name -> Q [Dec]
deriveDbPrio = flip deriveDb' . DeriveDbConfig Nothing []

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
