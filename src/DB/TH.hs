module DB.TH
  ( DeriveDbConfig(..)
  , deriveDbDef
  , deriveDb
  , deriveRow
  , deriveRowDef
  , deriveDbPrio
  , collectTables
  , ColumnStrategy(..)
  , def
  , underscore
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Default
import Data.List as L
import Data.String as S
import Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude as P
import qualified Text.Inflections as Inf

import "this" DB.Base
import "this" Intro
import "this" DB.QQ

data DeriveDbConfig = DeriveDbConfig
  { tableName :: Maybe String
  , fields    :: [(String, ColumnStrategy)]
  , renameField :: String -> String
  , pkeys      :: [String]
  , ukeys      :: [[String]]
  , prio       :: Int }
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
  _ -> error "deriveDb: unsupported data declaration"

deriveCols :: Name -> DeriveDbConfig -> Q [Dec]
deriveCols n cfg@DeriveDbConfig{} = reify n >>= \case
  TyConI (DataD _ _ _ _ [RecC name vars] _) -> do
    let columnsE = deriveColumns cfg n
    let columnsD = funD 'columnsInfo [clause [] (normalB columnsE) []]
    dbTableInst <- instanceD (pure []) (conT ''DbColumns `appT` conT n) [columnsD]
    pure $ [dbTableInst]
  _ -> error "deriveDb: unsupported data declaration"

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
  _ -> error "deriveRow: unsupported data declaration"

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
  _ -> error "deriveColumns: unsupported data declaration"

deriveDbDef :: Name -> Q [Dec]
deriveDbDef = flip deriveDb def

deriveRowDef :: Name -> Q [Dec]
deriveRowDef = flip deriveRow def

deriveDbPrio :: Int -> Name -> Q [Dec]
deriveDbPrio = flip deriveDb . DeriveDbConfig Nothing [] id def def

-- | Make expression of type [Query] applying 'createTableStmt' to all
-- the instances of typeclass 'DbTable'
collectTables :: Q Exp
collectTables = reify ''DbTable >>= \case
  ClassI _ instances -> do
    ins <- catMaybes <$> forM instances \case
      InstanceD _ _ (AppT _ insTy) _ -> do
        pure $ Just $ AppTypeE (VarE 'mkSetupPrio) insTy
      _                              -> pure Nothing
    [|joinSetupPrio $(pure (ListE ins)) <> staticSchema|]
  _ -> error "impossible"

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

staticSchema = [imdbTitleSearch]
imdbTitleSearch = [sql|
  create virtual table if not exists imdb_title_fts using fts5(original_title_text, content='imdb_title');
  create unique index imdb_title__url_slug on imdb_title(url_slug);
  create index title_episode_tsv__parent_id on title_episode_tsv(parent_id);
|]
