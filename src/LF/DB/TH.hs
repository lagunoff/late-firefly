module LF.DB.TH where

import Control.Lens
import Data.List as L
import Data.Text as T
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.ToRow
import Flat.Rpc
import LF.DB.Base
import LF.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude as P
import Text.Inflections

data DeriveDbConfig = DeriveDbConfig
  { tableName :: Maybe Text
  , primary   :: Maybe Text
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
      patNames <- mapM (\_ -> newName "a") vars
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
        dbTableInst = InstanceD Nothing [] (ConT ''DbTable `AppT` ConT n) [tableDescD]
        columns = vars <&> \v -> TupE [AppE (VarE 'recordFieldToDb) (textE (varName v)), (VarE 'fieldDesc `AppE` AppTypeE (ConE 'Proxy) (varType v))]
        tableDescE = ConE 'TableDesc `AppE` tblName `AppE` primaryListE `AppE` ListE columns
        tableDescD = FunD 'tableDesc [Clause [] (NormalB tableDescE) []]
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
deriveDb = deriveDb' (DeriveDbConfig Nothing Nothing)

recordFieldToDb :: Text -> Text
recordFieldToDb n = fromRight n $ parseCamelCase [] n <&> underscore
