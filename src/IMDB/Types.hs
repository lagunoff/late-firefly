module IMDB.Types where

import Data.Aeson hiding (Series)
import Data.Text as T
import GHC.TypeLits
import Text.Read
import Database.SQLite.Simple.Internal (Field(..))

import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.FromField as S

import "this" DB
import "this" Intro

newtype ImdbId (t::Symbol) = ImdbId
  {unImdbId :: Int64}
  deriving stock (Show, Eq, Generic)

instance KnownSymbol t => FromJSON (ImdbId t) where
  parseJSON = withText "ImdbId"
    $ either fail pure . imdbFromText @t

instance KnownSymbol t => ToJSON (ImdbId t) where
  toJSON = toJSON . toText . showb

instance KnownSymbol t => TextShow (ImdbId t) where
  showb (ImdbId i) = fromString $ symbolVal (Proxy @t) <> lpad (show i) where
    lpad x = Prelude.replicate (7 - Prelude.length x) '0' ++ x

imdbFromText :: forall t. KnownSymbol t => Text -> Either String (ImdbId t)
imdbFromText t = do
  let pre = T.pack $ symbolVal (Proxy @t)
  let woPreMay = T.stripPrefix pre t
  woPre <- maybe (Left $ "prefix " <> show pre <> " not found") pure woPreMay
  intId <- maybe (Left $ "cannot read Int") pure $ readMaybe @Int64 (T.unpack woPre)
  pure (ImdbId intId)

instance KnownSymbol t => FromField (ImdbId t) where
  fromField = \case
    Field (S.SQLInteger i) _ -> Ok $ ImdbId i
    f                        -> S.returnError S.ConversionFailed f "need an int"

instance KnownSymbol t => ToField (ImdbId t) where
  toField = \case
    ImdbId i    -> S.SQLInteger i

instance KnownSymbol t => DbField (ImdbId t) where
  columnInfo _ = ColumnInfo IntegerColumn False False Nothing
