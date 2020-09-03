module LateFirefly.DB.QQ where

import Control.Applicative
import Control.Lens
import Data.Attoparsec.Combinator as AP
import Data.Attoparsec.Text as AP
import Data.Functor
import Data.Map as M
import Data.Maybe
import Data.String
import Data.Text as T
import Database.SQLite.Simple as S
import Database.SQLite.Simple.ToField as S
import Database.SQLite.Simple.Types as S
import Database.SQLite3
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote

data Sql = Sql Text [SQLData] (Map Text SQLData)
  deriving (Show, Eq)

sql :: QuasiQuoter
sql = QuasiQuoter{..} where
  quotePat = error "sql: patterns unsupported"
  quoteType = error "sql: types unsupported"
  quoteDec = error "sql: declarations unsupported"
  quoteExp e = do
    let Right chunks = parseOnly pSql (T.pack e)
    let
      sqlTxt = chunks <&> \case
        SqlRaw t   -> litE $ StringL $ T.unpack t
        SqlParam t -> litE $ StringL "?"
        SqlUnsafe t -> pure $ either error id $ parseExp $ T.unpack t
        SqlEscaped t -> [|esc $(pure $ either error id $ parseExp $ T.unpack t)|]
      sqlPar = catMaybes $ chunks <&> \case
        SqlParam t -> Just [|S.toField $(pure $ either error id $ parseExp $ T.unpack t)|]
        _          -> Nothing
    [|Sql (T.intercalate "" $ $(listE sqlTxt)) $(listE sqlPar) M.empty|]

instance IsString Sql where
  fromString s = Sql (T.pack s) mempty mempty

instance Semigroup Sql where
  (<>) (Sql s1 p1 n1) (Sql s2 p2 n2) = Sql (s1 <> s2) (p1 <> p2) (n1 <> n2)

pParam :: Parser Text
pParam = char '{' *> AP.takeWhile (/='}') <* char '}'

pInterpol :: Parser Text
pInterpol = string "#{" *> AP.takeWhile (/='}') <* string "}"

pRaw :: Parser Text
pRaw = fmap T.pack $ AP.manyTill anyChar $ lookAhead p where
  p = void (string "#{") <|> void (char '{') <|> endOfInput

pEsc :: Parser Text
pEsc = string "{{" *> AP.takeWhile (/='}') <* string "}}"

data SqlChunk = SqlRaw Text | SqlParam Text | SqlUnsafe Text | SqlEscaped Text
  deriving (Show, Eq)

pSql :: Parser [SqlChunk]
pSql = manyTill p endOfInput where
  p = fmap SqlUnsafe pInterpol
     <|> fmap SqlEscaped pEsc <|> fmap SqlParam pParam <|> fmap SqlRaw pRaw

esc :: Text -> Text
esc t = "`" <> t <> "`"
