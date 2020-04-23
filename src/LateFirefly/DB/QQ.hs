module LateFirefly.DB.QQ where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Shakespeare.Text
import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.QQ as S

sql :: QuasiQuoter
sql = S.sql{quoteExp=qExp} where
  qExp = appE [|S.Query|] . quoteExp st
