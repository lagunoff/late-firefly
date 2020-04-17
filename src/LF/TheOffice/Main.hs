module Main where

import Data.Text as T
import Data.Text.IO as T
import LF.DB
import LF.Prelude
import LF.TheOffice.Schema
import Options.Generic

-- | Cli arguments
data Args
  = Update {dbpath :: Maybe Text}
  | InitSchema {dbpath2 :: Text}
  | GoodBye
  deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
  options <- getRecord "Parser for https://iwatchtheoffice.com/"
  case options of
    Update{dbpath=mayDb,..} -> do
      let dbpath = maybe "./late-firefly.sqlite" T.unpack mayDb
      withConnection dbpath do
        let
          qs = createTableStmt @Transaction <> createTableStmt @Episode <> createTableStmt @Season
          insertQ1 = [sql|insert into "transaction"(started_at, finished_at) values (1, null)|]
          insertQ2 = [sql|insert into season(uuid, version, thumbnail, "number") values ("", 1, "", 1)|]
        forM qs (T.putStrLn . fromQuery)
        forM qs execute_
        mapM_ execute_ [insertQ1, insertQ2]
      T.putStrLn "Goodbye..."
    InitSchema{..} -> do
      T.putStrLn "Goodbye..."
    GoodBye ->
      T.putStrLn "Goodbye..."
