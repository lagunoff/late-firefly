module LF.DB.Transaction where

import LF.DB.Base
import LF.DB.TH
import LF.Prelude
import Flat.Rpc
import Data.Time

data Transaction = Transaction
  { rowid       :: Id Transaction
  , started_at  :: UTCTime
  , finished_at :: Maybe UTCTime }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDb ''Transaction
