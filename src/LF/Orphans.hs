{-# OPTIONS_GHC -fno-warn-orphans #-}
module LF.Orphans where

import Data.Aeson as AE
import Flat.Rpc as FL (Flat(..))
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics (Generic)
import Data.UUID.Types.Internal (UUID(..))
import {-# SOURCE #-} LF.DB.Base
import Data.Typeable
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

deriving instance Generic Day
instance Flat Day

deriving instance Generic UTCTime
instance Flat UTCTime

instance Flat DiffTime where
  encode = FL.encode . diffTimeToPicoseconds
  decode = picosecondsToDiffTime <$> FL.decode
  size = size . diffTimeToPicoseconds

deriving instance Generic UUID
instance Flat UUID

deriving via JsonField [a] instance (FromJSON a, Typeable a) => FromField [a]
deriving via JsonField [a] instance ToJSON a => ToField [a]
