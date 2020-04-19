{-# OPTIONS_GHC -fno-warn-orphans #-}
module LF.Orphans where

import Data.Aeson as AE
import Data.Time.Calendar
import Data.Time.Clock
import Data.Typeable
import Data.UUID.Types.Internal (UUID(..))
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Flat.Rpc as FL (Flat(..))
import GHC.Generics (Generic)
import {-# SOURCE #-} LF.DB.Base

instance Flat a => Flat (Only a)

deriving instance Generic (a :. b)
instance (Flat a, Flat b) => Flat (a :. b)

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
