module LF.DB.Base where

import Data.Aeson as AE
import Data.Typeable as T
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

newtype JsonField a = JsonField {unJsonField :: a}

instance ToJSON a => ToField (JsonField a)
instance (FromJSON a, Typeable a) => FromField (JsonField a)

newtype ReadShowField a = ReadShowField {unReadShowField :: a}

instance Show a => ToField (ReadShowField a)
instance (Read a, Typeable a) => FromField (ReadShowField a)
