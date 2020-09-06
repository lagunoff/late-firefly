{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

import Data.Aeson
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID.Types.Internal (UUID(..))
import Database.SQLite.Simple
import GHC.Generics (Generic)
import GHC.Stack.Types
import TextShow
import Text.Shakespeare.Text
import Data.Text.Lazy.Builder as T

deriving instance Generic (a :. b)

deriving instance Generic Day

deriving instance Generic UTCTime

deriving instance Generic UUID

deriving stock instance Generic CallStack

deriving stock instance Generic SrcLoc

deriving anyclass instance FromJSON CallStack
deriving anyclass instance FromJSON SrcLoc
deriving anyclass instance ToJSON CallStack
deriving anyclass instance ToJSON SrcLoc

newtype ViaTextShow a = ViaTextShow {unViaTextShow :: a}

instance TextShow a => ToText (ViaTextShow a) where
  toText = T.fromText . showt . unViaTextShow

deriving via ViaTextShow Double instance ToText Double
deriving via ViaTextShow Float instance ToText Float
