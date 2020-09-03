{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LateFirefly.Orphans where

import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID.Types.Internal (UUID(..))
import Database.SQLite.Simple
import Flat as FL
import GHC.Generics (Generic)
import GHC.Stack.Types
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Data.Aeson

deriving anyclass instance Flat a => Flat (Only a)

deriving instance Generic (a :. b)
deriving anyclass instance (Flat a, Flat b) => Flat (a :. b)

deriving instance Generic Day
deriving anyclass instance Flat Day

deriving instance Generic UTCTime
deriving anyclass instance Flat UTCTime

instance Flat DiffTime where
  encode = FL.encode . diffTimeToPicoseconds
  decode = picosecondsToDiffTime <$> FL.decode
  size = size . diffTimeToPicoseconds

deriving instance Generic UUID
deriving anyclass instance Flat UUID

deriving anyclass instance Flat Name
deriving anyclass instance Flat OccName
deriving anyclass instance Flat NameFlavour
deriving anyclass instance Flat ModName
deriving anyclass instance Flat NameSpace
deriving anyclass instance Flat PkgName

deriving stock instance Generic CallStack
deriving anyclass instance Flat CallStack

deriving stock instance Generic SrcLoc
deriving anyclass instance Flat SrcLoc

deriving anyclass instance FromJSON CallStack
deriving anyclass instance FromJSON SrcLoc
deriving anyclass instance ToJSON CallStack
deriving anyclass instance ToJSON SrcLoc
