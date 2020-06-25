{-# OPTIONS_GHC -fno-warn-orphans #-}
module LateFirefly.Orphans where

import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID.Types.Internal (UUID(..))
import Database.SQLite.Simple
import Flat as FL
import GHC.Generics (Generic)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax

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

instance Flat Name
instance Flat OccName
instance Flat NameFlavour
instance Flat ModName
instance Flat NameSpace
instance Flat PkgName
