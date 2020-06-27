{-# LANGUAGE CPP #-}
module LateFirefly.Backend where

import Flat as FL
import LateFirefly.Prelude
import Options.Generic

data WebOpts = WebOpts
  { webPort :: Int
  , dbPath  :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ParseRecord)

instance Default WebOpts where
  def = WebOpts 8080 "./late-firefly.sqlite"
