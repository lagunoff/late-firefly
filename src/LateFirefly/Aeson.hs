{-# LANGUAGE CPP #-}
module LateFirefly.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  ) where

#ifndef __GHCJS__
import Data.Aeson
#else
data Value
class FromJSON a where
class ToJSON a where
#endif
