{-# LANGUAGE CPP #-}
module LateFirefly.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , defaultOptions
  , deriveJSON
  , Options(..)
  ) where

#ifndef __GHCJS__
import Data.Aeson
import Data.Aeson.TH
#else
import Language.Haskell.TH

data Value
class FromJSON a where
class ToJSON a where

data Options = Options
    { fieldLabelModifier :: String -> String
      -- ^ Function applied to field labels.
      -- Handy for removing common record prefixes for example.
    }
deriveJSON :: Options
           -- ^ Encoding options.
           -> Name
           -- ^ Name of the type for which to generate 'ToJSON' and 'FromJSON'
           -- instances.
           -> Q [Dec]
deriveJSON _ _ = pure []

defaultOptions = Options id
#endif
