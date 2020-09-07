{-# OPTIONS_GHC -Wno-orphans #-}
module Router.TH where

import Control.Applicative
import Control.Monad
import Data.Constraint
import Data.Maybe
import Language.Haskell.TH
import Prelude as P

import "this" Router

collectPages :: Q Exp
collectPages = reify ''IsPage >>= \case
  ClassI _ instances -> do
    ins <- catMaybes <$> forM instances \case
      InstanceD _ _ insTy _ -> do
        pure $ Just $ appE [|PageDict|] $ appTypeE [|Dict|] $ pure insTy
      _                              -> pure Nothing
    listE ins
  _ -> error "impossible"
