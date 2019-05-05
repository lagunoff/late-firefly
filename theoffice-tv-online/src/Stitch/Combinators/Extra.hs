{-# LANGUAGE FlexibleInstances, GADTs #-}
module Stitch.Combinators.Extra where

import Control.Monad.Trans.Stitch
import Stitch.Types
import Control.Monad.Trans.Writer.Strict
import qualified Data.Map as Map
import Data.String

(?) :: Monad m => String -> StitchT m a -> StitchT m a
sel ? (StitchT x) = StitchT $ censor (\(Block is ps cs) -> Block is [] (Children $ Map.singleton (fromString sel) (InnerBlock ps cs))) x
infixr 6 ?
