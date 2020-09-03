module LateFirefly.Index
  -- ( IndexState(..)
  -- , indexWidget
  -- )
where

import Control.Lens hiding ((#))
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Maybe
import Data.Constraint
import Data.Text as T
import LateFirefly.Prelude
import LateFirefly.Router
import LateFirefly.Parser
-- import LateFirefly.Series
import Lucid as H

data HomeR = HomeR
  deriving stock (Eq, Ord, Generic)

data HomeData = HomeData
  deriving stock (Eq, Ord, Generic)

instance HasParser Url HomeR where
  parser = dimap (const ()) (const HomeR) $ segments ["home"]

instance IsPage HomeR where
  type PageData HomeR = HomeData
  pageInit _ = pure HomeData
  pageWidget _ = do
    div_ do -- ! "class":="home" do
      div_ do -- ! "class":="home-wrapper" do
        h1_ do "Telikov.Net"
