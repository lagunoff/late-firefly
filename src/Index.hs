module Index where

import Control.Lens hiding ((#))

import "this" Intro
import "this" Router
import "this" Parser

data HomeR = HomeR
  deriving stock (Eq, Ord, Generic)

data HomeD = HomeD
  deriving stock (Eq, Ord, Generic)

instance IsPage HomeR HomeD where
  pageRoute = dimap (const ()) (const HomeR) $ segments ["home"]
  pageInit _ = pure HomeD
  pageWidget _ = do
    div_ do -- ! "class":="home" do
      div_ do -- ! "class":="home-wrapper" do
        h1_ do "Telikov.Net"
