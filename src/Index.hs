module Index where

import Control.Lens hiding ((#))

import "this" Intro
import "this" Router
import "this" Parser

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
