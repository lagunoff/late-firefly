{-# OPTIONS_GHC -Wno-orphans #-}
module Index where

import "this" Intro
import "this" Router

data HomeD = HomeD

instance IsPage "HomeR" HomeD where
  pageInit _ = pure HomeD
  pageWidget HomeR{} HomeD = do
    div_ do -- ! "class":="home" do
      div_ do -- ! "class":="home-wrapper" do
        h1_ do "Telikov.Net"
