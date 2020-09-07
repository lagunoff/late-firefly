{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Index where

import "this" Intro
import "this" Router
import "this" Widget
import Lucid.Base

data HomeD = HomeD

instance IsPage "HomeR" HomeD where
  pageInit _ = pure HomeD
  pageWidget HomeR{} HomeD = do
    let Theme{..} = theme
    div_ [class_ "home"] do
      div_ [class_ "home-wrapper"] do
        h1_ do "Telikov.Net"
        form_ [action_ "/", method_ "GET"] do
          input_ [name_ "s", type_ "search", makeAttribute "autofocus" "on"]
    [style|
      .home
        padding: 0 #{unit * 2} 0 #{unit * 2}
        box-sizing: border-box
      .home-wrapper
        margin: 0 auto
        max-width: #{pageWidth}
        text-align: center
        padding: 160px 0 160px 0
        h1
          font-size: 60px
          font-weight: 400
        input[type=search]
          display: block
          width: 100%
          margin: 0 auto
          max-width: #{unit * 70}
          height: #{unit * 6}
          border-radius: 3px
          outline: none
          border: solid 1px #{borderColor}
          padding: 0px 8px
          font-size: 18px
          &:focus
            border: solid 2px #{primary}
    |]
