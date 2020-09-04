module Template where

import Control.Lens hiding ((#))
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Maybe
import Data.Constraint
import Data.Text as T
import Language.Javascript.JSaddle
import "this" Intro
import "this" Router
import "this" Widget.Prelude
import "this" Icons

htmlTemplate :: Html () -> Html ()
htmlTemplate content = do
  let Theme{..} = theme
  headerWidget
  divClass "root" do
    div_ content
  footerWidget
  [style|
    html, body, body *
      font-family: Arial, sans-serif
    .root
      padding-bottom: calc(150px + #{unit * 6})
    html
      height:100%
    body
      margin: 0
      min-height:100%
      padding:0
      margin:0
      position:relative
    body a
      color: #{primaryText}
      text-decoration: none
      &:hover
        color: #{primary}
  |]

headerWidget :: Html ()
headerWidget = do
  let Theme{..} = theme
  divClass "header" do
    divClass "header-wrapper" do
      divClass "header-left" do
        linkTo HomeR_ do
          "className" =:"home-link"
          span_ "Telikov."
          span_ do
            "Net"
            "style" =: [st|color: #{showt primary}|]
        ulClass "menu" do
          li_ do linkTo (SeriesR_ (SeriesRoute "theoffice")) do div_ "Series"
          li_ do linkTo (SeriesR_ (SeriesRoute "theoffice")) do div_ "Movies"
          li_ do linkTo (SeriesR_ (SeriesRoute "theoffice")) do div_ "Genre"
          li_ do linkTo (SeriesR_ (SeriesRoute "theoffice")) do div_ "Top IMDB"
          li_ do linkTo (SeriesR_ (SeriesRoute "theoffice")) do div_ "A-Z List"
      divClass "search" do
        input_ do
          "placeholder" =: "Search"
        searchIcon ("className" =: "search")
    [style|
      body
        background: rgba(0,0,0,0.04)
      .header
        background: white
        height: #{unit * 7}
        width: 100%
        box-shadow: 0 0 16px rgba(0,0,0,0.1)
        z-index: 2
        position: relative
        padding: 0 #{unit * 3} 0 #{unit * 3}
        box-sizing: border-box
        .header-wrapper
          height: 100%
          display: flex
          align-items: center
          justify-content: space-between
          margin: 0 auto
          max-width: #{pageWidth}
        .header-left
          height: 100%
          display: flex
          align-items: center
        .home-link
          font-size: 33px
          color: rgba(0,0,0,0.9)
          display: block
          height: 100%
          text-decoration: none
          display: flex
          align-items: center
          padding: 0 #{unit} 0 #{unit}
          margin-left: -#{unit}
          &:hover
            background: black
            color: white
        .menu
          display: flex
          align-items: center
          margin: 0 0 0 #{unit * 2}
          padding: 0
          height: 100%
          li
            list-style: none
            margin: 0
            height: 100%
          a
            padding: 0 #{unit}
            display: flex
            align-items: center
            color: #{primaryText}
            text-decoration: none
            text-transform: uppercase
            font-size: 13px
            height: 100%
            &:hover
              color: #{primary}
        .search
          padding: 0 #{unit * 2}
          height: 100%
          display: flex
          align-items: center
          height: #{(unit * 4) - 2}
          box-sizing: border-box
          border-radius: #{unit * 2}
          border: solid 2px transparent
          box-shadow: 0 0 2px rgba(0,0,0,0.4)
          input
            padding-left: #{unit}
            border: none
            outline: none
            font-size: 16px
            background: transparent
            &:focus
              width: 400px
          svg
            opacity: 0.2
          &:hover
            border: solid 2px #{primary}
            box-shadow: none
            svg
              opacity: 1 |]

footerWidget :: Html ()
footerWidget = do
  let Theme{..} = theme
  divClass "footer" do
    divClass "footer-wrapper" do
      div_ do
        ulClass "menu" do
          li_ do linkTo (SeriesR_ (SeriesRoute "theoffice")) do div_ "Series"
          li_ do linkTo (SeriesR_ (SeriesRoute "theoffice")) do div_ "Movies"
          li_ do linkTo (SeriesR_ (SeriesRoute "theoffice")) do div_ "Genre"
          li_ do linkTo (SeriesR_ (SeriesRoute "theoffice")) do div_ "Top IMDB"
          li_ do linkTo (SeriesR_ (SeriesRoute "theoffice")) do div_ "A-Z List"
      div_ do
        linkTo HomeR_ do
          "className" =:"home-link"
          span_ "Telikov."
          span_ do
            "Net"
            "style" =: [st|color: #{showt primary}|]
  [style|
    .footer
      margin-top: #{unit * 8}
      height: 150px
      background: rgba(0,0,0,0.9)
      color: rgba(255,255,255,0.87)
      position:absolute
      bottom:0
      width:100%
      a
        color: rgba(255,255,255,0.87)
      .footer-wrapper
        display: flex
        align-items: top
        justify-content: space-between
        margin: 0 auto
        max-width: #{pageWidth}
      .home-link
        font-size: 33px
        display: block
        text-decoration: none
        display: flex
        align-items: center
        margin-top: #{unit * 3}
        color: rgba(255,255,255,0.87)
        &:hover
          color: rgba(255,255,255,0.87)
      .menu
        padding: 0
        margin: #{unit * 3} 0 0 0
        li
          list-style: none |]

page404 :: Html ()
page404 = htmlTemplate do
  h2_ "Not Found"
