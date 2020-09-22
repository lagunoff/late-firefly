module Site.Template where

import "this" Intro
import "this" Widget
import "this" Icons

htmlTemplate :: Html () -> Html ()
htmlTemplate content = do
  let Theme{..} = theme
  headerWidget
  div_ [class_ "root"] do
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
  div_ [class_ "header"] do
    div_ [class_ "header-wrapper"] do
      div_ [class_ "header-left"] do
        linkAtt HomeR [class_ "home-link"] do
          span_ "Telikov."
          span_ [style_ [st|color: #{showt primary}|] ] do
            "Net"
        ul_ [class_ "menu"] do
          li_ do linkTo (HomeR) do div_ "Series"
          li_ do linkTo HomeR do div_ "Movies"
          li_ do linkTo HomeR do div_ "Genre"
          li_ do linkTo HomeR do div_ "Top IMDB"
          li_ do linkTo HomeR do div_ "A-Z List"
      div_ [class_ "search"] do
        form_ [action_ "/", method_ "GET"] do
          input_
            [name_ "s", placeholder_ "Search"]
        searchIcon_
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
  div_ [class_ "footer"] do
    div_ [class_ "footer-wrapper"] do
      div_ do
        ul_ [class_ "menu"] do
          li_ do linkTo HomeR do div_ "Series"
          li_ do linkTo HomeR do div_ "Movies"
          li_ do linkTo HomeR do div_ "Genre"
          li_ do linkTo HomeR do div_ "Top IMDB"
          li_ do linkTo HomeR do div_ "A-Z List"
      div_ do
        linkAtt HomeR [class_ "home-link"] do
          span_ "Telikov."
          span_ [style_ [st|color: #{showt primary}|] ] do
            "Net"
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
