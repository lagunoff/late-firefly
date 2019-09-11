{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes            #-}
module Telikov.Header where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Massaraksh.Html
import Text.Lucius (lucius, renderCss)
import Telikov.Styles (Theme(..), unit, theme)
import Parser.TheOffice.Db (Season(..), Episode(..))

data Model = Model
  { modelFocused :: Bool -- ^ Search input has focus
  , modelSearchResults :: [SearchResult] -- ^ Search input has focus
  }

data SearchResult
  = ResultSeason Season
  | ResultEpisode Episode

view :: Html' msg model
view =
  div_ [ cs "root" ]
  [ div_ [ cs "logo" ] [ a_ [ href_ "" ] [ text_ "Telikov.com" ] ]
  , div_ [ cs "searchbar" ]
    [ input_ [ placeholder_ "Search" ]
    , button_ [ type_ "button" ] [ text_ "Go" ]
    ]
  , div_ [ cs "navbar" ]
    [ ul_ []
      [ li_ [] [ a_ [ href_ "" ] [ text_ "Home" ] ]
      , li_ [] [ a_ [ href_ "" ] [ text_ "Seasons" ] ]
      ]
    ]
  , element "style" [ type_ "text/css" ] [ text_ styles ]
  ]
 
styles = L.toStrict $ renderCss $ css () where
  Theme { unit, borderColor, bodyPadding, secondaryText } = theme
  css =
    [lucius|
      .#{prefix}-root {
        width: 100%;
        position: relative;
      }
      
      .#{prefix}-logo {
        position: absolute;
        left: #{bodyPadding}px;
        top: #{unit * 1.5}px;
        font-size: 22px;
        color: #{borderColor};
        a { color: #{borderColor}; text-decoration: none; &:hover { color: #{secondaryText} } }
      }
      
      .#{prefix}-searchbar {
        border-bottom: solid 1px #{borderColor};
        display: flex;
        padding: #{unit}px 0;
        justify-content: center;
        input {
          height: #{unit * 3}px;
          width: 500px;
          font-size: 18px;
          padding: 0 #{unit}px;
        }
      }

      .#{prefix}-navbar {
        padding: #{unit}px 0;
        display: flex;
        height: #{unit * 6}px;
        align-items: center;
        justify-content: center;
        ul { display: flex; flex-direction: row; }
        li { display: flex; }
      }
    |]

prefix = T.pack "header"
cs = class_ . (prefix <>) . (T.pack "-" <>) . T.pack
