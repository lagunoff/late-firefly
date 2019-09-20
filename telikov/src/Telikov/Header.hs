{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings, ScopedTypeVariables                #-}
module Telikov.Header where

import Massaraksh.Html
import Massaraksh
import Parser.TheOffice.Db (Episode (..), Season (..))
import Telikov.Styles (Theme (..), theme, unit)
import Text.Lucius (lucius, renderCss)
import Polysemy.State (modify, State)
import Telikov.Effects (httpGet, Http, Eff, Member)
import Control.Lens hiding (element, view)
import Data.JSString as JS
import qualified Data.JSString.Text as JS
import GHCJS.DOM.GlobalEventHandlers (click)

data Exists f = forall a. Exists { runExist :: f a }

data SearchResult
  = ResultSeason Season
  | ResultEpisode Episode

data Model = Model
  { modelFocused       :: Bool -- ^ Search input has focus
  , modelSearchResults :: [SearchResult] -- ^ Search input has focus
  , modelSearch :: JSString
  }
makeLensesWith camelCaseFields ''Model
  
data Msg a where
  SearchSubmit :: Msg ()
  SearchInput :: JSString -> Msg ()

init :: Model
init = Model False [] (JS.pack "Initial value")

eval :: (Member (State Model) r, Member Http r) => Msg a -> Eff r a
eval (SearchInput val) = do
  modify (search .~ val)
eval SearchSubmit = do
  resp <- httpGet "google.com"
--  jsg "console" # "log" [resp^.responseBody]
  pure ()

view :: forall ctx. Html' (Exists Msg) (Nested ctx Model)
view =
  div_ [ cs "root" ]
  [ div_ [ cs "logo" ] [ a_ [ href_ "" ] [ text_ "Telikov.com" ] ]
  , div_ [ cs "searchbar" ]
    [ input_ [ placeholder_ "Search", onInput $ \_ -> Left . Exists . SearchInput, prop "value" (modelSearch . here) ]
    , button_ [ type_ "button", on click $ \_ -> Left $ Exists $ SearchSubmit ] [ text_ "-->" ]
    ]
  , div_ [ cs "navbar" ]
    [ ul_ []
      [ li_ [] [ a_ [ href_ "" ] [ text_ "Home" ] ]
      , li_ [] [ a_ [ href_ "" ] [ text_ "Seasons" ] ]
      ]
    ]
  , element "style" [ type_ "text/css" ] [ text_ styles ]
  ]

styles = JS.lazyTextToJSString $ renderCss $ css () where
  Theme { unit, borderColor, bodyPadding, secondaryText } = theme
  css =
    [lucius|
      .#{JS.unpack prefix}-root {
        width: 100%;
        position: relative;
      }

      .#{JS.unpack prefix}-logo {
        position: absolute;
        left: #{bodyPadding}px;
        top: #{unit * 1.5}px;
        font-size: 22px;
        color: #{borderColor};
        a { color: #{borderColor}; text-decoration: none; &:hover { color: #{secondaryText} } }
      }

      .#{JS.unpack prefix}-searchbar {
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

      .#{JS.unpack prefix}-navbar {
        padding: #{unit}px 0;
        display: flex;
        height: #{unit * 6}px;
        align-items: center;
        justify-content: center;
        ul { display: flex; flex-direction: row; }
        li { display: flex; }
      }
    |]

prefix = "header"

cs :: JSString -> Attribute m i o
cs = class_ . (prefix <>) . ("-" <>)
