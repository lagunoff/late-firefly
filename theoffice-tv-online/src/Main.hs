{-# LANGUAGE OverloadedStrings, DataKinds, TypeFamilies, DeriveAnyClass, RankNTypes, TemplateHaskell #-}
module Main where

import Stitch
import qualified Haste.DOM as DOM
import Haste.Prim.Foreign
import Data.Text (unpack)
import Data.Typeable.Internal
import Data.String
import Data.Monoid ((<>))
import Data.OpenUnion
import TypeFun.Data.List (Elem)
import SDOM
import SDOM.Html
import SDOM.Prop
import qualified TheOffice.Home as Home
import TheOffice.Style
import qualified Data.Text as T


data Model = Model
  { counter :: Int
  , page :: Union Page
  }
type Page = '[Home.Model, String]

data Action
  = PageAction (forall a. (Typeable a, Elem a Page) => (TypeRep, a))

view :: SDOM Model msg
view =
  div_ [ class_ (cs "root") ]
  [ nav_ [ class_ (cs "nav") ]
    [ ul_ []
      [ li_ [] [ a_ [ href_ "" ] [ b_ [] [ text_ "TheOffice-tv.online" ] ] ]
      , li_ [] [ a_ [ href_ "" ] [ text_ "Seasons" ] ]
      , li_ [] [ a_ [ href_ "" ] [ text_ "Random episode" ] ]
      ]
    ]
  , dimap page id view02
  , div_ [ id_ "disqus_thread" ] []
  , node "style" [ stringProp "innerHTML" . unpack $ renderCSS styles ] []
  ]

view02 :: SDOM (Union Page) msg
view02 = Home.view
 `union` v02
 `union` unionExhausted
  where
    v02 :: SDOM String msg
    v02 = text_ (" :: String")
    
main :: IO ()
main = do
  root <- create (Model { counter = 0, page = (liftUnion Home.init) }) view
  DOM.appendChild DOM.documentBody root
  initDisqus

resetDisqus :: IO ()
resetDisqus = ffi "(function(){ DISQUS.reset({ reload: true }); })"

initDisqus :: IO ()
initDisqus = ffi "(function(){ init_disqus(); })"

styles :: CSS
styles = do
  "body" ? do
    "margin" .= "0"
    "color" .= "rgba(0,0,0,0.87)"
  "html, body *" ? do
    "font-family" .= "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,\"Helvetica Neue\",Arial,\"Noto Sans\",sans-serif,\"Apple Color Emoji\",\"Segoe UI Emoji\",\"Segoe UI Symbol\",\"Noto Color Emoji\""
  cs_ ".root" ? do
    "margin" .= "0"
  cs_ ".nav" ? do
    "background" .= "hsla(0, 0%, 0%, 0.02)"
    "ul" ? do
      "margin" .= "0"
      "padding" .= "0"
      "display" .= "flex"
      "margin" .= "0 auto"
      "width" .= px (pageWidth theme)
    "li" ? do
      "list-style" .= "none"
      "&:first-child a" ? "padding-left" .= "0"
      "a" ? do
        "padding" .= list ["0", px (unit theme)]
        "display" .= "block"
        "height" .= px (unit theme * 4.5)
        "line-height" .= px (unit theme * 4.5)
        "color" .= toCss (colorTextSecondary theme)
        "text-decoration" .= "none"
        "&:hover" ? do
          "background" .= "hsla(0, 0%, 0%, 0.04)"
  "#disqus_thread" ? do
    "max-width" .= px (pageWidth theme)
    "margin" .= "0 auto"
    
  where
    cs_ = fromString . cs  
    px = T.pack . (<> "px") . show
    list = T.intercalate " "
    
cs :: (IsString s, Monoid s) => s -> s
cs name = name <> "-ot0oxddvn33DoOAB"
