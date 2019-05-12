{-# LANGUAGE OverloadedStrings, DataKinds, TypeFamilies, DeriveAnyClass, RankNTypes, ScopedTypeVariables, GADTs #-}
module Main where

import Stitch
import qualified Haste.DOM as DOM
import Haste.Prim.Foreign
import qualified Haste.JSString as JSS
import Data.Text (unpack)
import Data.String
import Data.Monoid ((<>))
import Data.OpenUnion
import TypeFun.Data.List (Elem)
import SDOM
import SDOM.Html
import qualified SDOM.Html.Dynamic as Dyn
import SDOM.Prop
import qualified TheOffice.Home as Home
import qualified TheOffice.Season as Season
import qualified TheOffice.Episode as Episode
import TheOffice.Style
import qualified Data.Text as T
import qualified TheOffice.Router as R
import Data.IORef
import Data.Maybe

data Model = Model
  { counter :: Int
  , page :: Union Page
  }
type Page = '[Home.Model, Season.Model, Episode.Model]
type PageMsg = '[Season.Msg, Episode.Msg]

data Msg where
  PageAction :: (Elem a PageMsg) => a -> Msg
  Click :: Msg

view :: SDOM Model Msg
view =
  div_ [ class_ (cs "root") ]
  [ nav_ [ class_ (cs "nav"), on_ "click" . const . const . Just $ Click ]
    [ ul_ []
      [ li_ [] [ a_ [ href_ "#" ] [ b_ [] [ text_ "TheOffice-tv.online" ] ] ]
      , li_ [] [ a_ [ href_ "#" ] [ text_ "Seasons" ] ]
      , li_ [] [ a_ [ href_ "#" ] [ text_ "Random episode" ] ]
      ]
    ]
  , dimap page id view02
  , div_ [ id_ "disqus_thread" ] []
  , node "style" [ stringProp "innerHTML" . unpack $ renderCSS styles ] []
  ]

view02 :: SDOM (Union Page) Msg
view02 = Home.view
 `union` dimap id PageAction Season.view
 `union` dimap id PageAction Episode.view
 `union` unionExhausted
  where
    v02 :: SDOM String msg
    v02 = Dyn.text_ $ JSS.pack . (<> " :: String")

initPage :: R.Route -> Maybe (Union Page)
initPage R.Home = Just $ liftUnion Home.init
initPage (R.Season season) = liftUnion <$> Season.init season
initPage route@(R.Episode s e) = liftUnion <$> Episode.init s e

dispatch :: Msg -> IO ()
dispatch Click = putStrLn "Clicked"
dispatch (PageAction msg) = putStrLn "Unknown Message"
   
main :: IO ()
main = do
  route <- R.current
  let initModel = Model { counter = 0, page = fromMaybe (liftUnion Home.init) $ initPage $ fromMaybe R.Home route }
  inst <- attach initModel dispatch DOM.documentBody view
  initDisqus
  _ <- R.onPopState $ \route -> case initPage route of
    Nothing -> pure ()
    Just p -> do
      putStrLn $ "New Route: " <> show route
      actuate inst (\model -> model { page = p })
      resetScroll
      pure ()
  pure ()

resetDisqus :: IO ()
resetDisqus = ffi "(function(){ DISQUS.reset({ reload: true }); })"

resetScroll :: IO ()
resetScroll = ffi
  "(function() { window.scrollTo(0,0); })"
  
initDisqus :: IO ()
initDisqus = ffi
  "(function() {\
  \  var d = document, s = d.createElement('script');\
  \  s.src = 'https://theoffice-tv-online.disqus.com/embed.js';\
  \  s.setAttribute('data-timestamp', +new Date());\
  \  (d.head || d.body).appendChild(s);\
  \})"

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
      "padding" .= list ["0", px (unit theme * 2)]
      "box-sizing" .= "content-box"
    "li" ? do
      "list-style" .= "none"
      "&:first-child a" ? "margin-left" .= px (-unit theme)
      "a" ? do
        "padding" .= list ["0", px (unit theme)]
        "display" .= "block"
        "height" .= px (unit theme * 4.5)
        "line-height" .= px (unit theme * 4.5)
        "color" .= toCss (colorTextSecondary theme)
        "text-decoration" .= "none"
        "&:hover" ? do
          "color" .= toCss (colorText theme) 
        "&:active" ? do
          "color" .= toCss (colorSecondary theme) 
  "#disqus_thread" ? do
    "max-width" .= px (pageWidth theme)
    "padding" .= list ["0", px (unit theme * 2)]
    "box-sizing" .= "content-box"
    "margin" .= "0 auto"
  where
    cs_ = fromString . cs  
    px = T.pack . (<> "px") . show
    list = T.intercalate " "
    
cs :: (IsString s, Monoid s) => s -> s
cs name = name <> "-ot0oxddvn33DoOAB"
