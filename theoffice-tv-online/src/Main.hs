{-# LANGUAGE OverloadedStrings, DataKinds, TypeFamilies, DeriveAnyClass, RankNTypes #-}
module Main where

import Stitch
import qualified Haste.DOM as DOM
import Haste.Prim.Foreign
import qualified Haste.JSString as JSS
import Data.Text (unpack)
import Data.Typeable.Internal
import Data.String
import Data.Monoid ((<>))
import Data.OpenUnion
import TypeFun.Data.List (Elem)
import SDOM
import SDOM.Html
import qualified SDOM.Html.Dynamic as Dyn
import SDOM.Prop
import qualified TheOffice.Home as Home
import TheOffice.Style
import qualified Data.Text as T
import qualified TheOffice.Router as R
import Data.IORef

data Model = Model
  { counter :: Int
  , page :: Union Page
  }
type Page = '[Home.Model, String]

data Msg
  = PageAction (forall a. (Typeable a, Elem a Page) => (TypeRep, a))
  | Click

view :: SDOM Model Msg
view =
  div_ [ class_ (cs "root") ]
  [ nav_ [ class_ (cs "nav"), on_ "click" . const . Just $ Click ]
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

view02 :: SDOM (Union Page) msg
view02 = Home.view
 `union` v02
 `union` unionExhausted
  where
    v02 :: SDOM String msg
    v02 = Dyn.text_ $ JSS.pack . (<> " :: String")

initPage :: R.Route -> Union Page
initPage R.Home = liftUnion Home.init
initPage route@(R.Season {}) = liftUnion $ show route
initPage route@(R.Episode {}) = liftUnion $ show route

dispatch :: Msg -> IO ()
dispatch Click = putStrLn "Clicked"
dispatch _ = putStrLn "Unknown Message"
   
main :: IO ()
main = do
  let initModel = Model { counter = 0, page = (liftUnion Home.init) }
  root <- create_ dispatch initModel view
  DOM.appendChild DOM.documentBody root
  initDisqus
  modelRef <- newIORef initModel
  _ <- R.onPopState $ \route -> do
    putStrLn $ "New Route: " <> show route
    prevModel <- readIORef modelRef
    let nextModel = prevModel { page = initPage route }
    _ <- actuate prevModel nextModel root view
    writeIORef modelRef nextModel
    pure ()
  pure ()

resetDisqus :: IO ()
resetDisqus = ffi "(function(){ DISQUS.reset({ reload: true }); })"

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
