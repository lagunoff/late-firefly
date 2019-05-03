{-# LANGUAGE OverloadedStrings, DataKinds, TypeFamilies, DeriveAnyClass, RankNTypes, TemplateHaskell #-}
module Main where

import Stitch
import Haste.Foreign
import qualified Haste.DOM as DOM
import Haste.Prim (toJSStr)
import Data.Text (Text, unpack)
import Data.Typeable.Internal
import Data.String
import Data.Monoid ((<>))
import IWatchTheOffice.Db (Db(..), Episode(..), Season(..))
import IWatchTheOffice.Db.Data (db)
import Data.OpenUnion
import TypeFun.Data.List (Elem)
import SDOM
import SDOM.Html
import SDOM.Prop
import qualified TheOffice.Home as Home

addTwo :: Int -> Int -> IO Int
addTwo = ffi "(function(x, y) {return x + y;})"

data Model = Model
  { counter :: Int
  , page :: Union Page
  }
type Page = '[Home.Model, String]


data Action
  = PageAction (forall a. (Typeable a, Elem a Page) => (TypeRep, a))


view :: SDOM Model msg
view =
  div_ [ cs "root" ]
  [ input_ [ placeholder_ "Login" ]
  , input_ [ placeholder_ "Password" ]
  , button_ [ type_ "button" ] [ text_ "GO!!!" ]
  , dimap page id view02
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


styles :: CSS
styles = do
  clazz "root" ? do
    "max-width" .= "650px"
    "margin" .= "0 auto"
  "h1" ?
    "font-weight" .= "bold"

clazz_ :: Text -> Selector
clazz_ name = Selector (["." <> clazz name])

clazz :: (IsString s, Monoid s) => s -> s
clazz name = name <> "-WL8dmuWnXGisImeb"

cs c = stringProp "className" (clazz c)
