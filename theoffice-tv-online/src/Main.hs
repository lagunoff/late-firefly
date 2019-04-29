{-# LANGUAGE OverloadedStrings #-}
module Main where

import Stitch
import Stitch.Combinators
import Haste.Foreign
import Haste.DOM
import Control.Monad.IO.Class
import Data.Text (Text, unpack)
import Data.String
import Data.Monoid ((<>))
import IWatchTheOffice.Db (Db(..), Episode(..), Season(..))
import IWatchTheOffice.Db.Data (db)


addTwo :: Int -> Int -> IO Int
addTwo = ffi "(function(x, y) {return x + y;})"

newtype Model = Model { counter :: Int }


node :: MonadIO m => String -> [(String, String)] -> [m Elem] -> m Elem
node name attrs childs = do
  el <- newElem name
  mapM_ (uncurry $ setAttr el) attrs
  mapM_ (flip (>>=) (appendChild el)) childs
  pure el

textNode :: MonadIO m => String -> m Elem
textNode content = do
  newTextElem content


render :: MonadIO m => Model -> m Elem
render _ =
  node "div" []
  [ node "ul" [] $ db_seasons db
      # map (\season -> node "li" []
              [ textNode (season_code season)
              , node "ul" [] $ season_episodes season
                  # map (\episode -> node "li" [] [ textNode (episode_description episode) ] )
              ]
            )
  , node "style" [("innerHTML", unpack $ renderCSS styles)] []
  ]
  where
    (#) = flip ($)

main :: IO ()
main = do
  root <- render $ Model { counter = 0 }
  appendChild documentBody root

-- | An example of a CSS document defined programmatically using "Stitch". To convert this to an real CSS document, use 'renderCSS'.
styles :: CSS
styles = do
  cssImport "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
  comment "This is an example comment. The \"compressed\" printer automatically strips comments to save space"
  clazz "root" ? do
    "background-color" .= "#dddddd"
    "width" .= "100px"
    "height" .= "100px"
    clazz "sdfsdf" ? do
      "width" .= "100px"
      "height" .= "100px"
      "& + &" ? do
        "margin-left" .= "100px"
      
  "h1" ?
    "font-weight" .= "bold"

cs :: (IsString s, Monoid s) => s -> s
cs name = name <> "-42761f74"

clazz :: Text -> Selector
clazz x = Selector ["." <> cs x]
