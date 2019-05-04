{-# LANGUAGE OverloadedStrings, UndecidableInstances, FlexibleInstances, GADTs, PostfixOperators #-}
module TheOffice.Home where

import Stitch hiding ((?))
import Stitch.Combinators.Extra
import Haste.Prim (toJSStr)
import Data.String (IsString)
import qualified Data.Text as T
import Data.Monoid ((<>))
import SDOM (SDOM)
import SDOM.Html
import SDOM.Prop
import qualified TheOffice.Router as R
import IWatchTheOffice.Db
import IWatchTheOffice.Db.Data (db)

newtype Model = Model ()


init :: Model
init = Model ()

view :: SDOM Model msg
view =
  main_ [ class_ (cs "root") ]
  [ ul_ [ class_ (cs "ul") ] (db_seasons db # map renderSeson)
  , node "style" [ stringProp "innerHTML" . T.unpack $ renderCSS styles ] []
  ]
  where
    (#) = flip ($)
    renderSeson season =
      li_ [ class_ ("li") ]
      [ h3_ [] [text_ $ toJSStr $ season_code season]
      , a_ [ href_ . toJSStr . ("#" <>) . R.print . R.Season $ season_code season ]
        [ img_ [ class_ (cs "rounded"), src_ . toJSStr . season_thumbnail $ season ] ]
      , div_ [ class_ (cs "episodes") ]
        $ [ b_ [] [ (text_ . toJSStr . show . length . season_episodes) season] ]
        <> season_episodes season # map (\e -> span_ [] [ a_ [ href_ . toJSStr . ("#" <>) . R.print $ R.Episode (season_code season) (episode_code e) ] [ (text_ . toJSStr . episode_code) e] ] )
      ]

styles :: CSS
styles = do
  cs ".root" ? do
    "max-width" .= px innerWidth
    "margin" .= list [px 0, "auto"]
    "padding" .= list [px 0, px (unit * 2)]
    "box-sizing" .= "content-box"
  cs ".ul" ? do
    "padding" .= px 0
    "display" .= "flex"
    "justify-content" .= "space-between"
    "flex-wrap" .= "wrap"
    
 where
    unit = 8 :: Int
    innerWidth = 860 :: Int
    px = T.pack . (<> "px") . show
    list = T.intercalate " "

cs :: (IsString s, Monoid s) => s -> s
cs name = name <> "-5ET49ANhU9T0gUK5"

