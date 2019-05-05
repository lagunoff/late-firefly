{-# LANGUAGE OverloadedStrings, UndecidableInstances, FlexibleInstances, GADTs, PostfixOperators #-}
module TheOffice.Home where

import Stitch
import Haste.Prim (toJSStr)
import Data.String (IsString)
import qualified Haste.JSString as JSStr
import qualified Data.Text as T
import Data.String (fromString)
import Data.Monoid ((<>))
import SDOM (SDOM)
import SDOM.Html
import SDOM.Prop
import qualified TheOffice.Router as R
import IWatchTheOffice.Db
import IWatchTheOffice.Db.Data (db)
import TheOffice.Style

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
      li_ [ class_ (cs "li") ]
      [ h3_ [] [text_ $ toJSStr $ season_code season]
      , a_ [ class_ (cs "season-thumbnail"), href_ (seasonUrl season) ]
        [ img_ [ class_ (cs "rounded"), src_ . toJSStr . season_thumbnail $ season ] ]
      , div_ [ class_ (cs "episodes") ]
        $ [ b_ [] [ text_ . toJSStr . (<> " episodes: ") . show . length . season_episodes $ season] ]
        <> season_episodes season
        # map (\e -> a_ [ href_ (episodeUrl season e) ] [ text_ . (\x -> " " <> x <> " ") . prepareEpisode . toJSStr . episode_code $ e])
      ]
    seasonUrl season = toJSStr . ("#" <>) . R.print . R.Season . season_code $ season
    episodeUrl season episode = toJSStr . ("#" <>) . R.print $ R.Episode (season_code season) (episode_code episode)
    regex = "^S\\d\\d?" :: JSStr.RegEx
    prepareEpisode str = JSStr.replace str regex ""

styles :: CSS
styles = do
  cs_ ".root" ? do
    "max-width" .= px (pageWidth theme)
    "margin" .= list [px 0, "auto"]
    "padding" .= list [px 0, px (unit theme * 2)]
    "box-sizing" .= "content-box"
    "h3" ? do
      "font-size" .= "28px"
      "font-weight" .= "300"
      "margin" .= list ["0", "0", px (unit theme), "0"]
  cs_ ".ul" ? do
    "padding" .= px 0
    "margin" .= px 0
    "display" .= "flex"
    "justify-content" .= "space-between"
    "flex-wrap" .= "wrap"
  cs_ ".li" ? do
    "width" .= itemWidth 3
    "list-style" .= "none"
    "margin" .= list [px (unit theme * 2), "0", "0", "0"]
    "overflow" .= "hidden"
    "& img" ? do
      "width" .= "100%"
      "border-radius" .= "5px"
    "& > div" ? do
      "font-size" .= px 14
      "line-height" .= "1.4em"
      "b" ? do
        "display" .= "block"
      "a" ? do
        linkStyles
        "text-transform" .= "lowercase"
    media "(max-width: 768px) and (min-width: 500px)" ? do
      "width" .= itemWidth 2
    media "(max-width: 500px)" ? do
      "width" .= itemWidth 1
  cs_ ".season-thumbnail" ? do
    "display" .= "block"
    "position" .= "relative"
    "&:before" ? do
      "content" .= "\"\""
      "display" .= "block"
      "position" .= "absolute"
      "width" .= "100%"
      "height" .= "100%"
      "top" .= "0"
      "left" .= "0"
    "&:hover:before" ? do
      "background" .= "rgba(255, 255, 255, 0.2)"
 where
  cs_ = fromString . cs
  px = T.pack . (<> "px") . show
  list = T.intercalate " "
  gridPadding = unit theme * 2;
  itemWidth columns = T.pack $ "calc((100% - " <> show (gridPadding * (columns - 1)) <> "px) / " <> show columns <> ")"

cs :: (IsString s, Monoid s) => s -> s
cs name = name <> "-5ET49ANhU9T0gUK5"

