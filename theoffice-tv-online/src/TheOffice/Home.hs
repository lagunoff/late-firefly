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
  div_ []
  [ header_ [ class_ (cs "summary") ]
    [ a_ [ class_ (cs "poster"), href_ "https://m.media-amazon.com/images/M/MV5BMTgzNjAzMDE0NF5BMl5BanBnXkFtZTcwNTEyMzM3OA@@._V1_SY1000_CR0,0,736,1000_AL_.jpg" ]
      [ img_ [ class_ (cs "rounded"), width_ "270", src_ "https://m.media-amazon.com/images/M/MV5BMTgzNjAzMDE0NF5BMl5BanBnXkFtZTcwNTEyMzM3OA@@._V1_SY1000_CR0,0,736,1000_AL_.jpg" ]
      ]
    , div_ [ class_ (cs "summary-right") ]
      [ h2_ [] [ text_ "The Office (US)" ]
      , p_ [] [ text_ "This US adaptation, set at a paper company in Scranton, Pa., has a similar documentary style to that of the Ricky Gervais-led British original. It features the staff of Dunder-Mifflin, a staff that includes characters based on characters from the British show (and, quite possibly, people you work with in your office). There's Jim, the likable employee who's a bit of an everyman. Jim has a thing for receptionist-turned-sales rep Pam (because office romances are always a good idea). There's also Dwight, the successful co-worker who lacks social skills and common sense. And there's Ryan, who has held many jobs at the company" ]
      ]
    , div_ [ class_ (cs "clearfix") ] []
    ]
  , main_ [ class_ (cs "main") ]
    [ ul_ [ class_ (cs "ul") ] (db_seasons db # map renderSeson)
    , node "style" [ stringProp "innerHTML" . T.unpack $ renderCSS styles ] []
    ]
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
    seasonUrl season = toJSStr . ("#" <>) $ R.seasonUrl season
    episodeUrl season episode = toJSStr . ("#" <>) $ R.episodeUrl season episode
    regex = "^S\\d\\d?E" :: JSStr.RegEx
    prepareEpisode str = JSStr.replace str regex ""

styles :: CSS
styles = do
  cs_ ".summary" ? do
    "max-width" .= px (pageWidth theme)
    "margin" .= list [px (unit theme * 3), "auto"]
    "padding" .= list ["0", px (unit theme * 2)]
    "display" .= "flex"
    "flex-direction" .= "row"
  cs_ ".poster" ? do
    "margin-right" .= px (unit theme * 3)
    media "(max-width: 500px)" ? do
      "display" .= "none"
  cs_ ".summary-right" ? do
    "p" ? do
      "font-size" .= "14px"
      "line-height" .= "1.4em"
    "h2" ? do
      "margin" .= "0"
      "font-size" .= "40px"
      "font-weight" .= "400"
  cs_ ".main" ? do
    "padding" .= list [px 0, px (unit theme * 2)]
    "background" .= "rgba(0,0,0,0.03)"
    "margin-bottom" .= px (unit theme * 3)
  cs_ ".ul" ? do
    "max-width" .= px (pageWidth theme)
    "margin" .= list [px 0, "auto"]
    "box-sizing" .= "content-box"
    "padding" .= list ["0", "0", px (unit theme * 3), "0"]
    "display" .= "flex"
    "justify-content" .= "space-between"
    "flex-wrap" .= "wrap"
  cs_ ".li" ? do
    "width" .= itemWidth 3
    "list-style" .= "none"
    "margin" .= list [px (unit theme * 2), "0", "0", "0"]
    "overflow" .= "hidden"
    "h3" ? do
      "font-size" .= "28px"
      "font-weight" .= "300"
      "margin" .= list ["0", "0", px (unit theme), "0"]
    "& img" ? do
      "width" .= "100%"
    "& > div" ? do
      "font-size" .= px 14
      "line-height" .= "1.4em"
      "a" ? do
        linkStyles
        "color" .= toCss (colorTextSecondary theme)
        "text-transform" .= "lowercase"
        "&:hover" ? do
          "color" .= toCss (colorSecondary theme)
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
  cs_ ".clearfix" ? do
    "clear" .= "both"
  cs_ ".rounded" ? do
    "border-radius" .= "5px"
  where
    cs_ = fromString . cs
    px = T.pack . (<> "px") . show
    list = T.intercalate " "
    gridPadding = unit theme * 2;
    itemWidth columns = T.pack $ "calc((100% - " <> show (gridPadding * (columns - 1)) <> "px) / " <> show columns <> ")"

cs :: (IsString s, Monoid s) => s -> s
cs name = name <> "-5ET49ANhU9T0gUK5"

