{-# LANGUAGE OverloadedStrings, UndecidableInstances, FlexibleInstances, GADTs, PostfixOperators #-}
module TheOffice.Episode where
import IWatchTheOffice.Db
import IWatchTheOffice.Db.Data (db)
import SDOM.Html
import SDOM.Prop
import qualified SDOM.Html.Dynamic as Dyn
import qualified SDOM.Prop.Dynamic as Dyn
import qualified Haste.JSString as JS
import qualified TheOffice.Router as R
import SDOM
import Data.Monoid ((<>))
import Control.Arrow
import Data.List
import Data.Maybe
import Safe (atMay)

data Model = Model
  { episode :: Episode
  , active :: Int
  }
data Msg = Source Int

init :: String -> String -> Maybe Model
init s e = do
  seasn <- find (R.seasonCode >>> (== s)) (db_seasons db)
  ep <- find (R.episodeCode >>> (== e)) (season_episodes seasn)
  pure $ Model ep 3

view :: SDOM Model Msg
view =
  div_ []
  [ h2_ [] [ Dyn.text_ $ JS.pack . episode_name . episode ]
  , SDOM.list_ "ul" [] (episode_links . episode)
    $ li_ [] [ a_ [ Dyn.href_ (JS.pack . item) ] [ Dyn.text_ (JS.pack . item) ] ]
  , iframe_ [ attr "referrerpolicy" "no-referrer"
            , attr "scrolling" "no"
            , attr "allowfullscreen" "yes"
            , attr "frameborder" "0"
            , Dyn.stringProp "src" (\m -> JS.pack $ fromMaybe "" $ (episode_links (episode m) `atMay` (active m)))
            ] []
  ]
