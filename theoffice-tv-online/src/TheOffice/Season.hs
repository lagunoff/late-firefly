{-# LANGUAGE OverloadedStrings, UndecidableInstances, FlexibleInstances, GADTs, PostfixOperators #-}
module TheOffice.Season where
import IWatchTheOffice.Db
import IWatchTheOffice.Db.Data (db)
import SDOM.Html
import qualified SDOM.Html.Dynamic as Dyn
import qualified SDOM.Prop.Dynamic as Dyn
import qualified Haste.JSString as JS
import qualified TheOffice.Router as R
import SDOM
import Data.Monoid ((<>))
import Control.Arrow
import Data.List

newtype Model = Model { season :: Season }

data Msg = Click1 | Click2

init :: String -> Maybe Model
init code = Model <$> find (R.seasonCode >>> (== code)) (db_seasons db)

view :: SDOM Model Msg
view =
  div_ []
  [ p_ [] [ Dyn.text_ $ JS.pack . ("This is the season: " <>) . R.seasonCode . season ]
  , list_ "ul" [] (season_episodes . season)
    $ li_ []
      [ div_ [] [ img_ [ Dyn.src_ (JS.pack . episode_thumbnail . item) ] ]
      , div_ []
        [ h2_ [] [ Dyn.text_ (JS.pack . episode_name . item) ]
        , p_ [] [ Dyn.text_ (JS.pack . episode_short_description . item) ]
        ]
      ]
  ]
