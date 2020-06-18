{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}
module LateFirefly.Index
  ( Model(..)
  , indexWidget
  ) where

import Control.Lens
import Control.Monad.Trans
import Data.List as L
import Data.Maybe
import Data.Text as T
import Data.UUID.Types as U
import GHC.Records
import Language.Javascript.JSaddle
import LateFirefly.DB
import LateFirefly.Index.Season
import LateFirefly.Prelude
import LateFirefly.Router
import LateFirefly.TheOffice.Schema
import LateFirefly.Widget.Prelude
import Text.Shakespeare.Text (st)
import LateFirefly.RPC.TH
import Data.JSString.Text as JSS

data Model m = Model
  { route :: Route }
  deriving Generic

indexWidget :: Html
indexWidget = mdo
  let Theme{..} = theme
  headerWidget
  sliderWidget
  mdl <- liftIO $ newDynRef (Model route)
  route <- liftJSM $ hashRouter IndexR \x -> do
    modifyDynRef mdl \m -> m {route=x}
  div_ do
    "className" =: "root"
    div_ do
      routeDyn <- liftIO $ flip holdUniqDynBy (getDyn mdl)
        \Model{route=a} Model{route=b} -> a == b
      dynHtml $ routeDyn <&> getField @"route" <&> \case
        IndexR -> indexPage
        AboutR -> aboutPage
        SeasonR{..} -> episodesWidget season
        EpisodeR{..} -> episodeWidget episode
  [style|
    html, body, body *
      font-family: Arial, sans-serif
    body
      margin: 0|]

headerWidget :: Html
headerWidget = do
  let Theme{..} = theme
  div_ do
    "className" =: "header"
    a_ do
      "className" =: "home-link"
      "href" =: "#"
      span_ "telikov.net"
    ul_ do
      "className" =: "menu"
      li_ do a_ do "Seasons"; "href" =: printRoute IndexR
      li_ do a_ do "About"; "href" =: printRoute AboutR
    div_ do
      "className" =: "search"
      input_ do
        "placeholder" =: "Search"
    [style|
      .header
        display: flex
        align-items: center
        background: white
        color: #{primaryText}
        height: #{unit * 5}
        width: 100%
        box-shadow: 0 0 1px rgba(0,0,0,0.2)
        z-index: 2
        position: relative
      .home-link
        color: black
        &:hover
          color: #{primary}
        transition: color .2s
        font-weight: 600
        display: block
        height: 100%
        text-decoration: none
        display: flex
        align-items: center
        padding: 0 #{unit * 2} 0 #{unit * 2}
        text-transform: uppercase
      .menu
        margin: 0 #{unit * 2}
        padding: 0
        display: flex
        align-items: center
        li
          list-style: none
          margin: 0
        li + li
          margin-left: #{unit * 2}
        a
          color: #{secondaryText}
          text-decoration: none
          transition: color .2s
          &:hover
            color: #{primaryText}
      .search
        padding: 0 #{unit * 2}
        height: 100%
        display: flex
        align-items: center
        &:hover
          background: rgba(0,0,0,0.05)
        input
          border: none
          outline: none
          font-size: 16px
          background: transparent
          font-weight: 600
        |]

sliderWidget :: Html
sliderWidget = do
  img_ do
    "src" =: "https://sm.ign.com/t/ign_pl/screenshot/default/the-office-not-leaving-netflix-until-2021_7kc8.1280.jpg"
    "className" =: "slider"
  [style|
    .slider
      width: 100%
      height: 350px
      object-fit: contain
      background: rgba(0,0,0,0.05)
    |]

getSeasons :: Given Connection => Text -> IO [(Season, [Episode])]
getSeasons txt = do
  seasons <- selectFrom_ @Season [sql|where 1 order by `number`|]
  let seasonIds = T.intercalate ", " $ escText . U.toText . unUUID5 . getField @"uuid" <$> seasons
  episodes <- selectFrom_ @Episode [sql|where season_id in (#{seasonIds}) order by `code`|]
  pure $ seasons <&> \s@Season{uuid} -> (s, L.filter ((==uuid) . getField @"seasonId") episodes)

getEpisodes :: Given Connection => Int -> IO [Episode]
getEpisodes seasonNumber = do
  query [sql|
    select e.* from `episode` e
      left join `season` s on e.season_id=s.uuid
    where s.`number`=? order by e.code
  |] [seasonNumber]

getEpisode :: Given Connection => Text -> IO Episode
getEpisode epCode = do
  L.head <$> query [sql|
    select e.* from `episode` e
      left join `season` s on e.season_id=s.uuid
    where e.`code`=?
  |] [epCode]

aboutPage :: Html
aboutPage = do
  div_ do
    h1_ do
      "About Page Works!!!"

indexPage :: Html
indexPage = do
  ss <- $(remote 'getSeasons) ""
  seasonWidget ss

episodesWidget :: Int -> Html
episodesWidget sNum = do
  episodes <- $(remote 'getEpisodes) sNum
  div_ do
    "className" =: "root"
    ul_ $ for_ episodes \Episode{..} -> do
      li_ do
        a_ do
          "href" =: printRoute (EpisodeR sNum code)
          h3_ do [ht|Episode #{code}|]
          img_ do "src" =: thumbnail
          p_ do text shortDesc

episodeWidget :: Text -> Html
episodeWidget epCode = do
  Episode{..} <- $(remote 'getEpisode) epCode
  div_ do
    "className" =: "root"
    h3_ do [ht|Episode #{code}|]
    img_ do "src" =: thumbnail
    p_ $ text description
    ul_ $ for_ links (li_ . text)

hashRouter :: forall a. HasParser U a => a -> (a -> IO ()) -> JSM a
hashRouter def hashChange = do
  win <- jsg ("window" :: Text)
  let
    parseRoute = do
      hash <- fromJSValUnchecked =<< jsg ("location" :: Text) ! ("hash" :: Text)
      pure $ fromMaybe def $ listToMaybe $ parseUrl @a (T.drop 1 hash)
  win <# ("onpopstate" :: Text) $ fun \_ _ _ -> do
    liftIO . hashChange =<< parseRoute
  parseRoute
