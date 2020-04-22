{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}
module LF.Index (indexWidget, Model(..)) where

import Control.Lens
import Control.Monad.Trans
import Data.Maybe
import Data.Text as T
import Data.List as L
import Flat.Rpc hiding (remote, to)
import GHC.Records
import LF.Backend
import LF.DB
import LF.Prelude
import LF.Router
import LF.TheOffice.Schema
import Language.Javascript.JSaddle
import Massaraksh as H
import Text.Shakespeare.Text (st)

data Route
  = AboutR
  | EpisodeR {season :: Int, episode :: Text}
  | SeasonR {season :: Int}
  | IndexR
  deriving (Show, Eq, Generic, HasParser U)

data Model m = Model
  { route :: Route }
  deriving (Generic)

indexWidget :: (HtmlBase m, MonadClient m) => HtmlT m ()
indexWidget = mdo
  mdl <- liftIO $ newDynRef (Model route)
  route <- liftJSM $ hashRouter IndexR \x -> do
    modifyDynRef mdl \m -> m {route=x}
  seasons <- lift $ send "sdfsdf" $ static (remote getSeasons)
  div_ do
    "className" =: "root"
  ul_ do
    li_ do a_ do "Home"; "href" =: priUrl IndexR
    li_ do a_ do "About"; "href" =: priUrl AboutR
  div_ do
    routeDyn <- liftIO $ flip holdUniqDynBy (getDyn mdl)
      \Model{route=a} Model{route=b} -> a == b
    dynHtml $ routeDyn <&> getField @"route" <&> \case
      IndexR -> indexPage seasons
      AboutR -> aboutPage
      SeasonR{..} -> episodesWidget season
      EpisodeR{..} -> episodeWidget episode

priUrl = ("#" <>) . printUrl

getSeasons :: Given Connection => Text -> IO [Season :. Only Int]
getSeasons txt = do
  query_ [sql|
    select s.*, (select count(*) from `episode` where season_id=s.uuid)
    from season s order by `number`
  |]

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

aboutPage :: HtmlBase m => HtmlT m ()
aboutPage = do
  div_ do
    h1_ do
      "About Page Works!!!"

indexPage :: HtmlBase m => [Season :. Only Int] -> HtmlT m ()
indexPage ss = do
  div_ do
    h1_ do
      "Index Page Works!!!"
    ul_ $ for_ ss \(Season{..} :. Only eps) -> do
      li_ do
        a_ do
          "href" =: priUrl (SeasonR number)
          h3_ do text [st|Season #{showt number}|]
          div_ do text [st|#{showt eps} Episodes|]
          img_ do "src" =: thumbnail

episodesWidget :: (HtmlBase m, MonadClient m) => Int -> HtmlT m ()
episodesWidget sNum = mdo
  episodes <- lift $ send sNum $ static (remote getEpisodes)
  div_ do
    "className" =: "root"
    ul_ $ for_ episodes \Episode{..} -> do
      li_ do
        a_ do
          "href" =: priUrl (EpisodeR sNum code)
          h3_ do text [st|Episode #{code}|]
          img_ do "src" =: thumbnail
          p_ do text shortDesc

episodeWidget :: (HtmlBase m, MonadClient m) => Text -> HtmlT m ()
episodeWidget epCode = mdo
  Episode{..} <- lift $ send epCode $ static (remote getEpisode)
  div_ do
    "className" =: "root"
    h3_ do text [st|Episode #{code}|]
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
