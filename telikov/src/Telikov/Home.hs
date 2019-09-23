{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE StaticPointers        #-}
module Telikov.Home where

import Control.Lens hiding (element, view)
import Data.Char (isDigit)
import Data.Int (Int64)
import qualified Data.JSString.Text as JS
import qualified Data.Text as T
import Data.Traversable (for)
import Database.SQLite.Simple ((:.) (..), Only (..))
import Database.SQLite.Simple.QQ (sql)
import Haste.App (annotate, dispatch, remote)
import Massaraksh
import Massaraksh.Html
import Massaraksh.Lens
import Parser.TheOffice.Db (Episode (..), Season (..))
import Polysemy.State (get, gets, modify, runState)
import Telikov.Effects (Eval, Init, mapMessages, query, query_, Exists (..))
import qualified Telikov.Header as Header
import Telikov.RPC (TelikovBackend)
import Telikov.Styles (Theme (..), theme, unit)
import Text.Lucius (lucius, luciusFile, renderCss)

data Model = Model
  { modelSeasons     :: [(Season, [Episode])] -- ^ Data from server
  , modelHeaderModel :: Header.Model
  }
makeLensesWith camelCaseFields ''Model

data Msg a where
  Inc        :: Msg ()
  Dec        :: Msg ()
  GetSeasons :: Msg [(Season, [Episode])]
  SetSeasons :: [(Season, [Episode])] -> Msg ()
  HeaderMsg  :: forall a. Header.Msg a -> Msg a

init :: Init Model
init = do
  seasonEpisodes <- dispatch $ static (remote $ do
    annotate :: TelikovBackend ()
    lastTransactId <- query_ @(Only Int) [sql|select max(rowid) from transactions where finished_at not null|]
    seasonPairs <- query @(Only Int64 :. Season) [sql|select rowid, * from seasons where tid=?|] (lastTransactId !! 0)
    for seasonPairs $ \(seasonId :. season) -> do
      episodes <- query @(Episode) [sql|select * from episodes where season_id=?|] seasonId
      pure (season, episodes)
    )

  pure $ Model seasonEpisodes Header.init

eval :: Msg a -> Eval Model Msg a
eval = \case
  Inc -> pure ()
  Dec -> pure ()
  GetSeasons -> do
    model <- get
    pure $ modelSeasons model
  SetSeasons _ -> pure ()
  HeaderMsg msg -> do
    model <- gets modelHeaderModel
    (s, a) <- Header.eval msg
      & mapMessages HeaderMsg
      & runState model
    modify (headerModel .~ s)
    pure a

liftHeader :: Exists Header.Msg -> Exists Msg
liftHeader (Exists msg) = Exists (HeaderMsg msg)

view :: Html' (Exists Msg) Model
view =
  el "main" []
  [ focus nestedId $ focusN headerModel (mapUI liftHeader Header.view)
  , div_ [ class_ "content" ]
    [ askModel $ \model -> ul_ [] $ flip fmap (modelSeasons model) $ \(season, episodes) ->
        let seasonLink = a_ [ href_ ("#" <> JS.textToJSString (seasonHref season)) ] in
        li_ [ class_ "season" ] $
        [ seasonLink [ h2_ [] [ text_ ("Season " <> JS.textToJSString (seasonName season)) ] ]
        , ul_ [ class_ "episodes" ] $ flip fmap episodes $ \episode ->
            let episodeLink = a_ [ href_ ("#" <> JS.textToJSString (episodeHref episode)) ] in
            li_ [ class_ "episode" ]
            [ episodeLink [ img_ [ class_ "episode-thumbnail", src_ (JS.textToJSString $ episodeThumbnail episode) ] ]
            , episodeLink [ text_ (JS.textToJSString $ episodeCode episode) ]
            , episodeLink [ text_ (JS.textToJSString $ episodeShortDescription episode) ]
            ]
        ]
    , el "style" [ type_ "text/css" ] [ text_ resetcss ]
    , el "style" [ type_ "text/css" ] [ text_ globalCss ]
    , el "style" [ type_ "text/css" ] [ text_ styles ]
    ]
  ] where
    seasonName season = T.pack $ go [] $ T.unpack $ seasonHref season where
      go prefix []       = reverse $ takeWhile isDigit prefix
      go prefix ('/':[]) = reverse $ takeWhile isDigit prefix
      go prefix (x:xs)   = go (x:prefix) xs
    el = element

styles = JS.lazyTextToJSString $ renderCss $ css () where
  Theme { unit, primaryText, borderColor, secondaryText } = theme
  borderRadius = 5 :: Double
  itemPadding = unit * 2
  bodyPadding = unit * 3
  css =
    [lucius|
      .content {
         margin: 0 #{bodyPadding}px;
      }

      .topmenu {
        width: 100%;
        height: #{unit * 6}px;
        border-bottom: solid 1px #{borderColor};
        display: flex;
        align-items: center;
        padding: 0 #{bodyPadding}px;
        ul { display: flex; flex-direction: row; }
        li { display: flex; }
        input { height: #{unit * 4}px; }
      }

      .season {
        margin-top: #{unit * 3}px;
        > a {
          color: #{primaryText}; text-decoration: none;
          &:hover { color: red; }
          > h2 { font-size: 18px; font-weight: 600; display: inline-block; }
        }
      }

      .episodes {
        display: flex;
        flex-direction: row;
        overflow: hidden;
      }

      .episode-thumbnail {
        display: block;
        object-fit: cover;
        height: 150px;
      }

      .episode: {
        position: relative;
        display: flex;
        flex-direction: column;
        marginRight: #{itemPadding} * 0.5;
        a { border-radius: #{borderRadius}px }
      }

      .episode + .episode { margin-left: #{unit}px; }

      .episode-top {
        border-top-left-radius: #{borderRadius}px;
        border-top-right-radius: #{borderRadius}px;
        overflow: hidden;
      }

      .episode-bottom {
        border-bottom-left-radius: #{borderRadius}px;
        border-bottom-right-radius: #{borderRadius}px;
        border: solid 1px #{borderColor};
        height: #{unit * 6};
        padding: #{unit * 0.5}px #{unit}px;
        box-sizing: border-box;
        h4 {
          margin: 0;
          font-size: 16;
        }
        > *:nth-child(2) {
          color: #{secondaryText};
          font-size: 14px;
        }
      }
    |]

globalCss = JS.lazyTextToJSString $ renderCss $ css () where
  css =
    [lucius|
      body, body * {
        font-family: Helvetica, Arial, sans-serif;
      }
    |]

resetcss = JS.lazyTextToJSString $ renderCss $ $(luciusFile "./src/Telikov/reset.css") ()
