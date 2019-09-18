{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StaticPointers         #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
module Telikov.Home where

import Control.Lens hiding (element, view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState (get, put), gets, modify)
import Data.Char (isDigit)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Traversable (for)
import Database.SQLite.Simple ((:.) (..), Only (..))
import Database.SQLite.Simple.QQ (sql)
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Node hiding (Node)
import GHCJS.DOM.Types hiding (Node)
import Haste.App (annotate, remote)
import Massaraksh.Gui
import Massaraksh.Html
import Parser.TheOffice.Db (Episode (..), Season (..))
import Telikov.Effects (query, query_)
import qualified Telikov.Header as Header
import Telikov.RPC (TelikovBackend, callRPC)
import Telikov.Styles (Theme (..), theme, unit)
import Text.Lucius (lucius, luciusFile, renderCss)

data Model = Model
  { modelSeasons :: [(Season, [Episode])] -- ^ Data from server
  }
makeLensesWith camelCaseFields ''Model

data Msg a where
  Inc :: Msg ()
  Dec :: Msg ()
  GetSeasons :: Msg [(Season, [Episode])]
  SetSeasons :: [(Season, [Episode])] -> Msg ()

class MonadEmit msg m where
  emit :: msg a -> m a

eval
  :: (MonadIO m, MonadState Model m, MonadEmit Msg m)
  => Msg a
  -> m a
eval Inc = do
  model <- get
  put model
  seasons_ <- emit GetSeasons
  emit $ SetSeasons seasons_
  liftIO $ print (modelSeasons model) *> print seasons_
  pure ()
eval Dec = pure ()
eval GetSeasons = gets modelSeasons
eval (SetSeasons s) = modify (& seasons .~ s)

init :: JSM Model
init = do
  seasonEpisodes <- callRPC $ static (remote $ do
    annotate :: TelikovBackend ()
    lastTransactId <- query_ @(Only Int) [sql|select max(rowid) from transactions where finished_at not null|]
    seasonPairs <- query @(Only Int64 :. Season) [sql|select rowid, * from seasons where tid=?|] (lastTransactId !! 0)
    for seasonPairs $ \(seasonId :. season) -> do
      episodes <- query @(Episode) [sql|select * from episodes where season_id=?|] seasonId
      pure (season, episodes)
    )

  pure $ Model seasonEpisodes

view :: Html' (Msg a) Model
view =
  el "main" []
  [ Header.view
  , div_ [ class_ "content" ]
    [ askModel $ \model -> ul_ [] $ flip fmap (modelSeasons model) $ \(season, episodes) ->
        let seasonLink = a_ [ href_ (T.pack "#" <> seasonHref season) ] in
        li_ [ class_ "season" ] $
        [ seasonLink [ h2_ [] [ text_ (T.pack "Season " <> seasonName season) ] ]
        , ul_ [ class_ "episodes" ] $ flip fmap episodes $ \episode ->
            let episodeLink = a_ [ href_ (T.pack "#" <> episodeHref episode) ] in
            li_ [ class_ "episode" ]
            [ episodeLink [ img_ [ class_ "episode-thumbnail", src_ (episodeThumbnail episode) ] ]
            , episodeLink [ text_ (episodeCode episode) ]
            , episodeLink [ text_ (episodeShortDescription episode) ]
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

main :: Model -> JSM ()
main model = do
  doc  <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  storeHandle <- createStore model
  let sink (MsgStep f) = modifyStore storeHandle f
      sink _           = pure ()
  guiHandle <- unGui view (getStore storeHandle) sink
  appendChild_ body (ui guiHandle)

styles = L.toStrict $ renderCss $ css () where
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

globalCss = L.toStrict $ renderCss $ css () where
  css =
    [lucius|
      body, body * {
        font-family: Helvetica, Arial, sans-serif;
      }
    |]

resetcss = L.toStrict $ renderCss $ $(luciusFile "./src/Telikov/reset.css") ()
