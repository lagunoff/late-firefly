{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE StaticPointers    #-}
module Telikov.Home where

import Control.Lens hiding (element, view)
import Control.Monad.State.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.String (fromString)
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Node hiding (Node)
import GHCJS.DOM.Types hiding (Node)
import Haste.App
import Massaraksh.Gui
import Data.Traversable (for)
import Massaraksh.Html
import Parser.TheOffice.Db
import Telikov.RPC (MyS)
import Text.Lucius (lucius, luciusFile, renderCss)
import Text.Regex (matchRegex, mkRegex)
import Database.SQLite.Simple (Only (..), query, query_, withConnection, (:.)(..))
import Data.Int (Int64)

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
  seasons_ <- dispatch homeRPC
  pure $ Model seasons_
  where
    homeRPC :: RemotePtr (MyS [(Season, [Episode])])
    homeRPC = static (native $ remote $ do
      liftIO $ withConnection "./test.db" $ \conn -> do
      [Only updateId] <- query_ conn (fromString "select max(rowid) from transactions where finished_at not null") :: IO [Only Int]
      idSeasons <- query conn (fromString "select rowid, * from seasons where tid=?") (Only updateId) :: IO [Only Int64 :. Season]
      for idSeasons $ \(seasonId :. season) -> do
        episodes <- query conn (fromString "select * from episodes where season_id=?") (seasonId) :: IO [Episode]
        pure (season, episodes)
     )

view :: Html' (Msg a) Model
view =
  el "main" []
  [ nav_ [ class_ "topmenu" ]
    [ ul_ []
      [ li_ [] [ a_ [ href_ "" ] [ text_ "Home" ] ]
      , li_ [] [ a_ [ href_ "" ] [ text_ "Seasons" ] ]
      ]
    , div_ [] [ input_ [ placeholder_ "Search" ] ]
    ]
  , div_ [ class_ "content" ]
    [ askModel $ \model -> ul_ [] $ flip fmap (modelSeasons model) $ \(season, episodes) ->
        let seasonLink = a_ [ href_ (seasonHref season) ] in
        li_ [ class_ "season" ] $
        [ seasonLink [ h2_ [] [ text_ (T.pack "Season " <> seasonName season) ] ]
        , ul_ [ class_ "episodes" ] $ flip fmap episodes $ \episode ->
            let episodeLink = a_ [ href_ (episodeHref episode) ] in
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
    seasonName season = case matches of Just [n] -> T.pack n; _ -> T.pack "-1"; where
      matches = matchRegex (mkRegex "0*([[:digit:]]+)/$") $ T.unpack $ seasonHref season
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

styles :: T.Text
styles = L.toStrict $ renderCss $ [lucius|
.content {
  margin: 0 24px;
}
.topmenu {
  width: 100%;
  height: 48px;
  border-bottom: solid 1px #ccc;
  display: flex;
  ul {
    display: flex;
    flex-direction: row;
  }
  li {
    display: flex;
  }
}
.season {
  margin-top: 24px;
  > a {
    color: rgba(0,0,0,0.87); text-decoration: none;
    &:hover { color: red; }
    > h2 { font-size: 18px; font-weight: 600; display: inline-block; }
  }
}
.episodes {
  display: flex;
  flex-direction: row;
  overflow: hidden;
}
.episode {
}
.episode + .episode { margin-left: 8px; }
.episode-thumbnail {
  display: block;
  object-fit: cover;
  height: 150px;
}
|] ()

globalCss :: T.Text
globalCss = L.toStrict $ renderCss $ [lucius|
body, body * {
  font-family: Helvetica, Arial, sans-serif;
}
|] ()

resetcss :: T.Text
resetcss = L.toStrict $ renderCss $ $(luciusFile "./src/Telikov/reset.css") ()
