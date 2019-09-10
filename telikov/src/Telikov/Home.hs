{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Telikov.Home where

import Control.Lens hiding (element, view)
import Data.List ((!!))
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.GlobalEventHandlers
import GHCJS.DOM.Node hiding (Node)
import GHCJS.DOM.Types hiding (Node)
import Haste.App
import Parser.TheOffice.Db
import Language.Javascript.JSaddle hiding (new, (!!))
import Massaraksh.Gui
import Massaraksh.Html
import Telikov.RPC (greet)
import Text.Lucius (lucius, renderCss)
import qualified Data.Text.Lazy as L

data Model = Model
  { _counter :: Int      -- ^ Incremented each second
  , _clicks  :: Int      -- ^ How many times left button was clicked
  , _seasons :: [(Season, [Episode])] -- ^ Data from server
  }

$(makeLenses ''Model)

colors :: [String]
colors = ["#F44336", "#03A9F4", "#4CAF50", "#3F51B5", "#607D8B", "#FF5722"]

init :: JSM Model
init = do
  seasons <- dispatch greet ""
  pure $ Model 0 0 seasons

view :: Html' msg Model
view =
  element "main" []
  [ div_ [ prop_ "className" "root" ]
    [ h1_ [ prop "style" $ \m -> "color: " <> (colors !! (m ^. counter `rem` 6)) <> "" ] [ text_ "Kia ora (Hi)" ]
    , div_ [ prop_ "style" "display: flex; margin-bottom: 16px" ]
      [ div_ [ prop_ "style" "padding: 2px 8px; border: solid 3px red; background: rgba(255,0,0,0.5); color: white" ] [ text (show . _counter) ]
      , div_ [ prop_ "style" "margin-left: 24px; padding: 2px 8px; border: solid 3px green; background: rgba(0,255,0,0.5); color: white" ] [ text (show . _clicks) ]
      ]
    , div_ [ prop_ "class" "wrapper" ]
      [ button_ [ on click (\_ -> Right $ clicks %~ (+) 1) ] [ text_ "Click here" ]
      , button_ [ on click (\_ -> Right $ counter .~ 0) ] [ text_ "Reset counter" ]
      ]
    , element "style" [ prop_ "type" "text/css" ] [ text_ css ]
    , askModel $ \model -> ul_ [] $ flip fmap (_seasons model) $ \(season, episodes) ->
        li_ [] $
        [ h5_ [] [ text_ (seasonHref season) ]
        , img_ [ prop_ "src" (seasonThumbnail season) ]
        , ul_ [] $ flip fmap episodes $ \episode -> li_ [] [ text_ (episodeCode episode) ]
        ]
    ]
  ]

main :: Model -> JSM ()
main model = do
  doc  <- currentDocumentUnchecked
  body <- getBodyUnchecked doc

  storeHandle <- createStore model
  GuiHandle { ui, finalizer } <- unGui view (getStore storeHandle) $ \case
    MsgStep f -> modifyStore storeHandle f
    _ -> pure ()

  cb <- function $ \_ _ _ -> modifyStore storeHandle $ counter %~ (+) 1
--  jsg2 ("setInterval" :: String) cb (1000 :: Int)

  appendChild_ body ui

css = L.toStrict $ renderCss $ [lucius|
.root {
  margin: 0 auto;
  max-width: 600px;
}
|] ()
