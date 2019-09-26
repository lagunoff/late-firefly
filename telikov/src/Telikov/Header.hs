{-# LANGUAGE TemplateHaskell, OverloadedStrings, StaticPointers #-}
module Telikov.Header where

import Massaraksh.Html
import Massaraksh
import Parser.TheOffice.Db (Season (..))
import Telikov.Styles (Theme (..), theme, unit)
import Text.Lucius (lucius, renderCss)
import Polysemy.State (modify)
import Telikov.Effects (Eval, Exists(..), embed, query_)
import Telikov.RPC (TelikovBackend)
import Control.Lens hiding (element, view)
import Data.JSString as JS
import qualified Data.JSString.Text as JS
import GHCJS.DOM.GlobalEventHandlers (click)
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
import Haste.App (annotate, remote, dispatch)

data SearchResult
  = ResultSeason Season

data Model = Model
  { modelFocused       :: Bool -- ^ Search input has focus
  , modelSearchResults :: [SearchResult] -- ^ Search input has focus
  , modelSearch        :: JSString
  }
makeLensesWith camelCaseFields ''Model
  
data Msg a where
  SearchSubmit :: Msg ()
  SearchInput :: JSString -> Msg ()

init :: Model
init = Model False [] (JS.pack "Initial value")

eval :: Msg a -> Eval Model Msg a
eval = \case
  SearchInput val -> do
    modify (search .~ val)
  SearchSubmit -> do
    c <- dispatch $ static (remote $ do
      annotate :: TelikovBackend ()
      res <- query_ @(Only Int) [sql|select count(*) from episodes|]
      pure $ fromOnly $ res !! 0
      )
    c2 <- dispatch $ static (remote $ do
      annotate :: TelikovBackend ()
      res <- query_ @(Only Int) [sql|select count(*) from episodes|]
      pure $ fromOnly $ res !! 0
      )
    embed $ putStrLn $ "count: " <> show c
    embed $ putStrLn $ "count: " <> show c2
    pure ()
  
view :: forall ctx. Html' (Exists Msg) (Nested ctx Model)
view =
  div_ [ cs "root" ]
  [ div_ [ cs "logo" ] [ a_ [ href_ "" ] [ text_ "Telikov.com" ] ]
  , div_ [ cs "searchbar" ]
    [ input_ [ placeholder_ "Search", onInput $ \_ -> Left . Exists . SearchInput, prop "value" (modelSearch . here) ]
    , button_ [ type_ "button", on click $ \_ -> Left $ Exists $ SearchSubmit ] [ text_ "-->" ]
    ]
  , div_ [ cs "navbar" ]
    [ ul_ []
      [ li_ [] [ a_ [ href_ "" ] [ text_ "Home" ] ]
      , li_ [] [ a_ [ href_ "" ] [ text_ "Seasons" ] ]
      ]
    ]
  , element "style" [ type_ "text/css" ] [ text_ styles ]
  ]

styles = JS.lazyTextToJSString $ renderCss $ css () where
  Theme { unit, borderColor, bodyPadding, secondaryText } = theme
  unprefix = JS.unpack prefix
  css =
    [lucius|
      .#{unprefix}-root {
        width: 100%;
        position: relative;
      }

      .#{unprefix}-logo {
        position: absolute;
        left: #{bodyPadding}px;
        top: #{unit * 1.5}px;
        font-size: 22px;
        color: #{borderColor};
        a { color: #{borderColor}; text-decoration: none; &:hover { color: #{secondaryText} } }
      }

      .#{unprefix}-searchbar {
        border-bottom: solid 1px #{borderColor};
        display: flex;
        padding: #{unit}px 0;
        justify-content: center;
        input {
          height: #{unit * 3}px;
          width: 500px;
          font-size: 18px;
          padding: 0 #{unit}px;
        }
      }

      .#{unprefix}-navbar {
        padding: #{unit}px 0;
        display: flex;
        height: #{unit * 6}px;
        align-items: center;
        justify-content: center;
        ul { display: flex; flex-direction: row; }
        li { display: flex; }
      }
    |]

prefix = "header"

cs :: JSString -> Attribute m i o
cs = class_ . (prefix <>) . ("-" <>)
