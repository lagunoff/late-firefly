module LateFirefly.Index.Season where

import LateFirefly.Widget.Prelude
import LateFirefly.TheOffice.Schema
import LateFirefly.Router

seasonWidget
  :: (HtmlBase m, MonadClient m) => [(Season, [Episode])] -> HtmlT m ()
seasonWidget seasons = do
  div_ do
    for_ seasons \(Season{..}, episodes) -> do
      h3_ do
        "className" =: "season-header"
        [ht|Season #{showt number}|]
      ul_ do
        "className" =: "episodes-list"
        for_ episodes \Episode{..} -> do
          li_ do
            a_ do
              "href" =: printRoute (EpisodeR number code)
              img_ do "src" =: thumbnail
              div_ do
                h4_ [ht|Episode #{code}|]
                span_ (text name)
  [style|
    .episodes-list
      margin: 0
      display: flex
      li
        list-style: none|]

