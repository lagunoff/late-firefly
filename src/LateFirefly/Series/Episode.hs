module LateFirefly.Series.Episode (episodeWidget) where

import LateFirefly.Widget.Prelude
import LateFirefly.TheOffice.Schema
import LateFirefly.DB
import LateFirefly.RPC.TH
import Data.List as L
import LateFirefly.Series.Rules
import Data.Generics.Product

episodeWidget :: Text -> HtmlM Html
episodeWidget epCode = do
  Episode{..} <- $(remote 'getEpisode) epCode
  pure do
    let Theme{..} = theme
    (holdUniqDyn -> linkIdx, modifyIdx) <- liftIO (newDyn 0)
    divClass "episode-root" do
      h3_ [ht|Episode #{code}|]
      ulClass "tabs" $ for_ (L.zip links [0..]) \(_, idx) -> do
        li_ do
          dynClassList [("active", (fmap (==idx) linkIdx))]
          a_ do
            "href" =: "javascript:void 0"
            on_ "click" do
              liftIO $ sync $ modifyIdx (const idx)
            [ht|Server #{showt (idx + 1)}|]
      iframe_ do
        "referrerpolicy" `attr` "no-referrer"
        "scrolling" `attr` "no"
        "allowfullscreen" `attr` "true"
        "frameborder" `attr` "0"
        "style" =: [st|width: 900px; height: 600px|]
        "src" ~: ((links L.!!) <$> linkIdx)
      p_ (text description)
    [style|
      .episode-root
        max-width: 900px
        margin: 0 auto
        .tabs
          display: flex
          margin: 0
          padding: 0
          border-bottom: solid 2px #{primary}
          margin-bottom: #{unit}
          & > li
            list-style: none
            padding: #{unit} #{unit * 2}
        li.active
          background: #{primary}
          a
            color: white
        li a
          text-decoration: none
          color: #{primaryText}
    |]

getEpisode :: (?conn :: Connection) => Text -> IO Episode
getEpisode epCode = do
  episode <- L.head <$> flip query [epCode] [sql|
    select e.* from `episode` e
      left join `season` s on e.season_id=s.uuid
    where e.`code`=?
  |]
  links <- applyLinkRule' "iwatchtheoffice.com" $ getField @"links" episode
  pure episode {links=links}
