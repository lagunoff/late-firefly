{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module LateFirefly.Series.Season where

import LateFirefly.Widget.Prelude
import LateFirefly.TheOffice.Schema
import LateFirefly.Router
import LateFirefly.DB
import LateFirefly.RPC.TH

seasonWidget :: Int -> HtmlM Html
seasonWidget sNum = do
  episodes <- $(remote 'getEpisodes) sNum
  pure do
    let
      Theme{..} = theme
      thumbnailWidth = thumbnailHeight * 3 / 2
    divClass "season" do
      ul_ $ for_ episodes \Episode{..} -> do
        li_ do
          linkTo (EpisodeR (coerce sNum) (coerce code)) do
            h3_ do [ht|Episode #{code}|]
          divClass "row" do
            linkTo (EpisodeR (coerce sNum) (coerce code)) do
              img_ do
                "src" =: thumbnail
                "style" =: [st|width: #{showt thumbnailWidth}; height: #{showt thumbnailHeight}|]
            div_ do
              p_ (text description)
    [style|
      .season
        max-width: #{pageWidth}
        margin: 0 auto
        img
          border-radius: 0px
        h3
          text-transform: uppercase
          font-weight: 400
          font-size: 16px
          margin-bottom: 8px
          margin-top: 37px
        ul li
          list-style: none
        ul
          margin: 0 #{unit * 3}
          padding: 0
        ul .row
          display: flex
        ul .row > * + *
          margin-left: #{unit * 2}
        p
          margin: 0
    |]

getEpisodes :: (?conn :: Connection) => Int -> IO [Episode]
getEpisodes seasonNumber = do
  query [sql|
    select e.* from `episode` e
      left join `season` s on e.season_id=s.uuid
    where s.`number`=? order by e.code
  |] [seasonNumber]
