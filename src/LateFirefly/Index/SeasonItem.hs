{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module LateFirefly.Index.SeasonItem where

import LateFirefly.Widget.Prelude
import LateFirefly.TheOffice.Schema
import LateFirefly.Router
import LateFirefly.Icons
import Data.Text as T
import Data.Char
import Language.Javascript.JSaddle

seasonItemWidget :: [(Season, [Episode])] -> Html
seasonItemWidget seasons = do
  let
    Theme{..} = theme
    thumbnailWidth = thumbnailHeight * 3 / 2
    chevronWidth = unit * 5
  for_ seasons \(Season{..}, episodes) -> mdo
    el' "div" do
      "className" =: "season"
      linkTo (SeasonR (coerce number)) do
        "className" =: "season-header-link"
        h3Class "season-header"
          [ht|Season #{showt number}|]
      divClass "wrapper" do
        wrapperEl <- el' "ul" do
          "className" =: "episodes-list"
          for_ episodes \Episode{..} -> do
            li_ do
              linkTo (EpisodeR (coerce number) (coerce code)) do
                "className" =: "link"
                img_ do
                  "src" =: thumbnail
                  "style" =: [st|width: #{showt thumbnailWidth}; height: #{showt thumbnailHeight}|]
                div_ do
                  h4_ [ht|Episode #{episodeNumber code} — #{name}|]
          -- liClass "placeholder" $
          --   div_ blank
        buttonClass "chevron chevron-left" do
          chevronLeft_
          on_ "click" do
            liftJSM (scrollHoriz (-1) wrapperEl)
        buttonClass "chevron chevron-right" do
          chevronRight_
          on_ "click" do
            liftJSM (scrollHoriz 1 wrapperEl)
    blank
  [style|
    .season .wrapper:hover .chevron
      opacity: 1
    .season
      .link
        text-decoration: none
        color: #{primaryText}
        text-decoration: none
        h4
          margin: 0
          font-size: 14px
          font-weight: 400
          &:hover
            text-decoration: underline
      h3
        text-transform: uppercase
        font-weight: 400
        font-size: 16px
        margin-bottom: 8px
        margin-top: 37px
      .season-header-link
        text-decoration: none
      .episodes-list
        width: 100%
        overflow: hidden
        margin: 0
        display: flex
        padding: 0
        scroll-behavior: smooth
        li
          list-style: none
          img
            border-radius: 6px
        li + li
          margin-left: #{unit * 2}
      .chevron
        outline: none
        position: absolute
        top: #{(thumbnailHeight / 2) - (chevronWidth / 2)}
        z-index: 10
        opacity: 0
        cursor: pointer
        border: none
        padding: 0
        display: flex
        align-items: center
        justify-content: center
        width: #{chevronWidth}
        height: #{chevronWidth}
        border-radius: 50%
        background: white
        box-shadow: 0 4px 6px 0 rgba(0,0,0,0.05), 0 1px 0 1px rgba(0,0,0,0.05), 0 0 0 1px rgba(0,0,0,0.05)
        &:hover
          box-shadow: 0 10px 20px rgba(0,0,0,0.19), 0 6px 6px rgba(0,0,0,0.23);
      .chevron svg
        display: block
      .chevron-left
        left: #{unit}
      .chevron-right
        right: #{unit}
      .placeholder > *
        width: 300px
        height: 200px
        border-radius: 6px
        background: rgba(155, 147, 127, 0.06)
      .wrapper
        position: relative
  |]
  where
    episodeNumber = T.dropWhile (=='0') . T.takeWhileEnd isDigit

#ifndef __GHCJS__
scrollHoriz :: Int -> Element -> JSM ()
scrollHoriz dir elm = do
  fn <- eval [st|
    (function(dir, elm) {
      if (dir > 0) elm.scrollLeft += elm.clientWidth;
      else elm.scrollLeft -= elm.clientWidth;
    })|]
  void $ call fn jsUndefined (dir, elm)
#else
foreign import javascript unsafe
  "if ($1 > 0) { $2.scrollLeft += $2.clientWidth; } else { $2.scrollLeft -= $2.clientWidth; }"
  scrollHoriz :: Int -> Element -> JSM ()
#endif
