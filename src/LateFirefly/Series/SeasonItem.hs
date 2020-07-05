{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module LateFirefly.Series.SeasonItem where

import LateFirefly.Widget.Prelude
import LateFirefly.TheOffice.Schema
import LateFirefly.Router
import LateFirefly.Icons
import Data.Text as T
import Data.Char


seasonItemWidget :: [(Season, [Episode])] -> Html
seasonItemWidget seasons = do
  let
    Theme{..} = theme
    thumbnailHeight = thumbnailWidth * 2 / 3
    chevronWidth = unit * 5
    gap = unit
    unGap = unPixelSize gap
    scrollRelTo cw sw = max 0 . min (sw - cw) . (+ (cw + round unGap))
  for_ seasons \(Season{..}, episodes) -> mdo
    (size, mSize) <- liftIO (newDyn (Nothing::Maybe (Int, Int)))
    (scrollLeft, mScrollLeft) <- liftIO (newDyn (0::Int))
    div_ do
      "className" =: "season"
      linkTo (SeasonR (coerce number)) do
        "className" =: "season-header-link"
        h3Class "season-header"
          [ht|Season #{showt number}|]
      divClass "wrapper" do
        ul_ do
          "className" =: "episodes-list"
          "scrollLeft" ~: scrollLeft
          elementSize mSize
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
          "tabindex" `attr` "-1"
          toggleAttr "disabled" $ fmap (==0) scrollLeft
          chevronLeft_
          on_ "click" $ liftIO do
            dyn_read size >>= mapM_ \(cw, sw) -> sync $
              mScrollLeft (scrollRelTo (-cw) sw)
        buttonClass "chevron chevron-right" do
          "tabindex" `attr` "-1"
          toggleAttr "disabled" $
            liftA2 ((fromMaybe False .) . liftA2 \x (cw, sw) -> x >= sw - cw)
              (fmap Just scrollLeft) size
          chevronRight_
          on_ "click" $ liftIO do
            dyn_read size >>= mapM_ \(cw, sw) -> sync $
              mScrollLeft (scrollRelTo cw sw)
    blank
  [style|
    .season
      .link
        text-decoration: none
        color: #{primaryText}
        text-decoration: none
        h4
          margin: 0
          font-size: 14px
          font-weight: 600
          color: #{primaryText}
          &:hover
            color: #{primary}
      h3
        text-transform: uppercase
        font-weight: 400
        font-size: 16px
        margin-bottom: 8px
        margin-top: #{unit * 3}
      .season-header-link
        text-decoration: none
      .episodes-list::-webkit-scrollbar
        display: none
      .episodes-list
        width: 100%
        overflow-x: scroll
        -ms-overflow-style: none
        scrollbar-width: none
        display: flex
        padding: 0
        scroll-behavior: smooth
        li
          list-style: none
          img
            border-radius: #{thumbnailBorderRadius}
            object-fit: cover
        li + li
          margin-left: #{gap}
      .chevron
        outline: none
        position: absolute
        top: #{(thumbnailHeight / 2) - (chevronWidth / 2)}
        z-index: 10
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
          opacity: 1
        &[disabled]
          opacity: 0.65
        &[disabled]:hover
          box-shadow: 0 4px 6px 0 rgba(0,0,0,0.05), 0 1px 0 1px rgba(0,0,0,0.05), 0 0 0 1px rgba(0,0,0,0.05)
      .chevron svg
        display: block
      .chevron-left
        left: -#{chevronWidth *  0.5}
      .chevron-right
        right: -#{chevronWidth *  0.5}
      .placeholder > *
        width: 300px
        height: 200px
        border-radius: #{thumbnailBorderRadius}
        background: rgba(155, 147, 127, 0.06)
      .wrapper
        position: relative
  |]
  where
    episodeNumber = T.dropWhile (=='0') . T.takeWhileEnd isDigit

-- #ifndef __GHCJS__
-- scrollHoriz :: Int -> Element -> JSM ()
-- scrollHoriz dir elm = do
--   fn <- eval [st|
--     (function(dir, elm) {
--       if (dir > 0) elm.scrollLeft += elm.clientWidth;
--       else elm.scrollLeft -= elm.clientWidth;
--     })|]
--   void $ call fn jsUndefined (dir, elm)
-- #else
-- foreign import javascript unsafe
--   "if ($1 > 0) { $2.scrollLeft += $2.clientWidth; } else { $2.scrollLeft -= $2.clientWidth; }"
--   scrollHoriz :: Int -> Element -> JSM ()
-- #endif
