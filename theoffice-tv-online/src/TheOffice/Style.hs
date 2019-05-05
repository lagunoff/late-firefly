{-# LANGUAGE OverloadedStrings #-}
module TheOffice.Style where

import Data.String (IsString, fromString)
import Data.Monoid ((<>))
import Stitch

data Hsla = Hsla
  { hue :: Float
  , saturation :: Float
  , lightness :: Float
  , alpha :: Float
  } deriving (Show, Eq)

toCss :: (IsString s, Monoid s) => Hsla -> s
toCss (Hsla h s l a) = "hsla(" <> fromString (show h) <> ", " <> fromString (show s) <> "%, " <> fromString (show l) <> "%, " <> fromString (show a) <> ")"

data Theme = Theme
  { colorBorder :: Hsla
  , colorText :: Hsla
  , colorTextSecondary :: Hsla
  , colorAccent :: Hsla
  , colorSecondary :: Hsla
  , unit :: Float
  , pageWidth :: Float
  }

theme :: Theme
theme = Theme
  { colorBorder = Hsla 0 0 0 0.2
  , colorText = Hsla 0 0 0 0.87
  , colorTextSecondary = Hsla 0 0 0 0.54
  , colorAccent = Hsla 208 100 50 1
  , colorSecondary = Hsla 0 100 50 1
  , unit = 8
  , pageWidth = 860
  }

linkStyles :: CSS
linkStyles = do
  "color" .= toCss (colorAccent theme)
  "text-decoration" .= "none"
  "&:hover" ? do
    "color" .= toCss (lighten 1.3 $ colorAccent theme)
  "&:active" ? do
    "color" .= toCss (lighten 1.3 $ colorSecondary theme)

linkHoverSecondary :: CSS
linkHoverSecondary = do
  linkStyles
  "&:hover" ? do
    "color" .= toCss (lighten 1.3 $ colorSecondary theme)

lighten :: Float -> Hsla -> Hsla
lighten scale hsla = hsla { lightness = bound 0 100 $ lightness hsla * scale }

fade :: Float -> Hsla -> Hsla
fade scale hsla = hsla { alpha = bound 0 1 $ alpha hsla * scale }

saturate :: Float -> Hsla -> Hsla
saturate scale hsla = hsla { saturation = bound 0 100 $ saturation hsla * scale }

bound :: Float -> Float -> Float -> Float
bound minVal maxVal = max minVal . min maxVal
