module Telikov.Styles where

import Data.Word (Word8)
import qualified Data.Text as T
import Text.Lucius (ToCss(..))

data Rgba = Rgba Word8 Word8 Word8 Double
  deriving (Eq, Show)

data Theme = Theme
  { unit :: Double
  , primaryColor :: Rgba
  , secondaryColor :: Rgba
  , primaryText :: Rgba
  , secondaryText :: Rgba
  , borderColor :: Rgba
  , bodyPadding :: Double
  }

theme :: Theme
theme = Theme
  { unit = 8
  , bodyPadding = 8 * 3
  , primaryColor = Rgba 0 0 255 0
  , secondaryColor = Rgba 255 0 0 0
  , primaryText = Rgba 0 0 0 0.87
  , secondaryText = Rgba 0 0 0 0.54
  , borderColor = Rgba 0 0 0 0.12
  }

instance ToCss Double where
  toCss = toCss . T.pack . show

instance ToCss Rgba where
  toCss (Rgba r g b a) = toCss $ T.pack $ "rgba(" <> show r <> "," <> show g <> "," <> show b <> "," <> show a <> ")"
