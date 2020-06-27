{-# OPTIONS_GHC -Wno-orphans #-}
module LateFirefly.Widget where

import Data.Text as T
import GHC.Word
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Massaraksh.Text as H
import Text.Cassius
import Text.Shakespeare.Text as X (st)
import TextShow
import qualified Data.Text.Lazy as LT

data Theme = Theme
  { unit            :: PixelSize
  , primary         :: RGBA
  , primaryText     :: RGBA
  , secondaryText   :: RGBA
  , thumbnailHeight :: PixelSize
  , pageWidth       :: PixelSize }

theme = Theme
  { unit          = PixelSize 8
  , primary       = RGBA 203 121 34 1
  , primaryText   = RGBA 0 0 0 0.87
  , secondaryText = RGBA 0 0 0 0.54
  , thumbnailHeight = PixelSize 170
  , pageWidth       = PixelSize 1600 }

unPixelSize (PixelSize x) = x

style :: QuasiQuoter
style = cassius{quoteExp=qExp} where
  qExp = appE [|H.el "style" . (H.prop "type" ("text/css" :: T.Text) *>) . H.text . LT.toStrict . renderCss . ($ undefined)|] . quoteExp cassius

ht :: QuasiQuoter
ht = st{quoteExp=qExp} where
  qExp = appE [|H.text |] . quoteExp st

data RGBA = RGBA Word8 Word8 Word8 Double

instance ToCss RGBA where
  toCss (RGBA r g b a) = fromText $ "rgba(" <> showt r <> ", " <> showt g <> ", " <> showt b <> ", " <> showt a <> ")"

instance TextShow PixelSize where
  showb = toCss

instance TextShow RGBA where
  showb = toCss
