{-# OPTIONS_GHC -Wno-orphans #-}
module Widget.Base where

import Data.Text as T
import GHC.Word
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Lucid.Base
import Text.Cassius
import Text.Shakespeare.Text as X (st)
import qualified Data.Text.Lazy as LT

import "this" Intro

data Theme = Theme
  { unit           :: PixelSize
  , primary        :: RGBA
  , primaryText    :: RGBA
  , secondaryText  :: RGBA
  , borderColor    :: RGBA
  , thumbnailWidth :: PixelSize
  , pageWidth      :: PixelSize
  , thumbnailBorderRadius :: PixelSize
  }

theme = Theme{..} where
  unit           = PixelSize 8
  primary        = RGBA 203 121 34 1
  primaryText    = RGBA 0 0 0 0.87
  secondaryText  = RGBA 0 0 0 0.54
  borderColor    = RGBA 0 0 0 0.20
  thumbnailWidth = PixelSize 225
  pageWidth      = (thumbnailWidth + unit) * 5 - unit
  thumbnailBorderRadius = PixelSize 5

unPixelSize (PixelSize x) = x

style :: QuasiQuoter
style = cassius{quoteExp=qExp} where
  qExp = appE [|termRaw "style" [type_ "text/css"] . toHtmlRaw . LT.toStrict . renderCss . ($ undefined)|] . quoteExp cassius

js :: QuasiQuoter
js = st{quoteExp=qExp} where
  qExp = appE [|script_ []|] . quoteExp st

ht :: QuasiQuoter
ht = st{quoteExp=qExp} where
  qExp = appE [|toHtml |] . quoteExp st

data RGBA = RGBA Word8 Word8 Word8 Double

instance ToCss RGBA where
  toCss (RGBA r g b a) = fromText $
    "rgba(" <> showt r <> ", " <> showt g <> ", " <> showt b <> ", " <> showt a <> ")"

instance TextShow PixelSize where
  showb = toCss

instance TextShow RGBA where
  showb = toCss

fun_ :: Text -> [Text] -> Text -> Html ()
fun_ name args body = do
  script_ [] $ "function " <> name <> "(" <> T.intercalate ", " args <> "){\n" <> body <> "\n}"
