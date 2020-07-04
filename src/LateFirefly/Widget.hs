{-# OPTIONS_GHC -Wno-orphans #-}
module LateFirefly.Widget where

import Control.Monad.Reader

import Data.Text as T
import GHC.Word
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Javascript.JSaddle
import LateFirefly.Prelude
import Massaraksh.Internal as H
import Massaraksh.Text as H
import Text.Cassius
import Text.Shakespeare.Text as X (st)
import qualified Data.Text.Lazy as LT

data Theme = Theme
  { unit            :: PixelSize
  , primary         :: RGBA
  , primaryText     :: RGBA
  , secondaryText   :: RGBA
  , borderColor     :: RGBA
  , thumbnailHeight :: PixelSize
  , pageWidth       :: PixelSize }

theme = Theme
  { unit          = PixelSize 8
  , primary       = RGBA 203 121 34 1
  , primaryText   = RGBA 0 0 0 0.87
  , secondaryText = RGBA 0 0 0 0.54
  , borderColor   = RGBA 0 0 0 0.20
  , thumbnailHeight = PixelSize 150
  , pageWidth       = PixelSize 1190 }

unPixelSize (PixelSize x) = x

style :: QuasiQuoter
style = cassius{quoteExp=qExp} where
  qExp = appE [|H.el "style" . (H.prop "type" ("text/css" :: T.Text) *>) . H.text . LT.toStrict . renderCss . ($ undefined)|] . quoteExp cassius

elementSize' :: Element -> Modifier (Maybe (Int, Int)) -> Html
elementSize' elm modSize = do
  win <- liftJSM $ jsg ("window"::Text)
  let
    handleResize = do
      w::Int <- fromJSValUnchecked =<< elm ! ("clientWidth"::Text)
      h::Int <- fromJSValUnchecked =<< elm ! ("scrollWidth"::Text)
      liftIO $ sync $ modSize $ const $ Just (w, h)
  onEvent_ (coerce win) "resize" $ liftJSM handleResize
  liftJSM $ setTimeout 0 $ handleResize

elementSize :: Modifier (Maybe (Int, Int)) -> Html
elementSize f = flip elementSize' f =<< askElement

newSizeDyn :: HtmlM (Dynamic (Maybe (Int, Int)))
newSizeDyn = do
  (dSize, modSize) <- liftIO (newDyn Nothing)
  dSize <$ elementSize modSize

setTimeout :: Int -> JSM () -> JSM ()
setTimeout delay f = mdo
  cb <- function $ fun \_ _ _ -> freeFunction cb *> f
  void $ call (jsg ("setTimeout"::Text)) jsUndefined $ (cb, delay)

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
