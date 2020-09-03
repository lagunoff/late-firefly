{-# LANGUAGE NoOverloadedStrings #-}
module LateFirefly.JSaddle where

import Language.Javascript.JSaddle
import Data.Foldable
import Prelude hiding ((!!))

removeJsaddleJs :: JSM ()
removeJsaddleJs = do
  mScr::Maybe JSVal <- fromJSValUnchecked =<< (jsg "document" ! "body" # "querySelector" $ ["script"])
  for_ mScr \ch -> do
    jsg "document" ! "body" # "removeChild" $ ch
