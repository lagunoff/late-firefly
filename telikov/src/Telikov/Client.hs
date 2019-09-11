{-# LANGUAGE CPP #-}
module Main where

import GHCJS.DOM.Types (JSM)
import qualified Telikov.Home as Home
#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp (run)
#endif

mainClient :: JSM ()
mainClient = Home.main =<< Home.init

main :: IO ()
#ifdef __GHCJS__
main = mainClient
#else
main = run 3708 mainClient
#endif
