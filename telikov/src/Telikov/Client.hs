{-# LANGUAGE CPP #-}
module Main where

import GHCJS.DOM.Types (JSM)
import qualified Telikov.Home as Home

mainClient :: JSM ()
mainClient = Home.main =<< Home.init

main :: IO ()
#ifdef __GHCJS__
main = mainClient
#else
main = error "Telicov.Client.main: works only in GHCJS"
#endif
