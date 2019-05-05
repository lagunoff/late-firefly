{-# LANGUAGE OverloadedStrings #-}
module SDOM.Html.Dynamic where

import Haste.Prim
import SDOM

text_ :: (i -> JSString) -> SDOM i o
text_ = SDOMTextDyn
