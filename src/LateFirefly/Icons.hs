module LateFirefly.Icons where

import Massaraksh.Text
import LateFirefly.Widget.Prelude

svgIcon :: Text -> HtmlM x -> HtmlM x
svgIcon contents attrs = do
  elNS (Just "http://www.w3.org/2000/svg") "svg" do
    "width"   `attr` "1em"
    "height"  `attr` "1em"
    "viewBox" `attr` "0 0 16 16"
    "fill"    `attr` "currentColor"
    "xmlns"   `attr` "http://www.w3.org/2000/svg"
    "innerHTML" =: contents
    attrs

chevronLeft = svgIcon [st|<path fill-rule="evenodd" d="M11.354 1.646a.5.5 0 0 1 0 .708L5.707 8l5.647 5.646a.5.5 0 0 1-.708.708l-6-6a.5.5 0 0 1 0-.708l6-6a.5.5 0 0 1 .708 0z"/>|]
chevronLeft_ = chevronLeft blank

chevronRight = svgIcon [st|<path fill-rule="evenodd" d="M4.646 1.646a.5.5 0 0 1 .708 0l6 6a.5.5 0 0 1 0 .708l-6 6a.5.5 0 0 1-.708-.708L10.293 8 4.646 2.354a.5.5 0 0 1 0-.708z"/>|]
chevronRight_ = chevronRight blank
