module JavaScript.Web.XMLHttpRequest where

import Language.Javascript.JSaddle hiding (textFromJSString)

newtype XHR = XHR JSVal
