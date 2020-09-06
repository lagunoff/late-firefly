{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}
module Router.TH where

import Control.Error
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Catch as Catch
import qualified Control.Monad.Except as E
import Data.ByteString
import Data.Bifunctor
import Data.Coerce
import Data.Either
import Data.IORef
import Data.Maybe
import Data.Text
import Data.Typeable
import Data.Text.Encoding (decodeUtf8)
import Flat
import GHC.Fingerprint.Type
import GHC.StaticPtr
import GHC.Int
import JavaScript.Web.XMLHttpRequest as XHR
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Javascript.JSaddle
import "this" DB
import System.IO.Unsafe
import Unsafe.Coerce
import "this" Utils as Utils
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map as M
import qualified Data.Set as S
import "this" Eio

derivePage :: Name -> Name -> Name -> Q Exp
derivePage w i r = do
  dbTableInst <- instanceD (pure []) (conT ''IsPage `appT` conT n) [columnsD]
