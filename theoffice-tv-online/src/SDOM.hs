{-# LANGUAGE GADTs, OverloadedStrings, DataKinds, ScopedTypeVariables, FlexibleContexts, MagicHash #-}
module SDOM where

import Haste.Foreign
import Haste.Prim
import Data.OpenUnion
import TypeFun.Data.List (Delete)
import Data.Typeable
import Haste.DOM.JSString (Elem, newTextElem, newElem, appendChild)
import qualified Haste.JSString as JSS
import Control.Monad (forM_)
import GHC.Exts

data SDOM i o where
  SDOMText :: JSString -> SDOM i o
  SDOMTextDyn :: (i -> JSString) -> SDOM i o
  SDOMElement :: JSString -> [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
  SDOMDimap :: (i' -> i) -> (o -> o') -> SDOM i o -> SDOM i' o'
  SDOMUnion :: (Typeable here, Typeable (Union (Delete here s))) => SDOM here o -> SDOM (Union (Delete here s)) o -> SDOM (Union s) o

data SDOMAttr i o where
  SDOMAttr :: (Elem -> IO ()) -> SDOMAttr i o
  SDOMAttrDyn :: (i -> i -> Elem -> IO ()) -> SDOMAttr i o
  SDOMEvent :: JSString -> (JSAny -> Maybe o) -> SDOMAttr i o


on_ :: JSString -> (JSAny -> Maybe o) -> SDOMAttr i o
on_ = SDOMEvent


create :: i -> SDOM i o -> IO Elem
create _ (SDOMText content) = newTextElem content
create i (SDOMTextDyn f) = newTextElem (f i)
create i (SDOMDimap coproj proj child) = do
  el <- create (coproj i) child
  attachMeta (unsafeCoerce# proj) el
  pure el
create i (SDOMUnion left right) = case restrict i of
  Right i' -> create i' left
  Left i' -> create i' right
create i (SDOMElement name attrs childs) = do
  el <- newElem name
  forM_ attrs $ \attr -> applyAttr attr el i
  forM_ childs $ \ch -> create i ch >>= appendChild el
  pure el

create_ :: (o -> IO ()) -> i -> SDOM i o -> IO Elem
create_ handler input sdom = do
  el <- create input sdom
  attachMeta (unsafeCoerce# handler) el
  pure el
  
actuate :: i -> i -> Elem -> SDOM i o -> IO Elem
actuate _ _ el (SDOMText _) = pure el
actuate prev next el (SDOMTextDyn f) = setNodeValue (f next) el >> pure el
actuate prev next el (SDOMDimap coproj _ child) = actuate (coproj prev) (coproj next) el child
actuate prev next el sdom@(SDOMUnion left right) = case (restrict prev, restrict next) of
  (Right prev', Right next') -> actuate prev' next' el left
  (Left _,  Right next') -> create next' left
  (Left prev',  Left next') -> actuate prev' next' el right
  (Right _,  Left next') -> create next' right
actuate prev next el (SDOMElement _ attrs childs) = do
  forM_ attrs $ \attr -> updateAttr prev next attr el
  forM_ (indexed childs) $ \(key, ch) -> do
    maybeCh <- childAt key el
    case maybeCh of
      Nothing -> pure ()
      Just childEl -> do
        newChildEl <- actuate prev next childEl ch
        replaceChild childEl newChildEl el
  pure el

applyAttr :: SDOMAttr i o -> Elem -> i -> IO ()
applyAttr (SDOMAttr apply) el _ = apply el
applyAttr (SDOMAttrDyn update) el input = update input input el
applyAttr (SDOMEvent name readMsg) el _ = addEventListener name (unsafeCoerce# readMsg) el
  where
    addEventListener :: JSString -> (JSAny -> Maybe JSAny) -> Elem -> IO ()
    addEventListener = ffi "(function(name, readMessage, el) {\
                            \  el.addEventListener(name, function(event) {\
                            \    var iter = event.target;\
                            \    var action = readMessage(event); if (!action) return;\
                            \    for (; iter; iter = iter.parentElement) {\
                            \      var nodeData = iter._meta; if (!nodeData) continue;\
                            \      if ('proj' in nodeData) action = nodeData.proj(action);\
                            \    }\
                            \  });\
                            \})"

updateAttr :: i -> i -> SDOMAttr i o -> Elem -> IO ()
updateAttr _ _ (SDOMAttr _) _                = pure ()
updateAttr prev next (SDOMAttrDyn update) el = update prev next el
updateAttr _ _ (SDOMEvent _ _) _             = pure ()

union :: (Typeable a, Typeable (Union (Delete a s))) => SDOM a o -> SDOM (Union (Delete a s)) o -> SDOM (Union s) o
union = SDOMUnion
infixr 4 `union`

unionExhausted :: SDOM (Union '[]) o
unionExhausted = undefined

dimap :: (i' -> i) -> (o -> o') -> SDOM i o -> SDOM i' o'
dimap = SDOMDimap

instance Monoid JSString where
  mempty = JSS.empty
  mappend = JSS.append

setNodeValue :: JSString -> Elem -> IO ()
setNodeValue =
  ffi "(function(value, el) {\
  \  if (!(el instanceof Text)) return;\
  \  el.nodeValue = value;\
  \})"

replaceChild :: Elem -> Elem -> Elem -> IO ()
replaceChild =
  ffi "(function(prev, next, parent) {\
  \  if (prev.parentNode !== parent) return;\
  \  if (prev !== next) parent.replaceChild(next, prev);\
  \})"

childAt :: Int -> Elem -> IO (Maybe Elem)
childAt =
  ffi "(function(index, el) {\
  \  return el.childNodes[index] || null;\
  \})"

attachMeta :: (JSAny -> JSAny) -> Elem -> IO ()
attachMeta = ffi "(function(proj, el) {\
  \  el._meta = el._meta || {};\
  \  el._meta.proj = proj;\
  \})"

indexed :: [a] -> [(Int, a)]
indexed xs = go 0# xs
  where
    go i (a:as) = (I# i, a) : go (i +# 1#) as
    go _ _ = []

