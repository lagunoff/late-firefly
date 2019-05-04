{-# LANGUAGE GADTs, OverloadedStrings, DataKinds, ScopedTypeVariables, FlexibleContexts #-}
module SDOM where

import Haste.Foreign
import Haste.Prim
import Data.OpenUnion
import TypeFun.Data.List (Delete)
import Data.Typeable
import Haste.DOM.JSString (Elem, newTextElem, newElem, setAttr, appendChild, setProp)
import System.IO.Unsafe (unsafePerformIO)


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


create :: i -> SDOM i o -> IO Elem
create _ (SDOMText content) = newTextElem content
create i (SDOMTextDyn f) = newTextElem (f i)
create i (SDOMDimap coproj _ child) = create (coproj i) child
create i (SDOMUnion left right) = case restrict i of
  Right (i' :: here) -> create i' left
  Left i' -> create i' right
create i (SDOMElement name attrs childs) = do
  el <- newElem name
  mapM_ (\sdom -> applyAttr sdom el i) attrs
  mapM_ (\ch -> create i ch >>= appendChild el) childs
  pure el

applyAttr :: SDOMAttr i o -> Elem -> i -> IO ()
applyAttr (SDOMAttr apply) el _ = apply el
applyAttr (SDOMAttrDyn update) el input = update input input el
applyAttr (SDOMEvent _ _) _ _ = undefined

union :: (Typeable a, Typeable (Union (Delete a s))) => SDOM a o -> SDOM (Union (Delete a s)) o -> SDOM (Union s) o
union = SDOMUnion
infixr 4 `union`

unionExhausted :: SDOM (Union '[]) o
unionExhausted = undefined

dimap :: (i' -> i) -> (o -> o') -> SDOM i o -> SDOM i' o'
dimap = SDOMDimap

instance Monoid JSString where
  mempty = jsEmptyString 
  mappend = jsAppendString

jsEmptyString :: JSString
jsEmptyString = unsafePerformIO impl
  where
    impl :: IO JSString
    impl = ffi "\"\""
    
jsAppendString :: JSString -> JSString -> JSString
jsAppendString a b = unsafePerformIO $ impl a b
  where
    impl :: JSString -> JSString -> IO JSString
    impl = ffi "(function (a, b) { return a + b; })"
