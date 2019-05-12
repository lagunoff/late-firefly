{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SDOM
  ( PModel(..)
  , Sink
  , Last
  , SDOMInst(..)
  , SDOM(..)
  , SDOMAttr(..)
  , unSDOM
  , on_
  , attach
  , actuate
  , text_
  , textDyn
  , node
  , union
  , unionExhausted
  , list_
  , dimap
  ) where

import           Control.Monad      (forM_)
import           Data.Bifunctor
import           Data.IORef
import           Data.OpenUnion
import           Data.Typeable
import           GHC.Exts
import           Haste.DOM.JSString (Elem, appendChild, newElem, newTextElem)
import           Haste.Foreign
import qualified Haste.JSString     as JSS
import           Haste.Prim
import           Prelude            hiding (last)
import           TypeFun.Data.List  (Delete)

data PModel parent item = PModel { parent :: parent, item :: item }
type Sink a = a -> IO ()
type Last model = IO (Maybe (model, Elem))

data SDOMInst model msg = SDOMInst
  { instModel :: IORef model
  , instElem  :: IORef (Maybe Elem)
  , instLast  :: Last model
  , instSink  :: Sink msg
  , instSDOM  :: SDOM model msg
  }

newtype SDOM model msg
  = SDOM (Sink msg -> Last model -> model -> IO Elem)

unSDOM :: SDOM model msg -> Sink msg -> Last model -> model -> IO Elem
unSDOM (SDOM f) = f

data SDOMAttr i o where
  SDOMAttr :: (Elem -> IO ()) -> SDOMAttr i o
  SDOMAttrDyn :: (i -> i -> Elem -> IO ()) -> SDOMAttr i o
  SDOMEvent :: JSString -> (JSAny -> i -> Maybe o) -> SDOMAttr i o

on_ :: JSString -> (JSAny -> i -> Maybe o) -> SDOMAttr i o
on_ = SDOMEvent

mapLast :: (a -> b) -> Last a -> Last b
mapLast = fmap . fmap . first

mkLast :: Elem -> a -> Last a
mkLast el model = pure $ Just (model, el)

attach
  :: forall model msg
   . model
  -> Sink msg
  -> Elem
  -> SDOM model msg
  -> IO (SDOMInst model msg)
attach model sink root sdom = do
  modelRef <- newIORef model
  elRef <- newIORef Nothing
  let last :: Last model
      last = do
        maybeEl <- readIORef elRef
        m <- readIORef modelRef
        pure $ maybe Nothing (\el -> Just (m, el)) maybeEl
  el <- unSDOM sdom sink last model
  writeIORef elRef (Just el)
  appendChild root el
  pure $ SDOMInst modelRef elRef last sink sdom

actuate
  :: forall model msg
   . SDOMInst model msg
  -> (model -> model)
  -> IO ()
actuate (SDOMInst modelRef elRef last sink sdom) step = readIORef elRef >>= \case
  Just oldEl -> do
    old <- readIORef modelRef
    let new = step old
    newEl <- unSDOM sdom sink last new
    writeIORef modelRef new
    writeIORef elRef $ Just newEl
    unsafePtrEq oldEl newEl >>= \case
      True -> pure ()
      False -> replaceChild oldEl newEl
  Nothing -> pure ()

text_ :: JSString -> SDOM model msg
text_ content = SDOM $ \_ last _ -> last >>= \case
  Just (_, el) -> pure el
  Nothing      -> newTextElem content

textDyn :: (model -> JSString) -> SDOM model msg
textDyn mkContent = SDOM $ \_ last model -> last >>= \case
  Just (_, el) -> setNodeValue (mkContent model) el >> pure el
  Nothing      -> newTextElem (mkContent model)

dimap :: (i' -> i) -> (o -> o') -> SDOM i o -> SDOM i' o'
dimap coproj proj sdom = SDOM $ \sink last model -> do
  unSDOM sdom (sink . proj) (fmap (first coproj) <$> last) (coproj model)

node :: JSString -> [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
node name attrs childs = SDOM $ \sink last model -> last >>= \case
  Nothing -> do
    el <- newElem name
    forM_ attrs $ applyAttr sink last el model
    forM_ childs $ \ch -> unSDOM ch sink last model >>= appendChild el
    pure el
  Just (old, el) -> do
    forM_ attrs $ \attr -> updateAttr old model attr el
    forM_ (indexed childs) $ \(key, ch) -> childAt key el >>= \case
      Just childEl -> do
        newChildEl <- unSDOM ch sink (pure $ Just (old, childEl)) model
        unsafePtrEq childEl newChildEl >>= \case
          True -> pure ()
          False -> replaceChild childEl newChildEl
      Nothing -> pure ()
    pure el

applyAttr :: forall i o. Sink o -> Last i -> Elem -> i -> SDOMAttr i o -> IO ()
applyAttr _ _ el _ (SDOMAttr apply) = apply el
applyAttr _ _ el input (SDOMAttrDyn update) = update input input el
applyAttr sink last el _ (SDOMEvent name readMsg) = last >>= \case
  Nothing -> addEventListener (sink . fromPtr) (mapLast toPtr last) (\e i -> toPtr <$> readMsg e (fromPtr i)) name el
  Just _ -> pure ()

updateAttr :: i -> i -> SDOMAttr i o -> Elem -> IO ()
updateAttr _ _ (SDOMAttr _) _                = pure ()
updateAttr prev next (SDOMAttrDyn update) el = update prev next el
updateAttr _ _ (SDOMEvent _ _) _             = pure ()

union
  :: forall a s o
   . (Typeable a, Typeable (Union (Delete a s)))
  => SDOM a o
  -> SDOM (Union (Delete a s)) o
  -> SDOM (Union s) o
union left right = SDOM $ \sink last new -> last >>= \case
  Nothing -> case restrict new of
    Right i' -> unSDOM left sink (pure Nothing) i'
    Left  i' -> unSDOM right sink (pure Nothing) i'
  Just (old, el) -> case (restrict old, restrict new) of
    (Right old', Right new') -> unSDOM left sink (mkLast el old') new'
    (Left _, Right new')     -> unSDOM left sink (pure Nothing) new'
    (Left old', Left new')   -> unSDOM right sink (mkLast el old') new'
    (Right _, Left new')     -> unSDOM right sink (pure Nothing) new'

infixr 4 `union`

unionExhausted :: SDOM (Union '[]) o
unionExhausted = undefined

list_ :: JSString -> [SDOMAttr i o] -> (i -> [a]) -> SDOM (PModel i a) o -> SDOM i o
list_ name attrs coproj sdom = SDOM $ \sink last model -> last >>= \case
  Nothing -> do
    el <- newElem name
    forM_ attrs $ applyAttr sink last el model
    forM_ (coproj model) $ \ch -> unSDOM sdom sink (pure Nothing) (PModel model ch) >>= appendChild el
    pure el
  Just (_, el) -> do
    pure el

instance Monoid JSString where
  mempty = JSS.empty
  mappend = JSS.append

indexed :: [a] -> [(Int, a)]
indexed xs = go 0# xs
  where
    go i (a:as) = (I# i, a) : go (i +# 1#) as
    go _ _      = []

unsafePtrEq :: (ToAny a) => a -> a -> IO Bool
unsafePtrEq = ffi "(function(a, b) { return a === b; })"

setNodeValue :: JSString -> Elem -> IO ()
setNodeValue =
  ffi "(function(value, el) {\
  \  if (!(el instanceof Text)) return;\
  \  el.nodeValue = value;\
  \})"

replaceChild :: Elem -> Elem -> IO ()
replaceChild =
  ffi "(function(prev, next) {\
  \  if (!prev.parentNode) return;\
  \  if (prev !== next) prev.parentNode.replaceChild(next, prev);\
  \})"

childAt :: Int -> Elem -> IO (Maybe Elem)
childAt =
  ffi "(function(index, el) {\
  \  return el.childNodes[index] || null;\
  \})"

addEventListener
  :: Sink (Ptr o)
  -> Last (Ptr i)
  -> (JSAny -> Ptr i -> Maybe (Ptr o))
  -> JSString
  -> Elem
  -> IO ()
addEventListener =
  ffi "(function(sink, last, readMessage, name, el) {\
  \  el.addEventListener(name, function(event) {\
  \    var lastElModel = last(); if (!lastElModel) return;\
  \    var action = readMessage(event, lastElModel[0]); if (!action) return;\
  \    sink(action);\
  \  });\
  \})"
