module LateFirefly.Diff where

import Data.Generics.Product

data GPatch a where
  Noop   :: GPatch a
  New    :: a -> GPatch a
  Concat :: GPatch a -> GPatch a -> GPatch a
  Field  :: (HasField' f s a, Diff a) => Patch a -> GPatch s

class Diff a where
  type Patch a
  diff :: a -> a -> Patch a
  patch :: Patch a -> a -> a
