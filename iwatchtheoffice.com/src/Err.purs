module Err where

import Affjax (ResponseFormatError)
import Control.Monad.Except (ExceptT, except, withExceptT)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Prelude ((#), (>>=))

data Err
  = GenericErr Error
  | ResponseFormatError ResponseFormatError
  | EmptySelectorResults { url :: String, selector :: String } 
  | EmptyAttribute { url :: String, attribute :: String } 


liftA0
  ∷ ∀ m e a
  . MonadAff m
  ⇒ (e → Err)
  → Aff (Either e a)
  → ExceptT Err m a
liftA0 projLeft mea = mea # liftAff >>= except # withExceptT projLeft
