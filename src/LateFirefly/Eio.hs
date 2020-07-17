module LateFirefly.Eio where

import Control.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Error
import Data.Default
import GHC.Base

newtype Eio e a = Eio {unEio :: IO a}
  deriving newtype
    ( Alternative, Applicative, Functor, Monad, MonadPlus, Monoid, Semigroup
    , MonadIO, Default, MonadFix )

liftEither :: Exception e => Either e b -> Eio e b
liftEither = either (Eio . throwIO) (Eio . pure)
{-# INLINE liftEither #-}

runEio :: Exception e => Eio e b -> IO (Either e b)
runEio (unEio -> io) = fmap Right io `catch` (pure . Left)
{-# INLINE runEio #-}

liftIOWith :: (Exception e, Exception e') => (e -> e') -> IO a -> Eio e' a
liftIOWith f io = Eio $ io `catch` (throwIO . f)
{-# INLINE liftIOWith #-}

mapEio
  :: (Exception e, Exception e')
  => (Either e a -> Either e' b) -> Eio e a -> Eio e' b
mapEio f (unEio -> io) = Eio io' where
  io' = (io >>= either throwIO pure . f . Right)
    `catch` (either throwIO pure . f . Left)
{-# INLINE mapEio #-}

withEio :: (Exception e, Exception e') => (e -> e') -> Eio e a -> Eio e' a
withEio f (unEio -> io) = Eio io' where
  io' = io `catch` (throwIO . f)
{-# INLINE withEio #-}

throwE :: Exception e => e -> Eio e a
throwE = Eio . throwIO
{-# INLINE throwE #-}

catchE :: (Exception e, Exception e') => Eio e a -> (e -> Eio e' a) -> Eio e' a
catchE (Eio io) h = Eio $ io `catch` (unEio . h)
{-# INLINE catchE #-}

instance Exception e => MonadError e (Eio e) where
  throwError = throwE
  catchError = catchE
