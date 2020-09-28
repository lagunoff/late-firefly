module Eio where

import Control.Lens hiding (Prism')
import Control.Monad.Catch
import Control.Monad.Except
import Data.Default
import Data.Generics.Internal.VL.Prism
import GHC.Base
import qualified Data.Generics.Sum.Internal.Typed as Core

newtype Eio e a = Eio {unEio :: IO a}
  deriving newtype
    ( Alternative, Applicative, Functor, Monad, MonadPlus, Monoid, Semigroup
    , MonadIO, Default, MonadFix, MonadThrow, MonadCatch )

newtype Y i x a = Y {unY :: i -> IO a}

liftEio :: (e' <: e, Exception e, Exception e') => Eio e' a -> Eio e a
liftEio = withEio (review _S)

liftEither :: Exception e => Either e b -> Eio e b
liftEither = either (Eio . throwM) (Eio . pure)
{-# INLINE liftEither #-}

runEio :: Exception e => Eio e b -> IO (Either e b)
runEio (unEio -> io) = fmap Right io `catch` (pure . Left)
{-# INLINE runEio #-}

liftIOWith :: (Exception e, Exception e') => (e -> e') -> IO a -> Eio e' a
liftIOWith f io = Eio $ io `catch` (throwM . f)
{-# INLINE liftIOWith #-}

mapEio
  :: (Exception e, Exception e')
  => (Either e a -> Either e' b) -> Eio e a -> Eio e' b
mapEio f (unEio -> io) = Eio io' where
  io' = (io >>= either throwM pure . f . Right)
    `catch` (either throwM pure . f . Left)
{-# INLINE mapEio #-}

withEio :: (Exception e, Exception e') => (e -> e') -> Eio e a -> Eio e' a
withEio f (unEio -> io) = Eio io' where
  io' = io `catch` (throwM . f)
{-# INLINE withEio #-}

throwE :: Exception e => e -> Eio e a
throwE = Eio . throwM
{-# INLINE throwE #-}

catchE :: (Exception e, Exception e') => Eio e a -> (e -> Eio e' a) -> Eio e' a
catchE (Eio io) h = Eio $ io `catch` (unEio . h)
{-# INLINE catchE #-}

instance Exception e => MonadError e (Eio e) where
  throwError = throwE
  catchError = catchE

instance MonadFail (Eio e) where
  fail = Eio . fail

class (<:) a s where
  _S :: Prism' s a

instance {-# OVERLAPPING #-} (<:) a a where
  _S = iso id id
  {-# INLINE _S #-}

instance (Core.Context a s) => (<:) a s where
  _S eta = prism2prismvl Core.derived eta
  {-# INLINE _S #-}

type As e' e = (e' <: e, Exception e', Exception e)
