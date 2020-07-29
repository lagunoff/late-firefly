{-# LANGUAGE CPP #-}
module LateFirefly.Utils where

import Control.Error
import Control.Lens
import Control.Monad.Catch as C
import Control.Monad.Catch as Catch
import Control.Monad.Error
import Data.List as L
import Data.Proxy
import Data.String
import Data.Text as T
import Data.Typeable
import Data.UUID (UUID)
import Database.SQLite.Simple
import Database.SQLite3 as SQLite3
import Flat
import Data.Generics.Product
import GHC.Generics
import GHC.Int
import GHC.Stack
import GHC.Stack.Types
import GHC.TypeLits
import JavaScript.Web.XMLHttpRequest as XHR
import LateFirefly.Eio
import LateFirefly.Orphans ()
import Massaraksh (Html)
import Unsafe.Coerce
import qualified Control.Exception as Exception
import qualified Control.Exception as P

#ifndef __GHCJS__
import Data.Aeson as AE
import Data.HashMap.Strict as H
import Data.Aeson.Internal as AE
import Data.Aeson.Types as AE
#endif

newtype Id t = Id {unId :: Int64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Flat

newtype Tid t = Tid {unTid :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Flat, IsString)

newtype UUID5 t = UUID5 {unUUID5 :: UUID}
  deriving stock (Show, Read, Eq, Generic)
  deriving newtype Flat

data BackendError
  = SQLError SQLError
  | BEFlatError String
  deriving stock (Show, Eq, Generic)
  deriving Exception
#ifndef __GHCJS__
  deriving anyclass (FromJSON, ToJSON)
#endif

data PublicBackendError
  = ErrorCode (Id BackendError)
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

data FrontendError
  = XHRError XHR.XHRError
  | FlatError String
  | BadResponse Text
  | BackendError PublicBackendError
  deriving stock (Show, Eq, Generic)
  deriving Exception

type Backend a r = (?conn::Connection) => a -> Eio BackendError r

deriving stock instance Generic SQLError
deriving stock instance Generic SQLite3.Error
#ifndef __GHCJS__
deriving anyclass instance FromJSON SQLError
deriving anyclass instance ToJSON SQLError
deriving anyclass instance FromJSON SQLite3.Error
deriving anyclass instance ToJSON SQLite3.Error
#endif

instance MonadError FrontendError Html where
  throwError = Catch.throwM
  catchError = Catch.catch

catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync io f = io `Exception.catch` \e ->
  case Exception.fromException e of
    Just (Exception.SomeAsyncException _) -> Exception.throwIO e
    Nothing                               -> f e

errorTrace :: HasCallStack => a
errorTrace = withFrozenCallStack (error "errorTrace")

headTrace :: HasCallStack => [a] -> a
headTrace []    = withFrozenCallStack (error "headTrace")
headTrace (x:_) = x

headNote :: HasCallStack => String -> [a] -> a
headNote m []    = withFrozenCallStack (error m)
headNote _ (x:_) = x

maybeTrace :: HasCallStack => Maybe a -> a
maybeTrace Nothing  = withFrozenCallStack (error "maybeTrace")
maybeTrace (Just x) = x

maybeNote :: HasCallStack => String -> Maybe a -> a
maybeNote m Nothing  = withFrozenCallStack (error m)
maybeNote _ (Just x) = x

nemptyTrace :: (Eq a, Monoid a, HasCallStack) => a -> a
nemptyTrace x = if x == mempty then withFrozenCallStack (error "neTrace") else x

nemptyNote :: (Eq a, Monoid a, HasCallStack) => String -> a -> a
nemptyNote m x = if x == mempty then withFrozenCallStack (error m) else x

stripPrefix1 :: String -> String -> String
stripPrefix1 p x = fromMaybe x $ L.stripPrefix p x

replaces :: Eq a => [(a, a)] -> a -> a
replaces xs x = L.lookup x xs ?: x

#ifndef __GHCJS__
-- | Helper for convenient nested field parsing
(.::) :: FromJSON a => AE.Object -> [Text] -> AE.Parser a
(.::) = go where
  go _ []     = fail "empty field list"
  go o (k:ks) = case (H.lookup k o, ks) of
    (Nothing         , _)  -> fail $ "key " <> show k <> " not found"
    (Just v          , []) -> parseJSON v <?> AE.Key k
    (Just (Object o'), _)  -> go o' ks
    (Just _          , _)  -> fail "accessing property of non-object"
#endif

type family Demote' (kparam :: KProxy k) :: *
type Demote (a :: k) = Demote' ('KProxy :: KProxy k)

type instance Demote' ('KProxy :: KProxy Symbol) = String
type instance Demote' ('KProxy :: KProxy [a]) = [Demote' ('KProxy :: KProxy a)]

class Reflect (a :: k) where
  reflect :: Proxy (a :: k) -> Demote a

instance KnownSymbol s => Reflect (s :: Symbol) where
  reflect = symbolVal

instance Reflect ('[] :: [k]) where
  reflect _ = []

instance (Reflect x, Reflect xs) => Reflect (x ': xs) where
  reflect _ = reflect (Proxy :: Proxy x) : reflect (Proxy :: Proxy xs)

newtype Pick a (k::[Symbol]) = Pick
  { unPick :: a }

pickCons :: forall x s a xs. HasField' x s a => a -> Pick s xs -> Pick s (x ': xs)
pickCons x (Pick s) = Pick (setField @x x s)

pickNil :: (GNilRec (Rep s), Generic s) => Pick s '[]
pickNil = Pick nilRec

instance {-# OVERLAPPING #-}
  (KnownSymbol x, HasField' x s a, Show a, Show (Pick s xs)) => Show (Pick s (x ': xs)) where
  show (Pick s) = symbolVal (Proxy @x) <> " = " <> show (getField @x s) <> ", " <> show (Pick s::Pick s xs)

instance Show (Pick s '[]) where
  show _ = ""

type family Find (x::k) (xs::[k]) where
  Find x '[] = False
  Find x (x:xs) = True
  Find x (y:xs) = Find x xs

instance {-# OVERLAPPING #-}
  (Find x fs ~ True, HasField' x s a) => HasField' x (Pick s fs) a where
  field' = l . field' @x @s where
    l = lens (\(Pick x) -> x) (\(Pick x1) x2 -> Pick x2)

instance {-# OVERLAPPING #-}
  (KnownSymbol x, HasField' x s a, FromJSON a, FromJSON (Pick s xs)) => FromJSON (Pick s (x ': xs)) where
  parseJSON v = pickCons <$> (withObject "Pick" (\o -> o .: (T.pack $ symbolVal (Proxy @x))) v) <*> parseJSON @(Pick s xs) v

instance (GNilRec (Rep s), Generic s) => FromJSON (Pick s '[]) where
  parseJSON _ = pure (Pick nilRec)

-- | Construct a record with all fields assigned to 'unsafeCoerce (0::Int)'
class GNilRec f where
  gNilRec :: Proxy (f p) -> f p

instance GNilRec f => GNilRec (D1 c f) where
  gNilRec _ = M1 $ gNilRec (Proxy::Proxy(f x))

instance Constructor c => GNilRec (C1 c U1) where
  gNilRec _ = M1 U1

instance (GNilRec f, Constructor c) => GNilRec (C1 c f) where
  gNilRec _ = M1 $ gNilRec (Proxy::Proxy(f x))

instance Selector s => GNilRec (S1 s (K1 c f)) where
  gNilRec _ = M1 (K1 (unsafeCoerce (0::Int)))

instance (GNilRec f, GNilRec g) => GNilRec ((:*:) f g) where
  gNilRec _ = gNilRec (Proxy::Proxy(f x)) :*: gNilRec (Proxy::Proxy(g x))

nilRec :: forall a. (GNilRec (Rep a), Generic a) => a
nilRec = GHC.Generics.to $ gNilRec (Proxy @(Rep a _))
