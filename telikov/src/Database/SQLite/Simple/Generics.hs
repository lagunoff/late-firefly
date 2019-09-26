{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints #-}
module Database.SQLite.Simple.Generics where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import GHC.Generics
import Control.Applicative

-- Type class for default implementation of FromRow using generics
class GFromRow f where
    gFromRow :: RowParser (f p)

instance GFromRow f => GFromRow (M1 c i f) where
    gFromRow = M1 <$> gFromRow

instance (GFromRow f, GFromRow g) => GFromRow (f :*: g) where
    gFromRow = liftA2 (:*:) gFromRow gFromRow

instance FromField a => GFromRow (K1 R a) where
    gFromRow = K1 <$> field

instance GFromRow U1 where
    gFromRow = pure U1
      
-- Type class for default implementaGToRowon of ToRowgenerics
class GToRow f where
    gToRow :: f p -> [SQLData]

instance GToRow f => GToRow (M1 c i f) where
    gToRow (M1 x) = gToRow x

instance (GToRow f, GToRow g) => GToRow (f :*: g) where
    gToRow (f :*: g) = gToRow f ++ gToRow g

instance (ToField a) => GToRow (K1 R a) where
    gToRow (K1 a) = [toField a]

instance GToRow U1 where
    gToRow _ = []
  
