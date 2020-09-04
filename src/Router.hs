{-# OPTIONS_GHC -fno-warn-orphans #-}
module Router where

import Control.Lens (dimap)
import Data.Constraint
import Data.String as S
import Database.SQLite.Simple
import Flat
import GHC.Fingerprint
import qualified Data.Dynamic as D
import qualified Data.Map as M

import "this" Parser as X
import "this" Intro

class (HasUrl r, Typeable r, Typeable (PageData r)) => IsPage r where
  type PageData r
  pageInit :: (?conn::Connection) => r -> ServerIO (PageData r)
  pageWidget :: PageData r -> Html ()

data PageDict = forall a. PageDict (Dict (IsPage a))

data Page404Data = Page404Data
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

instance IsPage () where
  type PageData () = Page404Data
  pageInit _ = pure Page404Data
  pageWidget = undefined

instance HasParser Url () where
  parser = pUnit

pageParser :: [PageDict] -> Parser UrlChunks D.Dynamic (D.Dynamic, PageDict)
pageParser p = Parser (par p) pri where
  mm = mfng p
  par :: [PageDict] -> UrlChunks -> [((D.Dynamic, PageDict), UrlChunks)]
  par [] s = []
  par (x:xs) s = (fmap (\(a, b) -> ((a, x), b)) (parse (pd x) s)) <> par xs s
  pri :: D.Dynamic -> _ -> _
  pri d x = let z = D.dynTypeRep d in
    maybe x (\y -> X.print (pd y) d x) $ M.lookup (typeRepFingerprint z) mm
  pd :: PageDict -> UrlParser D.Dynamic
  pd (PageDict d@Dict) = dPar (pp d) where
    pp :: forall a. Dict (IsPage a) -> UrlParser a
    pp Dict = parser @_ @a
  mfng :: [PageDict] -> M.Map Fingerprint PageDict
  mfng = M.fromList . fmap \x@(PageDict d) -> (pp d, x) where
    pp :: forall a. Dict (IsPage a) -> Fingerprint
    pp Dict = typeRepFingerprint (typeRep (Proxy::Proxy a))
  dPar :: Typeable a => Parser' s a -> Parser' s D.Dynamic
  dPar = dimap g f where
    f = D.toDyn
    g = fromMaybe errorTrace . D.fromDynamic

printRoute :: HasUrl r => r -> Text
printRoute = ("/" <>) . printUrl

-- linkTo :: HasUrl r => r -> Html x -> Html x
-- linkTo = mayLinkTo . Just

instance S.IsString s => S.IsString (Seg s) where
  fromString = Seg . S.fromString
