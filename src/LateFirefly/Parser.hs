{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module LateFirefly.Parser where

import Control.Lens hiding (from, to)
import Control.Monad
import Data.Bool

import Data.Either
import Data.List as L
import Data.Map.Ordered as M
import Data.Maybe
import Data.Text as T
import GHC.Generics
import Network.URI
import Prelude hiding (print)
import Text.Inflections
import Text.Read


-- Local shortcuts for convenience
type U = UrlChunks
type Url = UrlChunks
type P = ParamState
type UrlParser a = Parser UrlChunks a a
type ParamParser a = Parser ParamState a a
type ParamState = Maybe Text
type Parser' s a = Parser s a a

data Parser s i o = Parser
  { parse :: s -> [(o, s)]
  , print :: i -> s -> s }

data UrlChunks = UrlChunks [Text] (OMap Text Text)
  deriving (Eq, Show)

segments :: [Text] -> Parser' U ()
segments xs = Parser parse print where
  parse (UrlChunks ss ps) = go ss xs where
    go ss []         = [((), UrlChunks ss ps)]
    go [] xs         = []
    go (s:ss) (x:xs) = if x == s then go ss xs else []
  print _ (UrlChunks ss ps) = UrlChunks (xs <> ss) ps

segment :: Text -> Parser' U ()
segment s = segments [s]
{-# INLINE segment #-}

gsegment :: Text -> Parser' U (U1 p)
gsegment s = U1 <@ segments [s]
{-# INLINE gsegment #-}

pSuccess :: a -> Parser' s a
pSuccess a = Parser ((:[]) . (a,)) (const id)

pUnit :: Parser' s ()
pUnit = pSuccess ()

(</>) :: Parser' U a -> Parser' U b -> Parser' U (a, b)
(</>) pa pb = Parser par pri where
  par s = do (a, s') <- parse pa s; parse pb s' <&> _1 %~ (a,)
  pri (a, b) = print pa a . print pb b

(/>) :: Parser' U () -> Parser' U b -> Parser' U b
(/>) pa pb = Parser par pri where
  par s = join (parse pa s <&> parse pb . snd)
  pri b = print pa () . print pb b

(</) :: Parser' U a -> Parser' U () -> Parser' U a
(</) = flip (/>)

(<||>) :: Parser' U a -> Parser' U b -> Parser' U (Either a b)
(<||>) pa pb = Parser par pri where
  par s = let r = parse pa s in if L.null r
    then parse pb s <&> (_1 %~ Right) else fmap (_1 %~ Left) r
  pri e s = either (flip (print pa) s) (flip (print pb) s) e

(|*|) :: Parser' U (f p) -> Parser' U (g p) -> Parser' U ((:*:) f g p)
(|*|) pf pg = dimap g1 f1 (pf </> pg) where
  g1 (a :*: b) = (a, b)
  f1 (a, b) = (a :*: b)

(|+|) :: Parser' U (f x) -> Parser' U (g x) -> Parser' U ((:+:) f g x)
(|+|) pf pg = dimap g1 f1 (pf <||> pg) where
  g1 (L1 x) = Left x
  g1 (R1 x) = Right x
  f1 (Left x) = L1 x
  f1 (Right x) = R1 x

(@>) :: Parser' U () -> b -> Parser' U b
(@>) p b = Parser (fmap (_1 .~ b) . parse p) (\_ -> print p ())

(<@) :: b -> Parser' U () -> Parser' U b
(<@) = flip (@>)

infixr 5 </>, />, </
infixr 4 @>, <@
infixr 4 |+|
infixr 5 |*|

param :: HasParser ParamState a => Text -> Parser' U a
param n = Parser par pri where
  par u@(UrlChunks _ ps) = parse parser (M.lookup n ps) <&> _2 .~ u
  pri a (UrlChunks ss ps) = UrlChunks ss (alterOMap (print parser a) n ps)

pSegment :: HasParser Text a => Text -> Parser' U a
pSegment n = Parser par pri where
  par u@(UrlChunks [] _)      = []
  par u@(UrlChunks (s:ss) ps) = parse parser s <&> _2 .~ UrlChunks ss ps
  pri a (UrlChunks ss ps) = UrlChunks (print parser a "":ss) ps

paramOr
  :: (Eq a, HasParser ParamState a) => Text -> a -> Parser' U a
paramOr n d = Parser par pri where
  par u@(UrlChunks _ ps) = let r = parse parser (M.lookup n ps) <&> _2 .~ u in
    if L.null r then [(d, u)] else r
  pri a (UrlChunks ss ps) = UrlChunks ss
    (alterOMap (bool (print parser a) (const Nothing) (a == d)) n ps)

gparam
  :: forall a c2 i2 p. HasParser ParamState a
  => Text -> Parser' U (S1 c2 (K1 i2 a) p)
gparam n = dimap (unK1 . unM1) (M1 . K1) $ param @a n
{-# INLINE gparam #-}

gparamOr
  :: forall a c i x. (Eq a, HasParser ParamState a)
  => Text -> a -> Parser' U (S1 c (K1 i a) x)
gparamOr n d = dimap (unK1 . unM1) (M1 . K1) $ paramOr @a n d
{-# INLINE gparamOr #-}

prepareUrl :: Text -> UrlChunks
prepareUrl url = UrlChunks segments params where
  breakOn1 s t = T.breakOn s t & _2 %~ T.drop 1
  (segText, parText) = bimap deURI deURI (breakOn1 "?" url)
  segments = T.splitOn "/" segText & L.filter (/= "") <&> deURI
  params = M.fromList $ T.splitOn "&" parText & L.filter (/= "") <&> breakOn1 "="

printChunks :: UrlChunks -> Text
printChunks (UrlChunks ss ps) =
  T.intercalate "?" ([segments] <> bool mempty [params] (params /= "")) where
    segments = T.intercalate "/" $ fmap enURI ss
    params = T.intercalate "&" $ L.filter (/= "") $ M.assocs ps
      <&> bimap enURI enURI <&> \(k, v) -> k <> "=" <> v

parseUrl :: HasParser U a => Text -> [a]
parseUrl = fmap fst . L.filter pp . parse parser . prepareUrl
  where
    pp (_, UrlChunks [] _) = True
    pp (_, UrlChunks _ _)  = False
{-# INLINE parseUrl #-}

printUrl :: HasParser U a =>  a -> Text
printUrl a = printChunks (print parser a emptyUrl)
{-# INLINE printUrl #-}

grec1
  :: forall a s f c1 c2 x. (Generic a, (D1 c1 (C1 c2 f) x) ~ Rep a x)
  => Parser' s (f x) -> Parser' s a
grec1 = dimap (unM1 . unM1 . from) (to . M1 . M1)
{-# INLINE grec1 #-}

grec
  :: forall a s c f x. (Generic a, (D1 c f x) ~ Rep a x)
  => Parser' s (f x) -> Parser' s a
grec = dimap (unM1 . from) (to . M1)
{-# INLINE grec #-}

gcon :: forall s c f x. Parser' s (f x) -> Parser' s (C1 c f x)
gcon = dimap unM1 M1
{-# INLINE gcon #-}

gcon1 :: forall s c p. Parser' s () -> Parser' s (C1 c U1 p)
gcon1 = dimap (const ()) (const (M1 U1))
{-# INLINE gcon1 #-}

instance Profunctor (Parser s) where
  dimap g f p = Parser (fmap (_1 %~ f) . parse p) (print p . g)
  {-# INLINE dimap #-}

class HasParser s a where
  parser :: Parser s a a
  default parser :: (Generic a, GParser (Rep a), s ~ U) => Parser s a a
  parser = dimap from to (gParser @(Rep a))

newtype ReadShowParam a = ReadShowParam {unReadShowParam :: a}
newtype ReadShowSegment a = ReadShowSegment {unReadShowSegment :: a}
newtype Seg a = Seg {unSeg :: a}
  deriving (Show, Eq, Ord)

deriving newtype instance HasParser s a => HasParser s (Seg a)

instance (Read a, Show a) => HasParser P (ReadShowParam a) where
  parser = Parser par pri where
    par st = fromMaybe [] do
      s <- fmap T.unpack st
      i <- readMaybe @a s
      pure [(ReadShowParam i, st)]
    pri (ReadShowParam i) _ =
      Just $ T.pack (show i)

instance (Read a, Show a) => HasParser Text (ReadShowSegment a) where
  parser = Parser par pri where
    par st = fromMaybe [] do
      i <- readMaybe @a (T.unpack st)
      pure [(ReadShowSegment i, st)]
    pri (ReadShowSegment i) _ =
      T.pack (show i)

deriving via ReadShowParam Int instance HasParser P Int
deriving via ReadShowParam Integer instance HasParser P Integer
deriving via ReadShowParam Double instance HasParser P Double
deriving via ReadShowParam Float instance HasParser P Float

deriving via ReadShowSegment Int instance HasParser Text Int
deriving via ReadShowSegment Integer instance HasParser Text Integer
deriving via ReadShowSegment Double instance HasParser Text Double
deriving via ReadShowSegment Float instance HasParser Text Float

instance HasParser P a => HasParser P (Maybe a) where
  parser = Parser par pri where
    par = maybe [(Nothing, Nothing)] (fmap (_1 %~ Just) . parse parser . Just)
    pri = maybe (const Nothing) (print parser)

instance HasParser P Text where
  parser = Parser par pri where
    par = maybe [] \s -> [(s, Just s)]
    pri i _ = Just i

instance HasParser Text Text where
  parser = Parser par pri where
    par = \s -> [(s, s)]
    pri i _ = i

emptyUrl :: UrlChunks
emptyUrl = UrlChunks mempty M.empty

constructorToSegment :: String -> Maybe Text
constructorToSegment (T.pack -> n)
  | '_' <- T.last n = Nothing
  | otherwise       =
    let n' = T.dropWhileEnd shouldDrop n
    in Just $ fromRight n' $
    (parseCamelCase [] n' <&> dasherize)
    where shouldDrop c = c == 'R' || c == '_'

alterOMap :: Ord k => (Maybe a -> Maybe a) -> k -> OMap k a -> OMap k a
alterOMap f k old =
  maybe (M.delete k old) ((|< old) . (k,)) $ f (M.lookup k old)

class GParser f where
  gParser :: Parser' U (f p)

instance GParser f => GParser (D1 c f) where
  gParser = dimap unM1 M1 (gParser @f)

instance {-# OVERLAPPING #-} (GParser f, Constructor c) => GParser (C1 c f) where
  gParser = dimap unM1 M1 $ pSegment /> gParser @f
    where
      pSegment = maybe pUnit segment ctr
      ctr = constructorToSegment $ conName (undefined :: t c f a)

instance {-# OVERLAPS #-} Constructor c => GParser (C1 c U1) where
  gParser = dimap unM1 M1 pSegment --  if ctr == "index" then pSuccess U1 else gsegment ctr
    where
      pSegment = maybe (pSuccess U1) gsegment ctr
      ctr = constructorToSegment $ conName (undefined :: t c U1 a)

instance {-# OVERLAPS #-} (HasParser U a, Constructor c) =>
  GParser (C1 c (S1 (d 'Nothing f g h) (Rec0 a))) where
  gParser = dimap (unK1 . unM1 . unM1) (M1 . M1 . K1) (pSegment /> parser @U @a)
    where
      pSegment = maybe pUnit segment ctr
      ctr = constructorToSegment
        $ conName (undefined :: t c (S1 (d 'Nothing f g h) (Rec0 a)) x)

-- If datatype has single constructor, ignore the name
instance {-# OVERLAPS #-} GParser f => GParser (D1 d (C1 c f)) where
  gParser = dimap (unM1 . unM1) (M1 . M1) gParser

instance {-# OVERLAPS #-} (HasParser Text a, Selector s) => GParser (S1 s (Rec0 (Seg a))) where
  gParser = dimap (unSeg . unK1 . unM1) (M1 . K1 . Seg)
    $ pSegment @a $ T.pack $ selName (undefined :: t s (Rec0 (Seg a)) a)

instance (HasParser P a, Selector s) => GParser (S1 s (Rec0 a)) where
  gParser = dimap (unK1 . unM1) (M1 . K1)
    $ param @a $ T.pack $ selName (undefined :: t s (Rec0 a) a)

instance (GParser f, GParser g) => GParser ((:*:) f g) where
  gParser = (|*|) gParser gParser

instance (GParser f, GParser g) => GParser ((:+:) f g) where
  gParser = (|+|) gParser gParser

enURI :: Text -> Text
enURI = T.pack . escapeURIString isAllowed . T.unpack where
  isAllowed c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_.~")

deURI :: Text -> Text
deURI = T.pack . unEscapeString . T.unpack
