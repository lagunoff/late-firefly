module Main where

import Prelude

import Affjax (URL, get)
import Affjax.ResponseFormat as Format
import Cheerio (Cheerio, attr, find, text, toArray)
import Cheerio.Static (loadRoot)
import Control.Monad.Error.Class (class MonadThrow, withResource)
import Control.Monad.Except (runExceptT, throwError)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson, genericDecodeJsonWith)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Class (gEncodeJson)
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep, encodeRep, genericEncodeJson, genericEncodeJsonWith)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Types.Generic.Rep (defaultEncoding)
import Data.Array (foldM, length, (!!))
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic, from, to)
import Data.Maybe (maybe)
import Data.Traversable (traverse, traverse_)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Err (Err(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Prim.Boolean (False, True)
import Unsafe.Coerce (unsafeCoerce)

newtype Series = Series
  { code :: String
  , name :: String
  , href :: String
  , short_description :: String
  , thumbnail :: String
  , description :: String
  , links :: Array String
  }
    
newtype Season = Season
  { code :: String
  , thumbnail :: String
  , href ∷ String
  , series :: Array Series
  }
    
newtype Db = Db
  { seasons ∷ Array Season
  }


process ∷ Aff (Either Err Unit)
process = runExceptT $ do
  let listUrl = "https://iwatchtheoffice.com/season-list/"
  body ← getString listUrl
  let root = loadRoot body
  entries ← findNonEmpty listUrl root ".entry-content > div"
  traceM $ entries # map text
  seasons ← traverse (parseSeason listUrl) entries
--  traverse_ (liftAff <<< insertSeason db) seasons
  let content = stringify $ encodeJson $ Db { seasons }
  liftEffect $ writeTextFile UTF8 "./db.json" content 
  pure unit

main01 :: Effect Unit
main01 = process # runAff_ case _ of
  Left err → log "Async Error occured" *> traceM err
  Right (Left err) → log "Error occured" *> traceM err
  Right (Right _) → log "All ended up fine"

main = main01


parseSeason ∷ ∀ m. MonadThrow Err m ⇒ MonadAff m ⇒ String → Cheerio → m Season
parseSeason url root = do
  a ← find1 url root "a"
  href ← attr0 url a "href"
  img ← find1 url root "img"
  thumbnail ← attr0 url img "src"
  inner_remaining_l ← find1 url root "#inner_remaining_l"
  let code = text inner_remaining_l
  series ← parseSeries href      
  pure $ Season { code, thumbnail, href, series }
  
parseSeriesEntry ∷ ∀ m. MonadThrow Err m ⇒ MonadAff m ⇒ String → Cheerio → m Series
parseSeriesEntry url root = do
  a ← find1 url root "a"
  href ← attr0 url a "href"
  img ← find1 url root "img"
  thumbnail ← attr0 url img "src"
  inner_remaining_l ← find1 url root "#inner_remaining_l"
  let code = text inner_remaining_l
  inner_remaining_r ← find1 url root "#inner_remaining_r"
  let name = text inner_remaining_r
  inner_remaining_r2 ← find1 url root "#inner_remaining_r2"
  let short_description = text inner_remaining_r2
  let seriesUrl = "https://iwatchtheoffice.com" <> href
  body ← getString seriesUrl
  let seriesRoot = loadRoot body
  linkz ← findNonEmpty seriesUrl seriesRoot ".linkz_box > a"
  links ← flip traverse linkz $ \link → do
    attr0 seriesUrl link "href"
  description_box ← find1 seriesUrl seriesRoot ".description_box"
  let description = text description_box
  pure $ Series { code, thumbnail, href, short_description, name, description, links }

parseSeries
  ∷ ∀ m
  . MonadThrow Err m
  ⇒ MonadAff m
  ⇒ String
  → m (Array Series)
parseSeries href = do
  let url = "https://iwatchtheoffice.com" <> href
  body ← getString url
  let root = loadRoot body
  entries ← findNonEmpty url root ".entry-content > div#outer"
  traverse (parseSeriesEntry url) entries
  

findNonEmpty ∷ ∀ m. MonadThrow Err m ⇒ String → Cheerio → String → m (Array Cheerio)
findNonEmpty url root selector = do
  let entries = find selector root # toArray
  if length entries == 0 then throwError (EmptySelectorResults { url, selector }) else pure unit
  pure entries

find1 ∷ ∀ m. MonadThrow Err m ⇒ String → Cheerio → String → m Cheerio
find1 url root selector = do
  let entries = find selector root # toArray
      first = entries !! 0
  maybe (throwError (EmptySelectorResults { url, selector })) pure first
  
attr0 ∷ ∀ m. MonadThrow Err m ⇒ String → Cheerio → String → m String
attr0 url root attribute = do
  maybe (throwError $ EmptyAttribute { url, attribute }) pure (attr attribute root)
  

getString ∷ ∀ m. MonadThrow Err m ⇒ MonadAff m ⇒ URL → m String
getString url = do
  liftEffect $ log $ "GET " <> url <> "…"
  resp ← liftAff $ get Format.string url
  either (throwError <<< ResponseFormatError) pure resp.body
  

main02 :: Effect Unit
main02 = runAff_ handleResult $ runExceptT $ do
  content ← liftEffect $ readTextFile UTF8 "./db.json"
  json ← either (throwError <<< JSONParseError) pure (jsonParser content)
  let db = decodeJson json :: Either _ Db
  traceM db
  pure unit
  where
    handleResult = case _ of
      Left err → log "Async Error occured" *> traceM err
      Right _ → log "Aff completed without errors"
  
  
derive instance genericSeries ∷ Generic Series _

instance encodeJsonSeries ∷ EncodeJson Series where
  encodeJson = genericEncodeJsonWith $ defaultEncoding { unwrapSingleArguments = true }
  
instance decodeJsonSeries ∷ DecodeJson Series where
  decodeJson = genericDecodeJsonWith $ defaultEncoding { unwrapSingleArguments = true }

derive instance genericSeason ∷ Generic Season _

instance encodeJsonSeason ∷ EncodeJson Season where
 encodeJson = genericEncodeJsonWith $ defaultEncoding { unwrapSingleArguments = true }
 
instance decodeJsonSeason ∷ DecodeJson Season where
  decodeJson = genericDecodeJsonWith $ defaultEncoding { unwrapSingleArguments = true }
                                                    
derive instance genericDb ∷ Generic Db _

instance encodeJsonDb ∷ EncodeJson Db where
  encodeJson = genericEncodeJsonWith $ defaultEncoding { unwrapSingleArguments = true }
  
instance decodeJsonDb ∷ DecodeJson Db where
  decodeJson = genericDecodeJsonWith $ defaultEncoding { unwrapSingleArguments = true }
