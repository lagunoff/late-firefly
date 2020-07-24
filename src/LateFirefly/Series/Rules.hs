{-# LANGUAGE CPP #-}
module LateFirefly.Series.Rules where

import LateFirefly.Prelude
import LateFirefly.DB
import Data.Text as T
import Network.URI
import Data.List as L
#ifndef __GHCJS__
import Text.Regex.TDFA
#endif

data LinkRule = LinkRule
  { rowid          :: Id LinkRule
  , uriAuthorityRe :: Maybe Text
  , siteRe         :: Maybe Text }
  deriving stock (Eq, Show, Generic)

deriveDb ''LinkRule

applyLinkRule :: LinkRule -> Text -> [Text] -> [Text]
#ifndef __GHCJS__
applyLinkRule rl site = Prelude.filter predicate where
  predicate :: Text -> Bool
  predicate t = isJust do
    uri <- parseURI (T.unpack t)
    flipMaybe do
      re <- siteRe rl
      flipMaybe $ T.unpack site =~~ T.unpack re
    flipMaybe do
      re <- uriAuthorityRe rl
      URIAuth{..} <- uriAuthority uri
      let str = uriUserInfo <> uriRegName <> uriPort
      str =~~ T.unpack re

  flipMaybe :: Maybe () -> Maybe ()
  flipMaybe = maybe blank (const Nothing)
#else
applyLinkRule _ _ = error "applyLinkRule: Unimplemented"
#endif

applyLinkRule' :: (?conn :: Connection) => Text -> [Text] -> IO [Text]
applyLinkRule' site xs = do
  rules <- selectFrom @LinkRule [sql|where 1|]
  pure $ L.foldl' (\xs rl -> applyLinkRule rl site xs) xs rules
