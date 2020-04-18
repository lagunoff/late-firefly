{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}
module LF.Index (indexWidget, Model(..)) where

import Control.Monad.Trans
import Control.Lens
import Data.Text as T
import Data.Maybe
import LF.Router
import LF.Prelude
import LF.Backend
import Massaraksh as H
import GHC.Records
import Language.Javascript.JSaddle
import LF.DB
import Flat.Rpc hiding (remote, to)
import LF.TheOffice.Schema

data Route
  = AboutR
  | IndexR
  deriving (Show, Eq, Generic, HasParser U)

data Model m = Model
  { route :: Route
  } deriving (Generic)

indexWidget :: (HtmlBase m, MonadClient m) => HtmlT m ()
indexWidget = mdo
  mdl <- liftIO $ newDynRef (Model route)
  route <- liftJSM $ hashRouter IndexR \x -> do
    modifyDynRef mdl \m -> m {route=x}
  seasons <- lift $ send "sdfsdf" $ static (remote getSeasons)
  div_ do
    "className" =: "root"
  ul_ do
    let priUrl = ("#" <>) . printUrl
    li_ do a_ do "Home"; "href" =: priUrl IndexR
    li_ do a_ do "About"; "href" =: priUrl AboutR
  div_ do
    routeDyn <- liftIO $ flip holdUniqDynBy (getDyn mdl)
      \Model{route=a} Model{route=b} -> a == b
    dynHtml $ routeDyn <&> getField @"route" <&> \case
      IndexR -> indexPage seasons
      AboutR -> aboutPage

getSeasons :: Given Connection => Text -> IO [Season]
getSeasons _ = do
  selectFrom_ @Season [sql|where 1|]

aboutPage :: HtmlBase m => HtmlT m ()
aboutPage = do
  div_ do
    h1_ do
      "About Page Works!!!"

indexPage :: HtmlBase m => [Season] -> HtmlT m ()
indexPage ss = do
  div_ do
    h1_ do
      "Index Page Works!!!"
    for_ ss (div_ . text . T.pack . show)

hashRouter :: forall a. HasParser U a => a -> (a -> IO ()) -> JSM a
hashRouter def hashChange = do
  win <- jsg ("window" :: Text)
  let
    parseRoute = do
      hash <- fromJSValUnchecked =<< jsg ("location" :: Text) ! ("hash" :: Text)
      pure $ fromMaybe def $ listToMaybe $ parseUrl @a (T.drop 1 hash)
  win <# ("onpopstate" :: Text) $ fun \_ _ _ -> do
    liftIO . hashChange =<< parseRoute
  parseRoute
