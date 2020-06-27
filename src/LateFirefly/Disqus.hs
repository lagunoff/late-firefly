module LateFirefly.Disqus where

import Language.Javascript.JSaddle
import Massaraksh.Text
import LateFirefly.Widget.Prelude

setupDisqus :: Html
setupDisqus = if True then blank else liftJSM do
  scriptEl <- createElement "script"
  scriptEl2 <- createElement "script"
  scriptEl2 <# ("innerHTML" :: Text) $ [st|
    window.disqus_config = function() {
      this.page.url = window.location.href;
      this.page.identifier = window.location.href;
    };
  |]
  date <- (new (jsg ("Date" :: Text))) $ ()
  dateVal <- date # ("valueOf" :: Text) $ ()
  scriptEl # ("setAttribute" :: Text) $ ("src" :: Text, "https://telikov-net.disqus.com/embed.js" :: Text)
  scriptEl # ("setAttribute" :: Text) $ ("data-timestamp" :: Text, dateVal)
--  jsg ("document" :: Text) ! ("body" :: Text) # ("appendChild" :: Text) $ scriptEl2
  jsg ("document" :: Text) ! ("body" :: Text) # ("appendChild" :: Text) $ scriptEl
  pure ()

embedDisqus :: Text -> Text -> Html
embedDisqus pageId pageTitle = if True then blank else do
  div_ ("id" =: "disqus_thread")
  void $ liftJSM $ eval [st|
    setTimeout(function() {
      DISQUS.reset({
        reload: true,
        config: function() {
          this.page.url = window.location.href;
          this.page.identifier = window.location.pathname;
          this.page.title = "#{pageTitle}";
        }
      });
    }, 100);
  |]
