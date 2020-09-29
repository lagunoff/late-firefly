module Site.Template where

import Data.Text.Lazy as TL
import Language.Javascript.JMacro
import Lucid

htmlTemplate :: TL.Text -> JStat -> Html () -> Html ()

headerWidget :: TL.Text -> Html ()

page404 :: Html ()
