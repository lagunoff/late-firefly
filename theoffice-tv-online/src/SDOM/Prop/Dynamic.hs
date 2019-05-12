{-# LANGUAGE OverloadedStrings #-}
module SDOM.Prop.Dynamic where

import Haste.Prim
import Haste.Prim.Foreign
import Haste.DOM.JSString (Elem)
import SDOM
import Haste.JSString (intercalate)


-- | Set field to `Bool` value
boolProp :: JSString -> (i -> Bool) -> SDOMAttr i o
boolProp = prop
-- | Set field to `String` value
stringProp :: JSString -> (i -> JSString) -> SDOMAttr i o
stringProp = prop
-- | Set field to `Text` value
jsStrProp :: JSString -> (i -> JSString) -> SDOMAttr i o
jsStrProp = prop
-- | Set field to `Int` value
intProp :: JSString -> (i -> Int) -> SDOMAttr i o
intProp = prop
-- | Set field to `Double` value
doubleProp :: JSString -> (i -> Double) -> SDOMAttr i o
doubleProp = prop
-- | Define multiple classes conditionally
prop :: ToAny a => JSString -> (i -> a) -> SDOMAttr i o
prop name val = SDOMAttrDyn $ \_ new el -> unsafeSetProp el name $ toAny $ val new
  where
    unsafeSetProp :: Elem -> JSString -> JSAny -> IO ()
    unsafeSetProp = ffi "(function(el, name, val) { el[name] = val; })"

--
-- > div_ [ classList_ [ ("empty", null items) ] [ ]
--
classList_ :: (i -> [(JSString, Bool)]) -> SDOMAttr i o
classList_ xs =
  jsStrProp "class" $ (\i -> intercalate (" " :: JSString) [ t | (t, True) <- xs i ])
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/title>
title_ :: (i -> JSString) -> SDOMAttr i o
title_ = jsStrProp "title"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/selected>
selected_ :: (i -> Bool) -> SDOMAttr i o
selected_ = boolProp "selected"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/hidden>
hidden_ :: (i -> Bool) -> SDOMAttr i o
hidden_             = boolProp "hidden"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/value>
value_ :: (i -> JSString) -> SDOMAttr i o
value_             = jsStrProp "value"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/defaultValue>
defaultValue_ :: (i -> JSString) -> SDOMAttr i o
defaultValue_      = jsStrProp "defaultValue"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/accept>
accept_ :: (i -> JSString) -> SDOMAttr i o
accept_            = jsStrProp "accept"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/acceptCharset>
acceptCharset_ :: (i -> JSString) -> SDOMAttr i o
acceptCharset_     = jsStrProp "acceptCharset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/action>
action_ :: (i -> JSString) -> SDOMAttr i o
action_            = jsStrProp "action"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/autocomplete>
autocomplete_ :: (i -> Bool) -> SDOMAttr i o
autocomplete_ b = jsStrProp "autocomplete" (\i -> if b i then "on" else "off")
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/autosave>
autosave_ :: (i -> JSString) -> SDOMAttr i o
autosave_          = jsStrProp "autosave"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/disabled>
disabled_ :: (i -> Bool) -> SDOMAttr i o
disabled_          = boolProp "disabled"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/enctype>
enctype_ :: (i -> JSString) -> SDOMAttr i o
enctype_           = jsStrProp "enctype"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/formation>
formation_ :: (i -> JSString) -> SDOMAttr i o
formation_         = jsStrProp "formation"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/list>
list_ :: (i -> JSString) -> SDOMAttr i o
list_              = jsStrProp "list"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/maxlength>
maxlength_ :: (i -> JSString) -> SDOMAttr i o
maxlength_         = jsStrProp "maxlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/minlength>
minlength_ :: (i -> JSString) -> SDOMAttr i o
minlength_         = jsStrProp "minlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/method>
method_ :: (i -> JSString) -> SDOMAttr i o
method_            = jsStrProp "method"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/multiple>
multiple_ :: (i -> Bool) -> SDOMAttr i o
multiple_          = boolProp "multiple"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/novalidate>
novalidate_ :: (i -> Bool) -> SDOMAttr i o
novalidate_        = boolProp "noValidate"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/pattern>
pattern_ :: (i -> JSString) -> SDOMAttr i o
pattern_           = jsStrProp "pattern"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/readonly>
readonly_ :: (i -> Bool) -> SDOMAttr i o
readonly_          = boolProp "readOnly"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/required>
required_ :: (i -> Bool) -> SDOMAttr i o
required_          = boolProp "required"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/size>
size_ :: (i -> JSString) -> SDOMAttr i o
size_              = jsStrProp "size"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/for>
for_ :: (i -> JSString) -> SDOMAttr i o
for_               = jsStrProp "for"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/form>
form_ :: (i -> JSString) -> SDOMAttr i o
form_               = jsStrProp "form"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/max>
max_ :: (i -> JSString) -> SDOMAttr i o
max_               = jsStrProp "max"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/min>
min_ :: (i -> JSString) -> SDOMAttr i o
min_               = jsStrProp "min"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/step>
step_ :: (i -> JSString) -> SDOMAttr i o
step_              = jsStrProp "step"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/cols>
cols_ :: (i -> JSString) -> SDOMAttr i o
cols_              = jsStrProp "cols"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/rows>
rows_ :: (i -> JSString) -> SDOMAttr i o
rows_              = jsStrProp "rows"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/wrap>
wrap_ :: (i -> JSString) -> SDOMAttr i o
wrap_              = jsStrProp "wrap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/target>
target_ :: (i -> JSString) -> SDOMAttr i o
target_            = jsStrProp "target"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/download>
download_ :: (i -> JSString) -> SDOMAttr i o
download_          = jsStrProp "download"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/downloadAs>
downloadAs_ :: (i -> JSString) -> SDOMAttr i o
downloadAs_        = jsStrProp "downloadAs"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/hreflang>
hreflang_ :: (i -> JSString) -> SDOMAttr i o
hreflang_          = jsStrProp "hreflang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/media>
media_ :: (i -> JSString) -> SDOMAttr i o
media_             = jsStrProp "media"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/ping>
ping_ :: (i -> JSString) -> SDOMAttr i o
ping_              = jsStrProp "ping"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/rel>
rel_ :: (i -> JSString) -> SDOMAttr i o
rel_               = jsStrProp "rel"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/ismap>
ismap_ :: (i -> JSString) -> SDOMAttr i o
ismap_             = jsStrProp "ismap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/usemap>
usemap_ :: (i -> JSString) -> SDOMAttr i o
usemap_            = jsStrProp "usemap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/shape>
shape_ :: (i -> JSString) -> SDOMAttr i o
shape_             = jsStrProp "shape"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/coords>
coords_ :: (i -> JSString) -> SDOMAttr i o
coords_            = jsStrProp "coords"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/src>
src_ :: (i -> JSString) -> SDOMAttr i o
src_               = jsStrProp "src"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/height>
height_ :: (i -> JSString) -> SDOMAttr i o
height_            = jsStrProp "height"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/width>
width_ :: (i -> JSString) -> SDOMAttr i o
width_             = jsStrProp "width"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/alt>
alt_ :: (i -> JSString) -> SDOMAttr i o
alt_               = jsStrProp "alt"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/autoplay>
autoplay_ :: (i -> Bool) -> SDOMAttr i o
autoplay_          = boolProp "autoplay"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/controls>
controls_ :: (i -> Bool) -> SDOMAttr i o
controls_          = boolProp "controls"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/loop>
loop_ :: (i -> Bool) -> SDOMAttr i o
loop_              = boolProp "loop"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/preload>
preload_ :: (i -> JSString) -> SDOMAttr i o
preload_           = jsStrProp "preload"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/poster>
poster_ :: (i -> JSString) -> SDOMAttr i o
poster_            = jsStrProp "poster"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/default>
default_ :: (i -> Bool) -> SDOMAttr i o
default_           = boolProp "default"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/kind>
kind_ :: (i -> JSString) -> SDOMAttr i o
kind_              = jsStrProp "kind"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/srclang>
srclang_ :: (i -> JSString) -> SDOMAttr i o
srclang_           = jsStrProp "srclang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/sandbox>
sandbox_ :: (i -> JSString) -> SDOMAttr i o
sandbox_           = jsStrProp "sandbox"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/seamless>
seamless_ :: (i -> JSString) -> SDOMAttr i o
seamless_          = jsStrProp "seamless"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/srcdoc>
srcdoc_ :: (i -> JSString) -> SDOMAttr i o
srcdoc_            = jsStrProp "srcdoc"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/reversed>
reversed_ :: (i -> JSString) -> SDOMAttr i o
reversed_          = jsStrProp "reversed"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/start>
start_ :: (i -> JSString) -> SDOMAttr i o
start_             = jsStrProp "start"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/align>
align_ :: (i -> JSString) -> SDOMAttr i o
align_             = jsStrProp "align"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/colspan>
colspan_ :: (i -> JSString) -> SDOMAttr i o
colspan_           = jsStrProp "colspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/rowspan>
rowspan_ :: (i -> JSString) -> SDOMAttr i o
rowspan_           = jsStrProp "rowspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/headers>
headers_ :: (i -> JSString) -> SDOMAttr i o
headers_           = jsStrProp "headers"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/scope>
scope_ :: (i -> JSString) -> SDOMAttr i o
scope_             = jsStrProp "scope"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/async>
async_ :: (i -> JSString) -> SDOMAttr i o
async_             = jsStrProp "async"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/charset>
charset_ :: (i -> JSString) -> SDOMAttr i o
charset_           = jsStrProp "charset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/content>
content_ :: (i -> JSString) -> SDOMAttr i o
content_           = jsStrProp "content"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/defer>
defer_ :: (i -> JSString) -> SDOMAttr i o
defer_             = jsStrProp "defer"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/httpEquiv>
httpEquiv_ :: (i -> JSString) -> SDOMAttr i o
httpEquiv_         = jsStrProp "httpEquiv"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/language>
language_ :: (i -> JSString) -> SDOMAttr i o
language_          = jsStrProp "language"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/scoped>
scoped_ :: (i -> JSString) -> SDOMAttr i o
scoped_            = jsStrProp "scoped"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/type>
type_ :: (i -> JSString) -> SDOMAttr i o
type_ = jsStrProp "type"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/name>
name_ :: (i -> JSString) -> SDOMAttr i o
name_ = jsStrProp "name"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/href>
href_ :: (i -> JSString) -> SDOMAttr i o
href_ = jsStrProp "href"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/id>
id_ :: (i -> JSString) -> SDOMAttr i o
id_ = jsStrProp "id"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/placeholder>
placeholder_ :: (i -> JSString) -> SDOMAttr i o
placeholder_ = jsStrProp "placeholder"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/checked>
checked_ :: (i -> Bool) -> SDOMAttr i o
checked_ = boolProp "checked"
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/SDOMAttr/autofocus>
autofocus_ :: (i -> Bool) -> SDOMAttr i o
autofocus_ = boolProp "autofocus"
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ :: (i -> JSString) -> SDOMAttr i o
class_ = jsStrProp "className"
