{-# LANGUAGE OverloadedStrings #-}
module SDOM.Html where

import Haste.Prim
import Haste.DOM.JSString (Elem, newTextElem, newElem, setAttr, appendChild, setProp)
import SDOM

node :: JSString -> [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
node = SDOMElement

text_ :: JSString -> SDOM i o
text_ = SDOMText

textD_ :: (i -> JSString) -> SDOM i o
textD_ = SDOMTextDyn

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
div_ = node "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
table_ = node "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
thead_ = node "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
tbody_ = node "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
tr_ = node "tr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
th_ = node "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
td_ = node "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
tfoot_ = node "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
section_ = node "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
header_ = node "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
footer_ = node "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
button_ = node "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
form_ = node "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
p_ = node "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
s_ = node "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
ul_ = node "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
span_ = node "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
strong_ = node "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
li_ = node "li"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
h1_ = node "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
h2_ = node "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
h3_ = node "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
h4_ = node "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
h5_ = node "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
h6_ = node "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: [SDOMAttr i o] -> SDOM i o
hr_ = flip (node "hr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
pre_ = node "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: [SDOMAttr i o] -> SDOM i o
input_ = flip (node "input") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
label_ = node "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
a_ = node "a"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
mark_ = node "mark"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
ruby_ = node "ruby"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
rt_ = node "rt"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
rp_ = node "rp"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
bdi_ = node "bdi"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
bdo_ = node "bdo"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: [SDOMAttr i o] -> SDOM i o
wbr_ = flip (node "wbr") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
details_ = node "details"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
summary_ = node "summary"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
menuitem_ = node "menuitem"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
menu_ = node "menu"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
fieldset_ = node "fieldset"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
legend_ = node "legend"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
datalist_ = node "datalist"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
optgroup_ = node "optgroup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
keygen_ = node "keygen"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
output_ = node "output"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
progress_ = node "progress"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
meter_ = node "meter"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
center_ = node "center"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
audio_ = node "audio"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
video_ = node "video"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: [SDOMAttr i o] -> SDOM i o
source_ = flip (node "source") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: [SDOMAttr i o] -> SDOM i o
track_ = flip (node "track") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: [SDOMAttr i o] -> SDOM i o
embed_ = flip (node "embed") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
object_ = node "object"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: [SDOMAttr i o] -> SDOM i o
param_ = flip (node "param") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
ins_ = node "ins"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
del_ = node "del"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
small_ = node "small"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
cite_ = node "cite"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
dfn_ = node "dfn"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
abbr_ = node "abbr"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
time_ = node "time"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
var_ = node "var"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
samp_ = node "samp"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
kbd_ = node "kbd"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
caption_ = node "caption"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
colgroup_ = node "colgroup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: [SDOMAttr i o] -> SDOM i o
col_ = flip (node "col") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
nav_ = node "nav"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
article_ = node "article"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
aside_ = node "aside"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
address_ = node "address"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
main_ = node "main"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
body_ = node "body"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
figure_ = node "figure"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
figcaption_ = node "figcaption"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
dl_ = node "dl"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
dt_ = node "dt"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
dd_ = node "dd"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: [SDOMAttr i o] -> SDOM i o
img_ = flip (node "img") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
iframe_ = node "iframe"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
canvas_ = node "canvas"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
math_ = node "math"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
select_ = node "select"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
option_ = node "option"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
textarea_ = node "textarea"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
sub_ = node "sub"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
sup_ = node "sup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: [SDOMAttr i o] -> SDOM i o
br_ = flip (node "br") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
ol_ = node "ol"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
blockquote_ = node "blockquote"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
code_ = node "code"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
em_ = node "em"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
i_ = node "i"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
b_ = node "b"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
u_ = node "u"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
q_ = node "q"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: [SDOMAttr i o] -> [SDOM i o] -> SDOM i o
script_ = node "script"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: [SDOMAttr i o] -> SDOM i o
link_ = flip (node "link") []
