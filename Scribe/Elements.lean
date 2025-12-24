/-
  Scribe.Elements - HTML element builder functions with region safety

  Elements are divided into:
  - Polymorphic elements (div, span, p, etc.) - work in any region
  - Stable-only elements (form, input, textarea, select) - only in stable regions

  This prevents forms from being placed inside HTMX-refreshable regions,
  which would cause user input to be lost on refresh.
-/
import Scribe.Builder
import Scribe.Attr

namespace Scribe

-- ============================================================================
-- Document structure (polymorphic)
-- ============================================================================

def html (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "html" attrs children

def head (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "head" attrs children

def body (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "body" attrs children

def title (content : String) : HtmlM r Unit :=
  element "title" [] (HtmlM.text content)

def meta_ (attrs : List Attr) : HtmlM r Unit :=
  emptyElement "meta" attrs

def link (attrs : List Attr) : HtmlM r Unit :=
  emptyElement "link" attrs

def script (attrs : List Attr := []) (content : String := "") : HtmlM r Unit :=
  if content.isEmpty then
    emptyElement "script" attrs
  else
    element "script" attrs (HtmlM.raw content)

def style (attrs : List Attr := []) (content : String) : HtmlM r Unit :=
  element "style" attrs (HtmlM.raw content)

-- ============================================================================
-- Semantic structure (polymorphic)
-- ============================================================================

def header (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "header" attrs children

def footer (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "footer" attrs children

def main (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "main" attrs children

def nav (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "nav" attrs children

def aside (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "aside" attrs children

def section_ (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "section" attrs children

def article (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "article" attrs children

-- ============================================================================
-- Block elements (polymorphic)
-- ============================================================================

def div (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "div" attrs children

def p (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "p" attrs children

def h1 (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "h1" attrs children

def h2 (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "h2" attrs children

def h3 (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "h3" attrs children

def h4 (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "h4" attrs children

def h5 (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "h5" attrs children

def h6 (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "h6" attrs children

def blockquote (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "blockquote" attrs children

def pre (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "pre" attrs children

def hr (attrs : List Attr := []) : HtmlM r Unit :=
  emptyElement "hr" attrs

-- ============================================================================
-- Lists (polymorphic)
-- ============================================================================

def ul (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "ul" attrs children

def ol (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "ol" attrs children

def li (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "li" attrs children

def dl (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "dl" attrs children

def dt (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "dt" attrs children

def dd (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "dd" attrs children

-- ============================================================================
-- Tables (polymorphic)
-- ============================================================================

def table (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "table" attrs children

def thead (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "thead" attrs children

def tbody (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "tbody" attrs children

def tfoot (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "tfoot" attrs children

def tr (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "tr" attrs children

def th (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "th" attrs children

def td (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "td" attrs children

def caption (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "caption" attrs children

-- ============================================================================
-- Inline elements (polymorphic)
-- ============================================================================

def span (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "span" attrs children

def a (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "a" attrs children

def strong (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "strong" attrs children

def em (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "em" attrs children

def b (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "b" attrs children

def i (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "i" attrs children

def u (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "u" attrs children

def s (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "s" attrs children

def code (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "code" attrs children

def kbd (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "kbd" attrs children

def samp (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "samp" attrs children

def var (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "var" attrs children

def small (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "small" attrs children

def sub (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "sub" attrs children

def sup (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "sup" attrs children

def mark (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "mark" attrs children

def abbr (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "abbr" attrs children

def time (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "time" attrs children

def br : HtmlM r Unit :=
  emptyElement "br" []

-- ============================================================================
-- Media (polymorphic)
-- ============================================================================

def img (attrs : List Attr) : HtmlM r Unit :=
  emptyElement "img" attrs

def audio (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "audio" attrs children

def video (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "video" attrs children

def source (attrs : List Attr) : HtmlM r Unit :=
  emptyElement "source" attrs

def picture (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "picture" attrs children

def figure (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "figure" attrs children

def figcaption (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "figcaption" attrs children

def iframe (attrs : List Attr := []) (children : HtmlM r Unit := pure ()) : HtmlM r Unit :=
  element "iframe" attrs children

-- ============================================================================
-- Forms - STABLE ONLY
-- These elements contain user input that would be lost on refresh.
-- They can ONLY be used in stable regions.
-- ============================================================================

/-- Form element - ONLY available in stable regions.
    Forms contain user input that would be lost if refreshed. -/
def form (attrs : List Attr := []) (children : HtmlM .stable Unit) : HtmlM .stable Unit :=
  element "form" attrs children

/-- Input element - ONLY available in stable regions.
    User input would be lost if placed in a volatile region. -/
def input (attrs : List Attr) : HtmlM .stable Unit :=
  emptyElement "input" attrs

/-- Textarea element - ONLY available in stable regions.
    User input would be lost if placed in a volatile region. -/
def textarea (attrs : List Attr := []) (content : String := "") : HtmlM .stable Unit :=
  element "textarea" attrs (HtmlM.text content)

/-- Button element - polymorphic since it doesn't hold state.
    Can be used in volatile regions for triggering actions. -/
def button (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "button" attrs children

/-- Select element - ONLY available in stable regions.
    User selection would be lost if placed in a volatile region. -/
def select (attrs : List Attr := []) (children : HtmlM .stable Unit) : HtmlM .stable Unit :=
  element "select" attrs children

/-- Option element - ONLY available in stable regions (inside select). -/
def option (attrs : List Attr := []) (content : String) : HtmlM .stable Unit :=
  element "option" attrs (HtmlM.text content)

/-- Optgroup element - ONLY available in stable regions (inside select). -/
def optgroup (attrs : List Attr := []) (children : HtmlM .stable Unit) : HtmlM .stable Unit :=
  element "optgroup" attrs children

/-- Label element - polymorphic since it's just text/display. -/
def label (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "label" attrs children

/-- Fieldset element - ONLY available in stable regions (contains forms). -/
def fieldset (attrs : List Attr := []) (children : HtmlM .stable Unit) : HtmlM .stable Unit :=
  element "fieldset" attrs children

/-- Legend element - ONLY available in stable regions (inside fieldset). -/
def legend (attrs : List Attr := []) (children : HtmlM .stable Unit) : HtmlM .stable Unit :=
  element "legend" attrs children

/-- Datalist element - ONLY available in stable regions. -/
def datalist (attrs : List Attr := []) (children : HtmlM .stable Unit) : HtmlM .stable Unit :=
  element "datalist" attrs children

/-- Output element - polymorphic since it displays computed values. -/
def output (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "output" attrs children

/-- Progress element - polymorphic since it's display only. -/
def progress (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "progress" attrs children

/-- Meter element - polymorphic since it's display only. -/
def meter (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "meter" attrs children

-- ============================================================================
-- Interactive elements (polymorphic)
-- ============================================================================

def details (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "details" attrs children

def summary (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "summary" attrs children

def dialog (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit :=
  element "dialog" attrs children

-- ============================================================================
-- Text shorthand (polymorphic)
-- ============================================================================

def text (s : String) : HtmlM r Unit := HtmlM.text s
def raw (s : String) : HtmlM r Unit := HtmlM.raw s

end Scribe
