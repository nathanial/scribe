/-
  Scribe.Elements - HTML element builder functions with region and level safety

  Elements are divided into:
  - Polymorphic elements (div, span, p, etc.) - work in any region and level
  - Stable-only elements (form, input, textarea, select) - only in stable regions
  - Toplevel-only elements (script, style) - only at document toplevel

  This prevents:
  - Forms being placed inside HTMX-refreshable regions (lost input)
  - Scripts being injected inside components (security)
-/
import Scribe.Builder
import Scribe.Attr

namespace Scribe

-- ============================================================================
-- Document structure (polymorphic in region and level)
-- ============================================================================

def html (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "html" attrs children

def head (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "head" attrs children

def body (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "body" attrs children

def title (content : String) : HtmlM r l Unit :=
  element "title" [] (HtmlM.text content)

def meta_ (attrs : List Attr) : HtmlM r l Unit :=
  emptyElement "meta" attrs

def link (attrs : List Attr) : HtmlM r l Unit :=
  emptyElement "link" attrs

-- ============================================================================
-- Toplevel-only elements (scripts, styles)
-- These can ONLY be used at document toplevel, not inside components.
-- ============================================================================

/-- Script element - ONLY available at toplevel.
    Prevents script injection inside components. -/
def script (attrs : List Attr := []) (content : String := "") : HtmlM r .toplevel Unit :=
  if content.isEmpty then
    emptyElement "script" attrs
  else
    element "script" attrs (HtmlM.raw content)

/-- Style element - ONLY available at toplevel.
    CSS should be defined at document level, not inline in components. -/
def style (attrs : List Attr := []) (content : String) : HtmlM r .toplevel Unit :=
  element "style" attrs (HtmlM.raw content)

-- ============================================================================
-- Semantic structure (polymorphic in region and level)
-- ============================================================================

def header (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "header" attrs children

def footer (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "footer" attrs children

def main (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "main" attrs children

def nav (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "nav" attrs children

def aside (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "aside" attrs children

def section_ (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "section" attrs children

def article (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "article" attrs children

-- ============================================================================
-- Block elements (polymorphic in region and level)
-- ============================================================================

def div (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "div" attrs children

def p (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "p" attrs children

def h1 (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "h1" attrs children

def h2 (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "h2" attrs children

def h3 (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "h3" attrs children

def h4 (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "h4" attrs children

def h5 (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "h5" attrs children

def h6 (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "h6" attrs children

def blockquote (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "blockquote" attrs children

def pre (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "pre" attrs children

def hr (attrs : List Attr := []) : HtmlM r l Unit :=
  emptyElement "hr" attrs

-- ============================================================================
-- Lists (polymorphic in region and level)
-- ============================================================================

def ul (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "ul" attrs children

def ol (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "ol" attrs children

def li (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "li" attrs children

def dl (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "dl" attrs children

def dt (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "dt" attrs children

def dd (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "dd" attrs children

-- ============================================================================
-- Tables (polymorphic in region and level)
-- ============================================================================

def table (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "table" attrs children

def thead (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "thead" attrs children

def tbody (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "tbody" attrs children

def tfoot (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "tfoot" attrs children

def tr (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "tr" attrs children

def th (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "th" attrs children

def td (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "td" attrs children

def caption (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "caption" attrs children

-- ============================================================================
-- Inline elements (polymorphic in region and level)
-- ============================================================================

def span (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "span" attrs children

def a (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "a" attrs children

def strong (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "strong" attrs children

def em (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "em" attrs children

def b (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "b" attrs children

def i (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "i" attrs children

def u (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "u" attrs children

def s (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "s" attrs children

def code (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "code" attrs children

def kbd (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "kbd" attrs children

def samp (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "samp" attrs children

def var (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "var" attrs children

def small (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "small" attrs children

def sub (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "sub" attrs children

def sup (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "sup" attrs children

def mark (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "mark" attrs children

def abbr (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "abbr" attrs children

def time (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "time" attrs children

def br : HtmlM r l Unit :=
  emptyElement "br" []

-- ============================================================================
-- Media (polymorphic in region and level)
-- ============================================================================

def img (attrs : List Attr) : HtmlM r l Unit :=
  emptyElement "img" attrs

def audio (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "audio" attrs children

def video (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "video" attrs children

def source (attrs : List Attr) : HtmlM r l Unit :=
  emptyElement "source" attrs

def picture (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "picture" attrs children

def figure (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "figure" attrs children

def figcaption (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "figcaption" attrs children

def iframe (attrs : List Attr := []) (children : HtmlM r l Unit := pure ()) : HtmlM r l Unit :=
  element "iframe" attrs children

-- ============================================================================
-- Forms - STABLE ONLY (level polymorphic)
-- These elements contain user input that would be lost on refresh.
-- They can ONLY be used in stable regions, but any level.
-- ============================================================================

/-- Form element - ONLY available in stable regions.
    Forms contain user input that would be lost if refreshed. -/
def form (attrs : List Attr := []) (children : HtmlM .stable l Unit) : HtmlM .stable l Unit :=
  element "form" attrs children

/-- Input element - ONLY available in stable regions.
    User input would be lost if placed in a volatile region. -/
def input (attrs : List Attr) : HtmlM .stable l Unit :=
  emptyElement "input" attrs

/-- Textarea element - ONLY available in stable regions.
    User input would be lost if placed in a volatile region. -/
def textarea (attrs : List Attr := []) (content : String := "") : HtmlM .stable l Unit :=
  element "textarea" attrs (HtmlM.text content)

/-- Button element - polymorphic since it doesn't hold state.
    Can be used in volatile regions for triggering actions. -/
def button (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "button" attrs children

/-- Select element - ONLY available in stable regions.
    User selection would be lost if placed in a volatile region. -/
def select (attrs : List Attr := []) (children : HtmlM .stable l Unit) : HtmlM .stable l Unit :=
  element "select" attrs children

/-- Option element - ONLY available in stable regions (inside select). -/
def option (attrs : List Attr := []) (content : String) : HtmlM .stable l Unit :=
  element "option" attrs (HtmlM.text content)

/-- Optgroup element - ONLY available in stable regions (inside select). -/
def optgroup (attrs : List Attr := []) (children : HtmlM .stable l Unit) : HtmlM .stable l Unit :=
  element "optgroup" attrs children

/-- Label element - polymorphic since it's just text/display. -/
def label (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "label" attrs children

/-- Fieldset element - ONLY available in stable regions (contains forms). -/
def fieldset (attrs : List Attr := []) (children : HtmlM .stable l Unit) : HtmlM .stable l Unit :=
  element "fieldset" attrs children

/-- Legend element - ONLY available in stable regions (inside fieldset). -/
def legend (attrs : List Attr := []) (children : HtmlM .stable l Unit) : HtmlM .stable l Unit :=
  element "legend" attrs children

/-- Datalist element - ONLY available in stable regions. -/
def datalist (attrs : List Attr := []) (children : HtmlM .stable l Unit) : HtmlM .stable l Unit :=
  element "datalist" attrs children

/-- Output element - polymorphic since it displays computed values. -/
def output (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "output" attrs children

/-- Progress element - polymorphic since it's display only. -/
def progress (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "progress" attrs children

/-- Meter element - polymorphic since it's display only. -/
def meter (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "meter" attrs children

-- ============================================================================
-- Interactive elements (polymorphic in region and level)
-- ============================================================================

def details (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "details" attrs children

def summary (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "summary" attrs children

def dialog (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit :=
  element "dialog" attrs children

-- ============================================================================
-- Text shorthand (polymorphic in region and level)
-- ============================================================================

def text (s : String) : HtmlM r l Unit := HtmlM.text s
def raw (s : String) : HtmlM r l Unit := HtmlM.raw s

end Scribe
