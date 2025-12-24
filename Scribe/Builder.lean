/-
  Scribe.Builder - Monadic HTML builder with phantom-typed regions
-/
import Scribe.Html
import Scribe.Region

namespace Scribe

/-- Builder state accumulating HTML children -/
structure BuilderState where
  children : Array Html
  deriving Inhabited

/-- Region-indexed HTML builder monad.
    The phantom type parameter `r` tracks whether we're building
    stable content (forms, user input) or volatile content (can be refreshed). -/
def HtmlM (_r : Region) := StateM BuilderState

namespace HtmlM

instance : Monad (HtmlM r) where
  pure a := (pure a : StateM BuilderState _)
  bind ma f := (ma >>= f : StateM BuilderState _)

instance : MonadState BuilderState (HtmlM r) where
  get := (get : StateM BuilderState _)
  set s := (set s : StateM BuilderState _)
  modifyGet f := (modifyGet f : StateM BuilderState _)

/-- Emit an HTML node to the builder -/
def emit (h : Html) : HtmlM r Unit :=
  modify fun s => { s with children := s.children.push h }

/-- Emit a text node (escaped) -/
def text (s : String) : HtmlM r Unit :=
  emit (.text s)

/-- Emit raw HTML (unescaped) -/
def raw (s : String) : HtmlM r Unit :=
  emit (.raw s)

/-- Run a builder and collect its children -/
def collect (m : HtmlM r Unit) : Array Html :=
  let (_, state) := (m : StateM BuilderState Unit).run { children := #[] }
  state.children

/-- Run a builder and return a fragment -/
def build (m : HtmlM r Unit) : Html :=
  let children := collect m
  match children.toList with
  | [single] => single
  | list => .fragment list

/-- Run a builder and render to string -/
def render (m : HtmlM r Unit) : String :=
  (build m).render

/-- Run a builder and render with pretty printing -/
def renderPretty (m : HtmlM r Unit) : String :=
  (build m).renderPretty

end HtmlM

/-- Build an HTML element with children from a builder (region-polymorphic) -/
def element (tag : String) (attrs : List Attr := []) (children : HtmlM r Unit) : HtmlM r Unit := do
  let inner := HtmlM.collect children
  HtmlM.emit (.element tag attrs inner.toList)

/-- Build an HTML element with no children (region-polymorphic) -/
def emptyElement (tag : String) (attrs : List Attr := []) : HtmlM r Unit :=
  HtmlM.emit (.element tag attrs [])

/-- Convenience for building a complete document (stable context) -/
def document (m : HtmlM .stable Unit) : Html :=
  HtmlM.build m

-- Region boundary combinators

/-- Create a volatile region (HTMX/SSE can swap this).
    Content inside will be refreshed when SSE events arrive. -/
def volatileRegion (id : String) (attrs : List Attr := [])
    (children : HtmlM .volatile Unit) : HtmlM r Unit := do
  let inner := HtmlM.collect children
  HtmlM.emit (.element "div" ([id_ id] ++ attrs) inner.toList)

/-- Create a stable region (contains ephemeral user state).
    Forms and user input should be inside stable regions. -/
def stableRegion (id : String) (attrs : List Attr := [])
    (children : HtmlM .stable Unit) : HtmlM r Unit := do
  let inner := HtmlM.collect children
  HtmlM.emit (.element "div" ([id_ id] ++ attrs) inner.toList)

/-- Lift stable content into any region context (safe direction).
    Stable content is always safe to include anywhere. -/
def liftStable (m : HtmlM .stable Unit) : HtmlM r Unit :=
  (m : StateM BuilderState Unit)

end Scribe
