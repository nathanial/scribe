/-
  Scribe.Builder - Monadic HTML builder with phantom-typed regions and levels
-/
import Scribe.Html
import Scribe.Region

namespace Scribe

/-- Builder state accumulating HTML children -/
structure BuilderState where
  children : Array Html
  deriving Inhabited

/-- Region and Level indexed HTML builder monad.
    - `r` (Region): tracks whether content is stable (forms) or volatile (can be refreshed)
    - `l` (Level): tracks whether we're at toplevel (scripts allowed) or nested (no scripts) -/
def HtmlM (_r : Region) (_l : Level) := StateM BuilderState

namespace HtmlM

instance : Monad (HtmlM r l) where
  pure a := (pure a : StateM BuilderState _)
  bind ma f := (ma >>= f : StateM BuilderState _)

instance : MonadState BuilderState (HtmlM r l) where
  get := (get : StateM BuilderState _)
  set s := (set s : StateM BuilderState _)
  modifyGet f := (modifyGet f : StateM BuilderState _)

/-- Emit an HTML node to the builder -/
def emit (h : Html) : HtmlM r l Unit :=
  modify fun s => { s with children := s.children.push h }

/-- Emit a text node (escaped) -/
def text (s : String) : HtmlM r l Unit :=
  emit (.text s)

/-- Emit raw HTML (unescaped) -/
def raw (s : String) : HtmlM r l Unit :=
  emit (.raw s)

/-- Run a builder and collect its children -/
def collect (m : HtmlM r l Unit) : Array Html :=
  let (_, state) := (m : StateM BuilderState Unit).run { children := #[] }
  state.children

/-- Run a builder and return a fragment -/
def build (m : HtmlM r l Unit) : Html :=
  let children := collect m
  match children.toList with
  | [single] => single
  | list => .fragment list

/-- Run a builder and render to string -/
def render (m : HtmlM r l Unit) : String :=
  (build m).render

/-- Run a builder and render with pretty printing -/
def renderPretty (m : HtmlM r l Unit) : String :=
  (build m).renderPretty

end HtmlM

/-- Build an HTML element with children from a builder (polymorphic in region and level) -/
def element (tag : String) (attrs : List Attr := []) (children : HtmlM r l Unit) : HtmlM r l Unit := do
  let inner := HtmlM.collect children
  HtmlM.emit (.element tag attrs inner.toList)

/-- Build an HTML element with no children (polymorphic in region and level) -/
def emptyElement (tag : String) (attrs : List Attr := []) : HtmlM r l Unit :=
  HtmlM.emit (.element tag attrs [])

/-- Convenience for building a complete document (stable, toplevel context) -/
def document (m : HtmlM .stable .toplevel Unit) : Html :=
  HtmlM.build m

-- Region boundary combinators

/-- Create a volatile region (HTMX/SSE can swap this).
    Content inside will be refreshed when SSE events arrive.
    Children are FORCED to nested level - scripts are not allowed in SSE-refreshable regions. -/
def volatileRegion (id : String) (attrs : List Attr := [])
    (children : HtmlM .volatile .nested Unit) : HtmlM r l Unit := do
  let inner := HtmlM.collect children
  HtmlM.emit (.element "div" ([id_ id] ++ attrs) inner.toList)

/-- Create a stable region (contains ephemeral user state).
    Forms and user input should be inside stable regions.
    Children are FORCED to nested level - scripts belong at document toplevel. -/
def stableRegion (id : String) (attrs : List Attr := [])
    (children : HtmlM .stable .nested Unit) : HtmlM r l Unit := do
  let inner := HtmlM.collect children
  HtmlM.emit (.element "div" ([id_ id] ++ attrs) inner.toList)

/-- Lift stable content into any region context (safe direction).
    Stable content is always safe to include anywhere.
    Level must match. -/
def liftStable (m : HtmlM .stable l Unit) : HtmlM r l Unit :=
  (m : StateM BuilderState Unit)

-- Level boundary combinators

/-- Create a nested level context (no scripts allowed inside).
    Components should use this to prevent script injection. -/
def nestedLevel (m : HtmlM r .nested Unit) : HtmlM r l Unit :=
  (m : StateM BuilderState Unit)

/-- Lift nested content to any level (safe direction).
    Nested content is always safe to include at toplevel. -/
def liftNested (m : HtmlM r .nested Unit) : HtmlM r l Unit :=
  (m : StateM BuilderState Unit)

end Scribe
