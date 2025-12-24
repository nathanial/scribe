/-
  Scribe.PageGraph.Builder - Graph-aware HTML builder monad

  Extends the standard HtmlM with state that tracks regions and interactions
  as they are created, building up a PageGraph alongside the HTML.
-/
import Scribe.Html
import Scribe.Region
import Scribe.PageGraph.Types
import Scribe.PageGraph.RegionRef

namespace Scribe.PageGraph

/-- Extended builder state that tracks both HTML and the interaction graph -/
structure GraphBuilderState where
  /-- Accumulated HTML children (same as standard BuilderState) -/
  children : Array Html
  /-- Registered regions -/
  regions : Array Region
  /-- Registered interactions -/
  interactions : Array Interaction
  /-- Current region context (what region are we building inside?) -/
  currentRegion : Option String
  /-- Counter for generating unique interaction IDs -/
  nextInteractionId : Nat
  deriving Inhabited

namespace GraphBuilderState

/-- Initial state with a root region -/
def empty : GraphBuilderState := {
  children := #[]
  regions := #[{ id := "root", kind := .volatile }]  -- Root region is always present
  interactions := #[]
  currentRegion := some "root"  -- Start inside the root region
  nextInteractionId := 0
}

/-- Extract the accumulated PageGraph -/
def toPageGraph (s : GraphBuilderState) : PageGraph := {
  regions := s.regions.toList
  interactions := s.interactions.toList
}

end GraphBuilderState

/-- Graph-aware HTML builder monad.
    Like HtmlM but also tracks regions and interactions.
    - `r` (Region): tracks whether content is stable or volatile
    - `l` (Level): tracks whether we're at toplevel or nested -/
def GraphHtmlM (_r : Scribe.Region) (_l : Level) := StateM GraphBuilderState

namespace GraphHtmlM

instance : Monad (GraphHtmlM r l) where
  pure a := (pure a : StateM GraphBuilderState _)
  bind ma f := (ma >>= f : StateM GraphBuilderState _)

instance : MonadState GraphBuilderState (GraphHtmlM r l) where
  get := (get : StateM GraphBuilderState _)
  set s := (set s : StateM GraphBuilderState _)
  modifyGet f := (modifyGet f : StateM GraphBuilderState _)

/-- Emit an HTML node to the builder -/
def emit (h : Html) : GraphHtmlM r l Unit :=
  modify fun s => { s with children := s.children.push h }

/-- Emit a text node (escaped) -/
def text (s : String) : GraphHtmlM r l Unit :=
  emit (.text s)

/-- Emit raw HTML (unescaped) -/
def raw (s : String) : GraphHtmlM r l Unit :=
  emit (.raw s)

/-- Register a region in the graph -/
def registerRegion (region : Region) : GraphHtmlM r l Unit :=
  modify fun s => { s with regions := s.regions.push region }

/-- Register an interaction in the graph -/
def registerInteraction (interaction : Interaction) : GraphHtmlM r l Unit :=
  modify fun s => { s with interactions := s.interactions.push interaction }

/-- Get the current region context -/
def getCurrentRegion : GraphHtmlM r l (Option String) := do
  return (← get).currentRegion

/-- Set the current region context, returning the previous value -/
def setCurrentRegion (regionId : Option String) : GraphHtmlM r l (Option String) :=
  modifyGet fun s => (s.currentRegion, { s with currentRegion := regionId })

/-- Generate a unique interaction ID -/
def freshInteractionId : GraphHtmlM r l String := do
  let id := (← get).nextInteractionId
  modify fun s => { s with nextInteractionId := s.nextInteractionId + 1 }
  pure s!"interaction-{id}"

/-- Run a nested builder and collect its children without affecting parent's children -/
def collectChildren (m : GraphHtmlM r l Unit) : GraphHtmlM r l (Array Html) := do
  let savedChildren := (← get).children
  modify fun s => { s with children := #[] }
  m
  let result := (← get).children
  modify fun s => { s with children := savedChildren }
  pure result

/-- Extract the PageGraph from the current state -/
def getPageGraph : GraphHtmlM r l PageGraph := do
  return (← get).toPageGraph

/-- Run the builder and get both HTML and PageGraph -/
def build (m : GraphHtmlM r l Unit) : Html × PageGraph :=
  let (_, state) := StateT.run m GraphBuilderState.empty
  let html := match state.children.toList with
    | [single] => single
    | list => Html.fragment list
  (html, state.toPageGraph)

/-- Run and render to string (for volatile content) -/
def render (m : GraphHtmlM .volatile l Unit) : String × PageGraph :=
  let (html, graph) := build m
  (html.render, graph)

/-- Run and render to string (for stable content like full pages) -/
def renderStable (m : GraphHtmlM .stable l Unit) : String × PageGraph :=
  let (html, graph) := build m
  (html.render, graph)

end GraphHtmlM

/-- Build an element with children (graph-aware version) -/
def element (tag : String) (attrs : List Attr := [])
    (children : GraphHtmlM r l Unit) : GraphHtmlM r l Unit := do
  let inner ← GraphHtmlM.collectChildren children
  GraphHtmlM.emit (.element tag attrs inner.toList)

/-- Build an empty element (graph-aware version) -/
def emptyElement (tag : String) (attrs : List Attr := []) : GraphHtmlM r l Unit :=
  GraphHtmlM.emit (.element tag attrs [])

end Scribe.PageGraph
