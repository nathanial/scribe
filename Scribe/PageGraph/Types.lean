/-
  Scribe.PageGraph.Types - Core types for the HTMX interaction graph model

  This module defines the fundamental types that represent an HTMX-enhanced
  page as a graph of regions (nodes) and interactions (edges).
-/

namespace Scribe.PageGraph

/-- The kind of a region determines its HTMX behavior -/
inductive RegionKind where
  | stable    -- Contains ephemeral user state (forms, inputs). Never HTMX-refreshed.
  | volatile  -- Display-only content. Can be updated via HTMX swap.
  deriving Repr, BEq, DecidableEq, Inhabited

/-- HTTP method for HTMX requests -/
inductive HttpMethod where
  | get
  | post
  | put
  | patch
  | delete
  deriving Repr, BEq, DecidableEq, Inhabited

namespace HttpMethod

def toAttrName : HttpMethod → String
  | .get => "hx-get"
  | .post => "hx-post"
  | .put => "hx-put"
  | .patch => "hx-patch"
  | .delete => "hx-delete"

end HttpMethod

/-- HTMX swap modes for content replacement -/
inductive SwapMode where
  | innerHTML    -- Replace inner content (default)
  | outerHTML    -- Replace entire element
  | beforebegin  -- Insert before the element
  | afterbegin   -- Insert at start of element
  | beforeend    -- Insert at end of element (append)
  | afterend     -- Insert after the element
  | delete       -- Delete the target element
  | none         -- Don't swap, just trigger events
  deriving Repr, BEq, DecidableEq, Inhabited

namespace SwapMode

def toAttrValue : SwapMode → String
  | .innerHTML => "innerHTML"
  | .outerHTML => "outerHTML"
  | .beforebegin => "beforebegin"
  | .afterbegin => "afterbegin"
  | .beforeend => "beforeend"
  | .afterend => "afterend"
  | .delete => "delete"
  | .none => "none"

end SwapMode

/-- What event triggers the HTMX request -/
inductive TriggerEvent where
  | click                   -- Mouse click (default for most elements)
  | submit                  -- Form submission
  | change                  -- Input value change
  | load                    -- Element load (for lazy loading)
  | revealed                -- Element scrolled into view
  | intersect               -- Intersection observer
  | every (seconds : Nat)   -- Polling interval
  | custom (event : String) -- Custom event name
  deriving Repr, BEq, Inhabited

namespace TriggerEvent

def toAttrValue : TriggerEvent → String
  | .click => "click"
  | .submit => "submit"
  | .change => "change"
  | .load => "load"
  | .revealed => "revealed"
  | .intersect => "intersect"
  | .every s => s!"every {s}s"
  | .custom e => e

end TriggerEvent

/-- A region in the page graph (a node) -/
structure Region where
  id : String
  kind : RegionKind
  deriving Repr, BEq, Inhabited

/-- An interaction in the page graph (an edge) -/
structure Interaction where
  id : String           -- Unique identifier for this interaction
  sourceRegion : String -- Region containing the trigger element
  method : HttpMethod   -- HTTP method (GET, POST, etc.)
  route : String        -- URL path for the request
  targetRegion : String -- Region that receives the response
  swap : SwapMode       -- How to swap the content
  trigger : TriggerEvent -- What triggers the request
  deriving Repr, BEq, Inhabited

/-- The complete page interaction graph -/
structure PageGraph where
  regions : List Region
  interactions : List Interaction
  deriving Repr, Inhabited

namespace PageGraph

/-- Empty page graph -/
def empty : PageGraph := { regions := [], interactions := [] }

/-- Add a region to the graph -/
def addRegion (pg : PageGraph) (r : Region) : PageGraph :=
  { pg with regions := pg.regions ++ [r] }

/-- Add an interaction to the graph -/
def addInteraction (pg : PageGraph) (i : Interaction) : PageGraph :=
  { pg with interactions := pg.interactions ++ [i] }

/-- Find a region by ID -/
def findRegion (pg : PageGraph) (id : String) : Option Region :=
  pg.regions.find? (·.id == id)

/-- Check if a region exists -/
def hasRegion (pg : PageGraph) (id : String) : Bool :=
  pg.regions.any (·.id == id)

end PageGraph

end Scribe.PageGraph
