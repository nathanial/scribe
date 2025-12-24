/-
  Scribe.PageGraph.RegionRef - Region references for type-safe targeting

  RegionRef is a handle to a region that can be passed to interactions,
  ensuring type-safe targeting of HTMX swaps.
-/
import Scribe.PageGraph.Types

namespace Scribe.PageGraph

/-- A reference to a region, returned when creating regions.
    Used for type-safe targeting in interactions. -/
structure RegionRef where
  id : String
  kind : RegionKind
  deriving Repr, BEq, Inhabited

namespace RegionRef

/-- Convert to the underlying Region type -/
def toRegion (ref : RegionRef) : Region :=
  { id := ref.id, kind := ref.kind }

/-- Create a volatile region reference -/
def volatile (id : String) : RegionRef :=
  { id, kind := .volatile }

/-- Create a stable region reference -/
def stable (id : String) : RegionRef :=
  { id, kind := .stable }

end RegionRef

/-- Target specification for HTMX interactions.
    Determines what element receives the swapped content. -/
inductive Target where
  /-- Target a specific region by reference -/
  | region (ref : RegionRef)
  /-- Target the element that triggered the request (hx-target="this") -/
  | self
  /-- Target the closest ancestor matching the selector (hx-target="closest ...") -/
  | closest (selector : String)
  /-- Target a descendant matching the selector (hx-target="find ...") -/
  | find (selector : String)
  /-- Target by CSS selector directly (hx-target="...") -/
  | selector (sel : String)
  deriving Repr, BEq, Inhabited

namespace Target

/-- Convert target to hx-target attribute value -/
def toAttrValue : Target → String
  | .region ref => s!"#{ref.id}"
  | .self => "this"
  | .closest sel => s!"closest {sel}"
  | .find sel => s!"find {sel}"
  | .selector sel => sel

/-- Get the region ID if this targets a specific region -/
def regionId? : Target → Option String
  | .region ref => some ref.id
  | _ => none

/-- Check if this target references a known region -/
def isRegionTarget : Target → Bool
  | .region _ => true
  | _ => false

end Target

end Scribe.PageGraph
