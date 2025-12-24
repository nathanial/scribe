/-
  Scribe.PageGraph.Regions - Region builders that track in the graph

  These functions create HTML regions (divs with IDs) while also registering
  them in the PageGraph for verification.
-/
import Scribe.Attr
import Scribe.PageGraph.Builder

namespace Scribe.PageGraph

open Scribe (Attr id_)

/-- Create a volatile region that can be HTMX-swapped.
    Returns a RegionRef for use in interaction targeting.

    Children are forced to nested level (no scripts in HTMX regions).
    The region is registered in the PageGraph. -/
def volatileRegion (id : String) (attrs : List Attr := [])
    (children : GraphHtmlM .volatile .nested Unit) : GraphHtmlM r l RegionRef := do
  -- Register region in graph
  GraphHtmlM.registerRegion { id, kind := .volatile }

  -- Save and set current region context
  let prevRegion ← GraphHtmlM.setCurrentRegion (some id)

  -- Collect children HTML
  let inner ← GraphHtmlM.collectChildren children

  -- Restore previous region context
  let _ ← GraphHtmlM.setCurrentRegion prevRegion

  -- Emit the div element
  GraphHtmlM.emit (.element "div" ([id_ id] ++ attrs) inner.toList)

  -- Return reference for targeting
  pure { id, kind := .volatile }

/-- Create a stable region that contains user state (forms, inputs).
    Stable regions should never be HTMX-swapped.
    Returns a RegionRef for reference (though targeting stable regions is invalid).

    Children are forced to nested level.
    The region is registered in the PageGraph. -/
def stableRegion (id : String) (attrs : List Attr := [])
    (children : GraphHtmlM .stable .nested Unit) : GraphHtmlM r l RegionRef := do
  -- Register region in graph
  GraphHtmlM.registerRegion { id, kind := .stable }

  -- Save and set current region context
  let prevRegion ← GraphHtmlM.setCurrentRegion (some id)

  -- Collect children HTML
  let inner ← GraphHtmlM.collectChildren children

  -- Restore previous region context
  let _ ← GraphHtmlM.setCurrentRegion prevRegion

  -- Emit the div element
  GraphHtmlM.emit (.element "div" ([id_ id] ++ attrs) inner.toList)

  -- Return reference
  pure { id, kind := .stable }

/-- Create a root region that represents the entire page.
    This is typically the body or a main container.
    Useful for establishing the root of the interaction graph. -/
def rootRegion (id : String) (attrs : List Attr := [])
    (children : GraphHtmlM .stable .toplevel Unit) : GraphHtmlM .stable .toplevel RegionRef := do
  -- Register as volatile since it could theoretically be swapped
  GraphHtmlM.registerRegion { id, kind := .volatile }

  -- Set as current region
  let prevRegion ← GraphHtmlM.setCurrentRegion (some id)

  -- Collect children
  let inner ← GraphHtmlM.collectChildren children

  -- Restore
  let _ ← GraphHtmlM.setCurrentRegion prevRegion

  -- Emit
  GraphHtmlM.emit (.element "div" ([id_ id] ++ attrs) inner.toList)

  pure { id, kind := .volatile }

end Scribe.PageGraph
