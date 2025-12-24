/-
  Tests for compile-time PageGraph verification
-/
import Scribe.PageGraph

open Scribe.PageGraph

-- ============================================================================
-- Example 1: A valid page graph (manually constructed)
-- ============================================================================

def validGraph : PageGraph := {
  regions := [
    { id := "root", kind := .volatile },
    { id := "board", kind := .volatile }
  ]
  interactions := [
    { id := "i1", sourceRegion := "root", method := .get,
      route := "/board", targetRegion := "board",
      swap := .innerHTML, trigger := .load }
  ]
}

-- Compile-time verification: this theorem only compiles if the graph is valid!
theorem validGraph_wellformed : validGraph.checkWellFormed = true := by native_decide

-- ============================================================================
-- Example 2: Invalid graph - missing target (should NOT compile if uncommented)
-- ============================================================================

def invalidGraph_missingTarget : PageGraph := {
  regions := [
    { id := "root", kind := .volatile }
  ]
  interactions := [
    { id := "i1", sourceRegion := "root", method := .get,
      route := "/board", targetRegion := "nonexistent",  -- This region doesn't exist!
      swap := .innerHTML, trigger := .load }
  ]
}

-- Uncomment to see compile-time failure:
-- theorem invalid1_wellformed : invalidGraph_missingTarget.checkWellFormed = true := by native_decide
-- Error: native_decide failed because the proposition is false

-- ============================================================================
-- Example 3: Invalid graph - targeting stable region
-- ============================================================================

def invalidGraph_stableTarget : PageGraph := {
  regions := [
    { id := "root", kind := .volatile },
    { id := "form", kind := .stable }  -- Stable region!
  ]
  interactions := [
    { id := "i1", sourceRegion := "root", method := .get,
      route := "/form", targetRegion := "form",  -- Targeting stable region!
      swap := .innerHTML, trigger := .load }
  ]
}

-- Uncomment to see compile-time failure:
-- theorem invalid2_wellformed : invalidGraph_stableTarget.checkWellFormed = true := by native_decide

-- ============================================================================
-- Example 4: Using the verified helper
-- ============================================================================

-- This will fail at compile time if the graph is invalid
#check validGraph.verified  -- Works!

-- ============================================================================
-- Example 5: Building a graph with the monad and inspecting it
-- ============================================================================

open Scribe (Attr)
open Scribe.PageGraph

-- A simple route type for testing
inductive TestRoute where
  | home
  | addCard

instance : Scribe.HasPath TestRoute where
  path
    | .home => "/"
    | .addCard => "/card"

def examplePage : GraphHtmlM .stable .toplevel Unit := do
  let board ← volatileRegion "board" [] do
    GraphHtmlM.text "Board content"

  let attrs ← hxPost TestRoute.addCard (.region board) .beforeend
  element "button" attrs do
    GraphHtmlM.text "Add Card"

-- Extract the graph
def examplePageGraph : PageGraph := (GraphHtmlM.build examplePage).2

-- COMPILE-TIME VERIFICATION of a monad-built graph!
-- This theorem only compiles if the page's interaction graph is valid.
theorem examplePage_wellformed : examplePageGraph.checkWellFormed = true := by native_decide

-- Debug: see what the graph looks like
#eval do
  let pg := examplePageGraph
  IO.println s!"=== PageGraph Debug ==="
  IO.println s!"Regions ({pg.regions.length}):"
  for r in pg.regions do
    IO.println s!"  - {r.id} ({repr r.kind})"
  IO.println s!"Interactions ({pg.interactions.length}):"
  for i in pg.interactions do
    IO.println s!"  - {i.id}: {i.sourceRegion} --> {i.targetRegion}"
  IO.println s!"checkWellFormed: {pg.checkWellFormed}"
