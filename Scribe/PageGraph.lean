/-
  Scribe.PageGraph - Reified HTMX Interaction Model

  This module provides a way to build HTML pages while simultaneously
  constructing an interaction graph that models the HTMX behavior.
  The graph can then be analyzed and verified.

  ## Usage

  ```lean
  import Scribe.PageGraph

  open Scribe.PageGraph

  -- Define routes with HasPath instance
  inductive Route where | home | addCard
  instance : Scribe.HasPath Route where
    path | .home => "/" | .addCard => "/card"

  -- Build page with graph tracking
  def myPage : GraphHtmlM .stable .toplevel Unit := do
    let board ← volatileRegion "board" [] do
      GraphHtmlM.text "Board content"

    -- Interaction bundles route + target + swap
    let attrs ← hxPost Route.addCard (.region board) .beforeend
    element "button" attrs do
      GraphHtmlM.text "Add Card"

  -- Run and validate
  def main : IO Unit := do
    let (html, graph) := GraphHtmlM.run myPage
    match graph.validate with
    | .ok () => IO.println "Graph is valid!"
    | .error msg => IO.println s!"Error: {msg}"
    IO.println graph.toMermaid
  ```

  ## Key Types

  - `PageGraph` - The reified interaction graph (regions + interactions)
  - `GraphHtmlM` - HTML builder monad that tracks the graph
  - `RegionRef` - Reference to a region for type-safe targeting
  - `Target` - Specification for HTMX targeting
  - `InteractionSpec` - Bundled interaction specification

  ## Key Functions

  - `volatileRegion` / `stableRegion` - Create regions, returning refs
  - `interaction` / `hxGet` / `hxPost` - Create interactions
  - `PageGraph.validate` - Runtime validation
  - `PageGraph.toMermaid` / `toDot` - Visualization
-/

import Scribe.PageGraph.Types
import Scribe.PageGraph.RegionRef
import Scribe.PageGraph.Builder
import Scribe.PageGraph.Regions
import Scribe.PageGraph.Interaction
import Scribe.PageGraph.Verify
import Scribe.PageGraph.Analysis
