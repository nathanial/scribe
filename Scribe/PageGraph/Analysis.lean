/-
  Scribe.PageGraph.Analysis - Graph analysis and visualization

  Functions for querying the interaction graph, computing reachability,
  and generating visual representations.
-/
import Scribe.PageGraph.Types

namespace Scribe.PageGraph

namespace PageGraph

-- ============================================================================
-- Edge Queries
-- ============================================================================

/-- Find all interactions that target a specific region (incoming edges) -/
def incomingEdges (pg : PageGraph) (regionId : String) : List Interaction :=
  pg.interactions.filter (·.targetRegion == regionId)

/-- Find all interactions originating from a specific region (outgoing edges) -/
def outgoingEdges (pg : PageGraph) (regionId : String) : List Interaction :=
  pg.interactions.filter (·.sourceRegion == regionId)

/-- Find all regions that can update a specific region -/
def sourceRegions (pg : PageGraph) (targetId : String) : List String :=
  pg.incomingEdges targetId |>.map (·.sourceRegion) |>.eraseDups

/-- Find all regions that a specific region can update -/
def targetRegions (pg : PageGraph) (sourceId : String) : List String :=
  pg.outgoingEdges sourceId |>.map (·.targetRegion) |>.eraseDups

-- ============================================================================
-- Reachability Analysis
-- ============================================================================

/-- Compute regions reachable from a starting region (BFS) -/
partial def reachableFrom (pg : PageGraph) (start : String) : List String :=
  let rec go (visited : List String) (frontier : List String) : List String :=
    match frontier with
    | [] => visited
    | current :: rest =>
      if visited.contains current then
        go visited rest
      else
        let newTargets := pg.targetRegions current
          |>.filter (!visited.contains ·)
          |>.filter (!frontier.contains ·)
        go (current :: visited) (rest ++ newTargets)
  go [] [start]

/-- Check if a region is reachable from another -/
def isReachable (pg : PageGraph) (from_ to : String) : Bool :=
  pg.reachableFrom from_ |>.contains to

/-- Find regions with no incoming edges (entry points) -/
def entryPoints (pg : PageGraph) : List Region :=
  pg.regions.filter fun r =>
    pg.incomingEdges r.id |>.isEmpty

/-- Find regions with no outgoing edges (dead ends) -/
def deadEnds (pg : PageGraph) : List Region :=
  pg.regions.filter fun r =>
    pg.outgoingEdges r.id |>.isEmpty

/-- Find isolated regions (no incoming or outgoing edges) -/
def isolatedRegions (pg : PageGraph) : List Region :=
  pg.regions.filter fun r =>
    (pg.incomingEdges r.id).isEmpty && (pg.outgoingEdges r.id).isEmpty

-- ============================================================================
-- Statistics
-- ============================================================================

/-- Graph statistics -/
structure Stats where
  regionCount : Nat
  interactionCount : Nat
  volatileRegions : Nat
  stableRegions : Nat
  entryPoints : Nat
  deadEnds : Nat
  deriving Repr

/-- Compute graph statistics -/
def stats (pg : PageGraph) : Stats := {
  regionCount := pg.regions.length
  interactionCount := pg.interactions.length
  volatileRegions := pg.regions.filter (·.kind == .volatile) |>.length
  stableRegions := pg.regions.filter (·.kind == .stable) |>.length
  entryPoints := pg.entryPoints.length
  deadEnds := pg.deadEnds.length
}

-- ============================================================================
-- Visualization
-- ============================================================================

/-- Generate a Mermaid diagram from the graph -/
def toMermaid (pg : PageGraph) : String :=
  let header := "graph TD"

  -- Generate node definitions with styling classes
  let nodes := pg.regions.map fun r =>
    let shape := if r.kind == .volatile then s!"[{r.id}]" else s!"({r.id})"
    let class_ := if r.kind == .volatile then ":::volatile" else ":::stable"
    s!"    {sanitizeId r.id}{shape}{class_}"

  -- Generate edge definitions
  let edges := pg.interactions.map fun i =>
    let label := s!"{i.method.toAttrName.drop 3} {i.route}"  -- drop "hx-" prefix
    s!"    {sanitizeId i.sourceRegion} -->|{label}| {sanitizeId i.targetRegion}"

  -- Class definitions for styling
  let styles := [
    "    classDef volatile fill:#e1f5fe,stroke:#01579b",
    "    classDef stable fill:#fff3e0,stroke:#e65100"
  ]

  String.intercalate "\n" ([header] ++ nodes ++ edges ++ styles)
where
  -- Sanitize IDs for Mermaid (replace special chars)
  sanitizeId (id : String) : String :=
    id.map fun c => if c.isAlphanum then c else '_'

/-- Generate a simple ASCII representation of the graph -/
def toAscii (pg : PageGraph) : String :=
  let regionLines := pg.regions.map fun r =>
    let kind := if r.kind == .volatile then "volatile" else "stable"
    s!"  [{r.id}] ({kind})"

  let interactionLines := pg.interactions.map fun i =>
    s!"  {i.sourceRegion} --({i.method.toAttrName} {i.route})--> {i.targetRegion}"

  let header := s!"PageGraph ({pg.regions.length} regions, {pg.interactions.length} interactions)"
  let regionsSection := "Regions:" :: regionLines
  let interactionsSection := "Interactions:" :: interactionLines

  String.intercalate "\n" ([header, ""] ++ regionsSection ++ [""] ++ interactionsSection)

/-- Generate a DOT (Graphviz) representation -/
def toDot (pg : PageGraph) : String :=
  let header := "digraph PageGraph {"
  let graphSettings := [
    "    rankdir=LR;",
    "    node [shape=box];"
  ]

  let nodes := pg.regions.map fun r =>
    let style := if r.kind == .volatile
      then "style=filled,fillcolor=lightblue"
      else "style=filled,fillcolor=lightyellow"
    s!"    \"{r.id}\" [{style}];"

  let edges := pg.interactions.map fun i =>
    let label := s!"{i.method.toAttrName.drop 3}\\n{i.route}"
    s!"    \"{i.sourceRegion}\" -> \"{i.targetRegion}\" [label=\"{label}\"];"

  let footer := "}"

  String.intercalate "\n" ([header] ++ graphSettings ++ nodes ++ edges ++ [footer])

end PageGraph

end Scribe.PageGraph
