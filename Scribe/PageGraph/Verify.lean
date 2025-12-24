/-
  Scribe.PageGraph.Verify - Formal verification of PageGraph properties

  Defines properties that a well-formed page graph should satisfy,
  with Decidable instances enabling compile-time verification via `decide`.

  Usage:
  ```lean
  def myPage : GraphHtmlM .stable .toplevel Unit := do
    let board ← volatileRegion "board" [] do ...
    ...

  def myPageGraph : PageGraph := (GraphHtmlM.build myPage).2

  -- Compile-time verification!
  theorem myPage_wellformed : myPageGraph.checkWellFormed = true := by native_decide
  ```
-/
import Scribe.PageGraph.Types

namespace Scribe.PageGraph

namespace PageGraph

-- ============================================================================
-- Boolean checks (computable, for native_decide)
-- ============================================================================

/-- Check if all interaction targets reference existing regions -/
def checkAllTargetsExist (pg : PageGraph) : Bool :=
  pg.interactions.all fun i =>
    pg.regions.any fun r => r.id == i.targetRegion

/-- Check if all interaction sources reference existing regions -/
def checkAllSourcesExist (pg : PageGraph) : Bool :=
  pg.interactions.all fun i =>
    pg.regions.any fun r => r.id == i.sourceRegion

/-- Check if all interaction targets are volatile regions -/
def checkTargetsAreVolatile (pg : PageGraph) : Bool :=
  pg.interactions.all fun i =>
    match pg.regions.find? (·.id == i.targetRegion) with
    | some r => r.kind == .volatile
    | none => true  -- Missing target caught by checkAllTargetsExist

/-- Check that no interaction targets its own source region -/
def checkNoSelfTargeting (pg : PageGraph) : Bool :=
  pg.interactions.all fun i => i.sourceRegion != i.targetRegion

/-- Check that every region (except root) is reachable via some interaction -/
def checkNoOrphanedRegions (pg : PageGraph) (root : String) : Bool :=
  pg.regions.all fun r =>
    r.id == root || pg.interactions.any fun i => i.targetRegion == r.id

/-- Check all well-formedness properties -/
def checkWellFormed (pg : PageGraph) : Bool :=
  pg.checkAllTargetsExist &&
  pg.checkAllSourcesExist &&
  pg.checkTargetsAreVolatile

/-- Check strict well-formedness including no self-targeting and no orphans -/
def checkStrictlyWellFormed (pg : PageGraph) (root : String) : Bool :=
  pg.checkWellFormed &&
  pg.checkNoSelfTargeting &&
  pg.checkNoOrphanedRegions root

-- ============================================================================
-- Logical Properties (Prop) - For stating theorems
-- ============================================================================

/-- All interaction targets reference existing regions -/
def allTargetsExist (pg : PageGraph) : Prop :=
  ∀ i ∈ pg.interactions, ∃ r ∈ pg.regions, r.id = i.targetRegion

/-- All interaction sources reference existing regions -/
def allSourcesExist (pg : PageGraph) : Prop :=
  ∀ i ∈ pg.interactions, ∃ r ∈ pg.regions, r.id = i.sourceRegion

/-- All interaction targets are volatile regions -/
def targetsAreVolatile (pg : PageGraph) : Prop :=
  ∀ i ∈ pg.interactions,
    ∀ r ∈ pg.regions, r.id = i.targetRegion → r.kind = .volatile

/-- No interaction targets its own source region -/
def noSelfTargeting (pg : PageGraph) : Prop :=
  ∀ i ∈ pg.interactions, i.sourceRegion ≠ i.targetRegion

/-- Every region (except root) is reachable via some interaction -/
def noOrphanedRegions (pg : PageGraph) (root : String) : Prop :=
  ∀ r ∈ pg.regions,
    r.id = root ∨ ∃ i ∈ pg.interactions, i.targetRegion = r.id

/-- Well-formedness as a Prop -/
structure WellFormed (pg : PageGraph) : Prop where
  targetsExist : pg.allTargetsExist
  sourcesExist : pg.allSourcesExist
  targetsVolatile : pg.targetsAreVolatile

-- ============================================================================
-- Reflection lemmas: connect Bool checks to Prop properties
-- ============================================================================

theorem checkAllTargetsExist_iff (pg : PageGraph) :
    pg.checkAllTargetsExist = true ↔ pg.allTargetsExist := by
  simp only [checkAllTargetsExist, allTargetsExist, List.all_eq_true,
             List.any_eq_true, beq_iff_eq]

theorem checkAllSourcesExist_iff (pg : PageGraph) :
    pg.checkAllSourcesExist = true ↔ pg.allSourcesExist := by
  simp only [checkAllSourcesExist, allSourcesExist, List.all_eq_true,
             List.any_eq_true, beq_iff_eq]

theorem checkNoSelfTargeting_iff (pg : PageGraph) :
    pg.checkNoSelfTargeting = true ↔ pg.noSelfTargeting := by
  simp only [checkNoSelfTargeting, noSelfTargeting, List.all_eq_true, bne_iff_ne]

-- ============================================================================
-- Compile-time verification helpers
-- ============================================================================

/-- Verify a page graph at compile time. Returns the proof if valid. -/
def verified (pg : PageGraph) (h : pg.checkWellFormed = true := by native_decide) :
    pg.checkWellFormed = true := h

/-- Verify strict well-formedness at compile time. -/
def verifiedStrict (pg : PageGraph) (root : String)
    (h : pg.checkStrictlyWellFormed root = true := by native_decide) :
    pg.checkStrictlyWellFormed root = true := h

end PageGraph

end Scribe.PageGraph
