/-
  Scribe.PageGraph.Interaction - Bundled interaction builder

  Creates HTMX interactions as a single unit, returning the appropriate
  attributes while also registering the interaction in the PageGraph.
-/
import Scribe.Html
import Scribe.RouteAttrs
import Scribe.PageGraph.Builder

namespace Scribe.PageGraph

open Scribe (Attr HasPath)

/-- Specification for an HTMX interaction.
    Bundles method, route, target, swap mode, and trigger into one unit. -/
structure InteractionSpec where
  method : HttpMethod
  route : String
  target : Target
  swap : SwapMode := .innerHTML
  trigger : TriggerEvent := .click
  deriving Repr, Inhabited

namespace InteractionSpec

/-- Create a GET interaction -/
def get [HasPath R] (route : R) (target : Target)
    (swap : SwapMode := .innerHTML) (trigger : TriggerEvent := .click) : InteractionSpec :=
  { method := .get, route := HasPath.path route, target, swap, trigger }

/-- Create a POST interaction -/
def post [HasPath R] (route : R) (target : Target)
    (swap : SwapMode := .innerHTML) (trigger : TriggerEvent := .submit) : InteractionSpec :=
  { method := .post, route := HasPath.path route, target, swap, trigger }

/-- Create a PUT interaction -/
def put [HasPath R] (route : R) (target : Target)
    (swap : SwapMode := .innerHTML) (trigger : TriggerEvent := .submit) : InteractionSpec :=
  { method := .put, route := HasPath.path route, target, swap, trigger }

/-- Create a PATCH interaction -/
def patch [HasPath R] (route : R) (target : Target)
    (swap : SwapMode := .innerHTML) (trigger : TriggerEvent := .submit) : InteractionSpec :=
  { method := .patch, route := HasPath.path route, target, swap, trigger }

/-- Create a DELETE interaction -/
def delete [HasPath R] (route : R) (target : Target)
    (swap : SwapMode := .innerHTML) (trigger : TriggerEvent := .click) : InteractionSpec :=
  { method := .delete, route := HasPath.path route, target, swap, trigger }

end InteractionSpec

/-- Build an interaction: generates HTMX attributes and registers in the graph.

    Usage:
    ```lean
    let board ← volatileRegion "board" [] do ...
    button [← interaction (.post Route.addCard (.region board) .beforeend)]
      (text "Add Card")
    ```
-/
def interaction (spec : InteractionSpec) : GraphHtmlM r l (List Attr) := do
  -- Get source region from context
  let sourceRegion := (← GraphHtmlM.getCurrentRegion).getD "root"

  -- Determine target region ID for graph
  let targetRegionId := match spec.target with
    | .region ref => ref.id
    | .self => sourceRegion  -- Self-targeting means same region
    | .closest _ => sourceRegion  -- Can't statically determine
    | .find _ => sourceRegion     -- Can't statically determine
    | .selector _ => sourceRegion -- Can't statically determine

  -- Generate unique ID and register interaction
  let interactionId ← GraphHtmlM.freshInteractionId
  GraphHtmlM.registerInteraction {
    id := interactionId
    sourceRegion
    method := spec.method
    route := spec.route
    targetRegion := targetRegionId
    swap := spec.swap
    trigger := spec.trigger
  }

  -- Build and return HTMX attributes
  let methodAttr : Attr := { name := spec.method.toAttrName, value := spec.route }
  let targetAttr : Attr := { name := "hx-target", value := spec.target.toAttrValue }
  let swapAttr : Attr := { name := "hx-swap", value := spec.swap.toAttrValue }
  let triggerAttr : Attr := { name := "hx-trigger", value := spec.trigger.toAttrValue }

  -- Only include non-default attributes
  let attrs := [methodAttr, targetAttr]
  let attrs := if spec.swap != .innerHTML then attrs ++ [swapAttr] else attrs
  let attrs := if spec.trigger != .click then attrs ++ [triggerAttr] else attrs

  pure attrs

/-- Shorthand: Create a GET interaction with the given route and target -/
def hxGet [HasPath R] (route : R) (target : Target)
    (swap : SwapMode := .innerHTML) : GraphHtmlM r l (List Attr) :=
  interaction (InteractionSpec.get route target swap)

/-- Shorthand: Create a POST interaction with the given route and target -/
def hxPost [HasPath R] (route : R) (target : Target)
    (swap : SwapMode := .innerHTML) : GraphHtmlM r l (List Attr) :=
  interaction (InteractionSpec.post route target swap)

/-- Shorthand: Create a PUT interaction -/
def hxPut [HasPath R] (route : R) (target : Target)
    (swap : SwapMode := .innerHTML) : GraphHtmlM r l (List Attr) :=
  interaction (InteractionSpec.put route target swap)

/-- Shorthand: Create a PATCH interaction -/
def hxPatch [HasPath R] (route : R) (target : Target)
    (swap : SwapMode := .innerHTML) : GraphHtmlM r l (List Attr) :=
  interaction (InteractionSpec.patch route target swap)

/-- Shorthand: Create a DELETE interaction -/
def hxDelete [HasPath R] (route : R) (target : Target)
    (swap : SwapMode := .innerHTML) : GraphHtmlM r l (List Attr) :=
  interaction (InteractionSpec.delete route target swap)

end Scribe.PageGraph
