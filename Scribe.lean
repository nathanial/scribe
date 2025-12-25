/-
  Scribe - Monadic HTML Builder for Lean 4

  A type-safe, composable HTML builder using a monad for fluent construction
  with phantom-typed regions for HTMX/SSE safety.

  Regions:
  - .stable: Contains ephemeral user state (forms, modals). Never refreshed.
  - .volatile: Can be refreshed by SSE/HTMX.

  Form elements (form, input, textarea, select) only compile in stable regions.

  Example:
  ```lean
  import Scribe
  open Scribe

  def page : HtmlM .stable Unit := do
    html do
      body do
        volatileRegion "content" [class_ "container"] do
          h1 (text "Welcome")
          p (text "This is Scribe.")
        stableRegion "modal" [] do
          form [] do
            input [type_ "text", name_ "email"]
  ```
-/

import Scribe.Html
import Scribe.Attr
import Scribe.Region
import Scribe.Builder
import Scribe.Elements
import Scribe.RouteAttrs
