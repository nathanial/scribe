/-
  Scribe - Monadic HTML Builder for Lean 4

  A type-safe, composable HTML builder using a monad for fluent construction.

  Example:
  ```lean
  import Scribe
  open Scribe

  def page : Html := HtmlM.build do
    html do
      head do
        title "Hello"
      body do
        div [class_ "container"] do
          h1 (text "Welcome")
          p (text "This is Scribe.")
  ```
-/

import Scribe.Html
import Scribe.Attr
import Scribe.Builder
import Scribe.Elements
