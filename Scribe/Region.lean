/-
  Scribe.Region - Phantom types for DOM region safety

  This module provides compile-time safety for HTMX/SSE applications by
  distinguishing between:
  - Stable regions: contain ephemeral user state (forms, modals)
  - Volatile regions: can be refreshed by SSE/HTMX

  Forms placed in volatile regions become compile errors.
-/
import Scribe.Html
import Scribe.Attr

namespace Scribe

/-- Phantom type for DOM region volatility -/
inductive Region where
  | stable    -- Contains ephemeral user state (forms, modals, user input)
  | volatile  -- Can be refreshed by SSE/HTMX without warning
  deriving DecidableEq, Repr

end Scribe
