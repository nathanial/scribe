/-
  Scribe.Region - Phantom types for DOM region and level safety

  This module provides compile-time safety for HTMX/SSE applications:

  Region (volatility):
  - Stable regions: contain ephemeral user state (forms, modals)
  - Volatile regions: can be refreshed by SSE/HTMX
  Forms placed in volatile regions become compile errors.

  Level (document structure):
  - Toplevel: layout/document level (scripts, meta tags allowed)
  - Nested: component level (no scripts allowed)
  Scripts placed at nested level become compile errors.
-/
import Scribe.Html
import Scribe.Attr

namespace Scribe

/-- Phantom type for DOM region volatility -/
inductive Region where
  | stable    -- Contains ephemeral user state (forms, modals, user input)
  | volatile  -- Can be refreshed by SSE/HTMX without warning
  deriving DecidableEq, Repr

/-- Phantom type for document structure level -/
inductive Level where
  | toplevel  -- Document/layout level: scripts, meta tags, styles allowed
  | nested    -- Component level: no scripts allowed (security)
  deriving DecidableEq, Repr

end Scribe
