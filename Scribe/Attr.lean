/-
  Scribe.Attr - Common HTML attribute helpers
-/
import Scribe.Html

namespace Scribe

-- Global attributes
def class_ (v : String) : Attr := ⟨"class", v⟩
def id_ (v : String) : Attr := ⟨"id", v⟩
def style_ (v : String) : Attr := ⟨"style", v⟩
def title_ (v : String) : Attr := ⟨"title", v⟩
def lang_ (v : String) : Attr := ⟨"lang", v⟩
def dir_ (v : String) : Attr := ⟨"dir", v⟩
def hidden_ : Attr := ⟨"hidden", ""⟩
def tabindex_ (v : Int) : Attr := ⟨"tabindex", toString v⟩

-- Data attributes
def data_ (name : String) (v : String) : Attr := ⟨s!"data-{name}", v⟩

-- Link/navigation attributes
def href_ (v : String) : Attr := ⟨"href", v⟩
def target_ (v : String) : Attr := ⟨"target", v⟩
def rel_ (v : String) : Attr := ⟨"rel", v⟩
def download_ (v : String := "") : Attr := ⟨"download", v⟩

-- Image/media attributes
def src_ (v : String) : Attr := ⟨"src", v⟩
def alt_ (v : String) : Attr := ⟨"alt", v⟩
def width_ (v : Nat) : Attr := ⟨"width", toString v⟩
def height_ (v : Nat) : Attr := ⟨"height", toString v⟩
def loading_ (v : String) : Attr := ⟨"loading", v⟩

-- Form attributes
def action_ (v : String) : Attr := ⟨"action", v⟩
def method_ (v : String) : Attr := ⟨"method", v⟩
def enctype_ (v : String) : Attr := ⟨"enctype", v⟩
def name_ (v : String) : Attr := ⟨"name", v⟩
def value_ (v : String) : Attr := ⟨"value", v⟩
def type_ (v : String) : Attr := ⟨"type", v⟩
def placeholder_ (v : String) : Attr := ⟨"placeholder", v⟩
def required_ : Attr := ⟨"required", ""⟩
def disabled_ : Attr := ⟨"disabled", ""⟩
def readonly_ : Attr := ⟨"readonly", ""⟩
def checked_ : Attr := ⟨"checked", ""⟩
def selected_ : Attr := ⟨"selected", ""⟩
def multiple_ : Attr := ⟨"multiple", ""⟩
def autofocus_ : Attr := ⟨"autofocus", ""⟩
def autocomplete_ (v : String) : Attr := ⟨"autocomplete", v⟩
def min_ (v : String) : Attr := ⟨"min", v⟩
def max_ (v : String) : Attr := ⟨"max", v⟩
def step_ (v : String) : Attr := ⟨"step", v⟩
def pattern_ (v : String) : Attr := ⟨"pattern", v⟩
def maxlength_ (v : Nat) : Attr := ⟨"maxlength", toString v⟩
def minlength_ (v : Nat) : Attr := ⟨"minlength", toString v⟩
def for_ (v : String) : Attr := ⟨"for", v⟩
def rows_ (v : Nat) : Attr := ⟨"rows", toString v⟩
def cols_ (v : Nat) : Attr := ⟨"cols", toString v⟩

-- Table attributes
def colspan_ (v : Nat) : Attr := ⟨"colspan", toString v⟩
def rowspan_ (v : Nat) : Attr := ⟨"rowspan", toString v⟩
def scope_ (v : String) : Attr := ⟨"scope", v⟩

-- Meta/head attributes
def charset_ (v : String) : Attr := ⟨"charset", v⟩
def content_ (v : String) : Attr := ⟨"content", v⟩
def httpEquiv_ (v : String) : Attr := ⟨"http-equiv", v⟩

-- Script/style attributes
def async_ : Attr := ⟨"async", ""⟩
def defer_ : Attr := ⟨"defer", ""⟩
def integrity_ (v : String) : Attr := ⟨"integrity", v⟩
def crossorigin_ (v : String) : Attr := ⟨"crossorigin", v⟩

-- ARIA attributes
def ariaLabel_ (v : String) : Attr := ⟨"aria-label", v⟩
def ariaHidden_ (v : Bool) : Attr := ⟨"aria-hidden", if v then "true" else "false"⟩
def ariaDescribedby_ (v : String) : Attr := ⟨"aria-describedby", v⟩
def ariaLabelledby_ (v : String) : Attr := ⟨"aria-labelledby", v⟩
def role_ (v : String) : Attr := ⟨"role", v⟩

-- Event handler placeholders (values would be JS code)
def onclick_ (v : String) : Attr := ⟨"onclick", v⟩
def onsubmit_ (v : String) : Attr := ⟨"onsubmit", v⟩
def onchange_ (v : String) : Attr := ⟨"onchange", v⟩
def oninput_ (v : String) : Attr := ⟨"oninput", v⟩

-- HTMX attributes
-- Core request attributes
def hx_get (v : String) : Attr := ⟨"hx-get", v⟩
def hx_post (v : String) : Attr := ⟨"hx-post", v⟩
def hx_put (v : String) : Attr := ⟨"hx-put", v⟩
def hx_patch (v : String) : Attr := ⟨"hx-patch", v⟩
def hx_delete (v : String) : Attr := ⟨"hx-delete", v⟩

-- Targeting and swapping
def hx_target (v : String) : Attr := ⟨"hx-target", v⟩
def hx_swap (v : String) : Attr := ⟨"hx-swap", v⟩
def hx_select (v : String) : Attr := ⟨"hx-select", v⟩
def hx_select_oob (v : String) : Attr := ⟨"hx-select-oob", v⟩
def hx_swap_oob (v : String) : Attr := ⟨"hx-swap-oob", v⟩

-- Triggering
def hx_trigger (v : String) : Attr := ⟨"hx-trigger", v⟩
def hx_confirm (v : String) : Attr := ⟨"hx-confirm", v⟩

-- Request modifiers
def hx_vals (v : String) : Attr := ⟨"hx-vals", v⟩
def hx_headers (v : String) : Attr := ⟨"hx-headers", v⟩
def hx_include (v : String) : Attr := ⟨"hx-include", v⟩
def hx_params (v : String) : Attr := ⟨"hx-params", v⟩
def hx_encoding (v : String) : Attr := ⟨"hx-encoding", v⟩

-- UI feedback
def hx_indicator (v : String) : Attr := ⟨"hx-indicator", v⟩
def hx_disabled_elt (v : String) : Attr := ⟨"hx-disabled-elt", v⟩

-- History and URL
def hx_push_url (v : String) : Attr := ⟨"hx-push-url", v⟩
def hx_replace_url (v : String) : Attr := ⟨"hx-replace-url", v⟩
def hx_history_elt : Attr := ⟨"hx-history-elt", ""⟩

-- Other
def hx_boost (v : String := "true") : Attr := ⟨"hx-boost", v⟩
def hx_ext (v : String) : Attr := ⟨"hx-ext", v⟩
def hx_preserve : Attr := ⟨"hx-preserve", ""⟩
def hx_sync (v : String) : Attr := ⟨"hx-sync", v⟩

-- Generic attribute constructor
def attr_ (name : String) (value : String) : Attr := ⟨name, value⟩

-- ============================================================================
-- Type-safe HTMX targeting
-- ============================================================================

/-- Wrapper type documenting intent to target a volatile region.
    Use this with hx_target_vol to indicate the target is safe to refresh. -/
structure VolatileTarget where
  id : String
  deriving Repr

/-- Create a volatile target reference.
    Documents that the referenced ID should be a volatile region. -/
def volatileTarget (id : String) : VolatileTarget := ⟨id⟩

/-- HTMX target that explicitly documents it references a volatile region.
    Prefer this over hx_target for type-safe HTMX development. -/
def hx_target_vol (target : VolatileTarget) : Attr :=
  ⟨"hx-target", "#" ++ target.id⟩

end Scribe
