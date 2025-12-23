import Lake
open Lake DSL

package scribe where
  version := v!"0.1.0"

require crucible from ".." / "crucible"

@[default_target]
lean_lib Scribe where
  roots := #[`Scribe]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe scribe_tests where
  root := `Tests.Main
