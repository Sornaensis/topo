---
description: "Modern Haskell (2026) coding standards: totality, exhaustive handling, type-driven design, clean modularization, and strong tests."
applyTo: "**/*.hs"
---

- Write modern, idiomatic Haskell (2026 best practices) with a strong preference for totality and correctness.
- Total-by-default: avoid partial functions and bottom (`undefined`, `error`, non-exhaustive patterns, `head`, `tail`, `fromJust`, etc.). If absolutely unavoidable, document the invariant and enforce it at the boundary.
- Exhaustiveness: all pattern matches must be exhaustive; handle all constructors; avoid partial record updates and incomplete matches.
- Make invalid states unrepresentable: encode invariants in types (ADTs, `newtype`, smart constructors, phantom types/GADTs when appropriate). Prefer domain-specific error types.
- Prefer explicit, structured failure (`Either`, `Maybe`, validated domain types) over exceptions.
- Keep modules small and cohesive. When a module approaches ~300–500 LOC, split into logical submodules with clear responsibilities and minimal exports.
- Public API quality:
  - Explicit export lists for public modules.
  - Haddock docs for every exported symbol and meaningful module headers.
  - Every public-facing API change must include tests: Hspec behavior tests + QuickCheck properties for invariants/edge cases.
- Favor clarity over cleverness: small composable functions, readable types, and predictable effects.
- Use modern language features responsibly (deriving strategies, `DerivingVia` when helpful, `newtype` over `type`, avoid orphan instances).

- **_ALWAYS_** Fix all warnings you see in repo files during the build, when you have finished your current task.
- **_ALWAYS_** Make a note of refactorings and fixes that you spot during your work on a task, in .etc/refactorings.md. Make it as a checklist if it is mutliple steps, and make child-items checklist items as well.
- **_ALWAYS_** Make a note of missing documentation in public APIs when you notice it and make a note in .etc/documentation-fixes.md.