---
description: "*.cabal policy: do not edit. Suggest changes elsewhere and keep Cabal files stable."
applyTo: "**/*.cabal"
---

- **Never edit *.cabal files.** Do not modify, reformat, or “fix” them—no dependency edits, module list edits, flags, or metadata changes.
- If a change would normally require a .cabal edit:
  - Prefer updating the *source-of-truth* file if present (e.g., `package.yaml` / hpack configuration) and let regeneration produce the .cabal changes.
  - Otherwise, describe the required .cabal change in your explanation and implement everything you can without touching the file (code changes, tests, docs). Leave the .cabal edit to a human.
- If a build failure appears to require .cabal edits, explicitly call that out and propose the minimal patch content *without applying it*.
