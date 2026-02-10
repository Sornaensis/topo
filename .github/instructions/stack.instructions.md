# .github/instructions/stack.instructions.md
---
description: Best practices for maintaining stack.yaml (reproducible, portable, CI-friendly).
applyTo: "stack.yaml"
---

- Treat stack.yaml as a *reproducible build contract*: changes must keep builds deterministic across machines and CI.
- Always pin a Stackage snapshot via `resolver:` (prefer LTS unless there is a compelling reason).
- Keep `extra-deps` minimal. When needed, pin exact versions (and exact git SHAs if using git deps).
- Do **not** add machine-specific paths (e.g., `extra-include-dirs`, `extra-lib-dirs`) to stack.yaml. Prefer:
  - documenting system deps in README, or
  - using CI install steps / Nix / system package managers, or
  - per-developer configuration in the user’s Stack config (not committed).
