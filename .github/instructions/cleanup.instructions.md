---
description: "Always include this when asked to do tidying, or cleanup"
applyTo: "**/*.hs"
---

Cleanup:

- Run `hlint`, using stack to install it if it is not on the $PATH
- Implement all suggested fixes
- Repeat until `hlint` no longer has any complaints about the codebase
- **_IF_** hlint suggests any multi-step or structural changes across modules, INSTEAD of implementing it, make a distinct note in .etc/refactorings.md, then stop and ask for help.