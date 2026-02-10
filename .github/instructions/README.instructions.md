---
description: README best practices for this repo: fast onboarding, skimmable structure, concrete commands, honest support/stability, and low-rot documentation.
applyTo: "README.md"
---

# README writing rules

## Primary goal
Optimize for **time-to-first-success** and **trust**:
- A new user should succeed within ~5 minutes.
- Claims must be measurable (commands, checks, policies).
- Keep README skimmable; move depth to `docs/` and Haddock.

## Structure (preferred order)
1. **Title + 1–2 sentence pitch** (what it is, who it’s for, what it solves)
2. **Status / support** (stability level, supported OS, GHC/Stack policy)
3. **Quickstart** (copy/paste commands + short expected output)
4. **What it does** (3–7 bullets) + **Non-goals** (optional, 1–3 bullets)
5. **Installation** (prereqs + platform notes + system deps)
6. **Usage** (minimal realistic example + a common next step link)
7. **API / Documentation** (how to build Haddock + links to `docs/`)
8. **Development** (build/test/docs/lint/format commands)
9. **Testing policy** (Hspec + QuickCheck; how to run)
10. **Troubleshooting** (top 5–10 common issues with fixes)
11. **Contributing** (PR expectations; how to run CI-equivalent checks)
12. **License** (and security/contact info if relevant)

## Quickstart rules
- The first code block should be the happy path.
- Include exact Stack commands (e.g., `stack build`, `stack test`, `stack exec ...`).
- Include a brief “expected output” snippet when useful.

## Installation rules
- Be explicit about prerequisites (Stack version policy, GHC policy via resolver).
- List required system deps and how to install them on Linux/macOS/Windows.
- Avoid machine-specific config in README (no hardcoded local paths).

## Usage rules
- Prefer a minimal *realistic* example over a toy example.
- Keep examples short and copy/paste friendly.
- If there’s configuration, include a minimal config block + defaults behavior.

## Documentation rules
- README is for onboarding and high-level concepts.
- Put long explanations in `docs/` and reference them.
- Mention how to build Haddock: `stack build --haddock --no-haddock-deps`.

## Development + quality rules
- Provide exact commands to run local checks (tests, docs, lint/format if present).
- Any claim like “property tested”, “safe”, “total”, etc. must be backed by:
  - where the tests live, and
  - how to run them.

## Troubleshooting rules
- Include common failure modes and direct fixes:
  - missing system libraries
  - GHC/resolver mismatch
  - Windows toolchain/linker issues
- Keep each item brief: **Problem** → **Fix**.

## Style rules
- Keep it skimmable: short paragraphs, bullets, clear headings.
- Use badges sparingly (CI, license, release/Hackage if relevant).
- Be honest about stability and API maturity; note breaking-change/migration policy.
- Avoid duplicating detailed docs in multiple places to prevent doc rot.
