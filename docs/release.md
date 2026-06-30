# Topo 1.0 release packaging

This guide defines the reproducible 1.0 release artifact set and the tag used to
rebuild it from a clean checkout.

## Version matrix

| Package | Cabal version | Release role |
|---|---:|---|
| `topo` | `1.0.0.0` | Core terrain, world, overlay, simulation, persistence, and format library. |
| `topo-seer` | `1.0.0.0` | SDL UI executable and direct HTTP/OpenAPI host. |
| `topo-plugin-sdk` | `1.0.0.0` | External plugin SDK, runner, and fixture executable. |
| `topo-plugin-example` | `1.0.0.0` | Minimal generator plugin example executable. |
| `topo-plugin-civ-example` | `1.0.0.0` | Civilization/data-resource plugin example executable. |

The public HTTP API is versioned separately as OpenAPI `info.version=1.0.0` and
`api_version=1`. The `/version` route reports the `topo-seer` package version.

## Release tag

Use one annotated Git tag for the workspace release:

```sh
git tag -a v1.0.0 -m "Topo 1.0.0"
git push origin v1.0.0
```

Tag `v1.0.0` must point at the commit that contains the package version matrix,
`CHANGELOG.md`, `docs/release-notes/1.0.md`, this release guide, and the
committed OpenAPI artifact. Do not publish package-specific tags for the 1.0
workspace unless a later split-package release process explicitly replaces this
single workspace tag.

## Reproducible clean-checkout build

Run these commands from a fresh clone of the release tag. Keep artifacts under
`.tmp/release/`; the directory is ignored so the checkout stays clean.

```sh
git clone --branch v1.0.0 --depth 1 <repo-url> topo-1.0.0
cd topo-1.0.0

# Confirm the source checkout is clean before producing artifacts.
git status --short

# Build and test the workspace with the pinned resolver and lock file.
stack build --test --work-dir .tmp/stack-work-release

# Generate coverage and enforce the 1.0 release thresholds used by CI.
stack --work-dir .tmp/stack-work-release test --coverage 2>&1 | tee .tmp/release/coverage-test.log
stack --work-dir .tmp/stack-work-release hpc report --all 2>&1 | tee .tmp/release/coverage-report.log
python3 tools/check-coverage-thresholds.py .tmp/release/coverage-report.log

# Build source distributions and validate each tarball by compiling it.
stack sdist --work-dir .tmp/stack-work-release --test-tarball --tar-dir .tmp/release/sdist

# Build distributable executables into a staging directory.
stack build \
  topo-seer:exe:topo-seer \
  topo-plugin-sdk:exe:topo-plugin-fixture \
  topo-plugin-example:exe:topo-plugin-example \
  topo-plugin-civ-example:exe:topo-plugin-civ-example \
  --work-dir .tmp/stack-work-release \
  --copy-bins --local-bin-path .tmp/release/bin
```

Expected source tarballs:

- `.tmp/release/sdist/topo-1.0.0.0.tar.gz`
- `.tmp/release/sdist/topo-seer-1.0.0.0.tar.gz`
- `.tmp/release/sdist/topo-plugin-sdk-1.0.0.0.tar.gz`
- `.tmp/release/sdist/topo-plugin-example-1.0.0.0.tar.gz`
- `.tmp/release/sdist/topo-plugin-civ-example-1.0.0.0.tar.gz`

Expected executable staging outputs are `topo-seer`, `topo-plugin-fixture`,
`topo-plugin-example`, and `topo-plugin-civ-example` with the platform's normal
executable suffix. Native SDL2/SDL2_ttf runtime libraries are not vendored in the
Stack artifacts and must be supplied by the target system or installer.

## Release documentation artifacts

Publish these documentation artifacts with the release:

- `CHANGELOG.md`
- `docs/release-notes/1.0.md`
- `docs/api/README.md`
- `docs/api/openapi.json`
- `docs/migration/pre-1.0-to-1.0.md`
- `docs/plugin-dev/manifest.md`
- `docs/plugin-dev/rpc-protocol.md`

The `topo-seer-test` suite compares `docs/api/openapi.json` to the generated
contract during workspace test runs, so rerun it whenever route metadata,
examples, version responses, auth, or schema docs change. Package-only
`sdist --test-tarball` runs skip that repository documentation check when the
`docs/api` artifact is not present in the unpacked package.

## Artifact cleanliness check

After artifact creation, the only working-tree additions should be ignored files
under `.tmp/release/` and Stack work directories. Run `git status --short` from
the clean checkout before publishing; tracked package metadata and committed docs
must already match the release tag.
