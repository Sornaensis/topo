---
description: Use this when performing refactorings!
applyTo: "**/*.hs"
---

Refactoring Process:

The paramount thing is to understand logical grouping of source files into _submodules_ instead of just making very _wide_ modules. When you have a single module with more than one domain of a few files each, create subdomains as new Domain/ Domain2/ modules to hold their own submodules. Keep in mind haskell module import restrictions when doing this. But this is the _MAIN_ idea behind refactorings.

- **_IMPORTANT_** Always begin additively, create the new module layout and files and expose them through package.yaml
- Once the new module is setup, begin splitting up all functionality into the new source file structure
- **_IMPORTANT_** Wire up tests _first_ to the new modules to ensure Public API adherence, build only. In some cases you _may_ have to rewire dependent modules to get it to compile
- **_MOST IMPORTANT_** Once everything builds, refactor any remaining references to the old modules to the new, remove all references to the old modules from package.yaml, and replace the source file contents with a single comment below the module declaration saying they may be deleted