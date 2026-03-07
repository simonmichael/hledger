# Fast recompilation feedback

## Problem

The full rebuild cycle (`stack build hledger`) is slow when changes touch core modules like `Types.hs` — about 11 seconds due to cascading recompilation of all dependents. Leaf module changes are fast (~1.6s) but core changes are frequent during development.

## Solution: `ghc -fno-code` type-checking

Type-check without code generation, ~2s regardless of what changed:

```
stack exec -- ghc -fno-code -fforce-recomp \
  -Wall -Wno-incomplete-uni-patterns -Wno-missing-signatures -Wno-orphans -Wno-type-defaults -Wno-unused-do-bind \
  -DDEVELOPMENT -DVERSION="\"1.51.99\"" \
  -ihledger-lib -ihledger \
  TARGET_MODULE.hs
```

Replace `TARGET_MODULE.hs` with the module you changed (e.g. `hledger-lib/Hledger/Data/Lots.hs`).

## Benchmarks (M-series Mac, 2026-02)

| Scenario | `stack build hledger` | `ghc -fno-code` |
|---|---|---|
| After touching `Types.hs` | ~11s | ~2s |
| After touching `Lots.hs` (leaf) | ~1.6s | ~2s |

`-fno-code` is ~5x faster for core module changes. For leaf-only changes `stack build` is comparable since it's already incremental.

## Alternatives considered

**ghcid** (`just ghcid`): Keeps a GHCi session loaded and does incremental `:reload` on file changes. Sub-second for leaf changes after initial load, but slow startup and requires managing a persistent background process. Not worth the complexity when `-fno-code` is already fast.

**GHCi manual reload** (`just ghci` then `:reload`): Same incremental benefit as ghcid but requires an interactive session. Good for human developers; awkward for automated tooling.

**HLS**: Provides continuous type-checking via LSP but requires an LSP client.

## Recommended workflow

1. After code changes: run `ghc -fno-code` targeting the changed module (~2s)
2. Before functional tests: run `stack build hledger` (need the actual binary)
3. Before committing: run `just functest --hide` for full regression check
