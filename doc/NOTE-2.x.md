# hledger 2.x

## Strategy

Summary of discussion [Thoughts on hledger 2 #2547](https://github.com/simonmichael/hledger/issues/2547):

### Positions

**Simon's motivations**: drop costly cruft, marketing power of "2.0", lot tracking as flagship feature, AI-era demarcation, decouple from the 3-month release cycle.

**New users (rickles42, Daniii44)**: Beancount's v2->v3 transition was a cautionary tale -- intermingled docs, broken companion tools, unclear what works with which version. Don't repeat that.

**adept (collaborator)**: don't break compatibility without a clear goal that requires it. History is full of needless rewrites that killed projects.

### Recommended: "Boring 2.0"

Stay on master, release 2.0 when ready. Don't maintain parallel long-lived branches.

1. **Linear release path**: 1.52 -> 1.99.1 (preview) -> 1.99.2 -> ... -> 2.0. Avoids the cost of maintaining two diverging trunks.

2. **Minimise breaking changes; make them opt-in first**: new behaviours behind flags (like `--lots`), old behaviours deprecated with warnings for a release or two, then removed. Proven pattern (Rust editions, Python `__future__`, GHC extensions).

3. **Lot tracking alone justifies 2.0**: it's a large, data-model-impacting feature. Combined with accumulated improvements, it's enough. Bundling too many breaking cleanups risks the Beancount trap.

4. **Keep docs unified**: a single docset with "New in 2.0" / "Changed in 2.0" callouts, not two divergent doc trees.

5. **AI demarcation is worth noting but shouldn't drive versioning**: it's a process change, not user-facing. Mention in release notes, not a reason to break compatibility.

### Key Principle

Take the marketing win (call it 2.0) but keep the technical disruption minimal.

### Current most likely plan:

- Keep using one master branch.
- On next release day (march 1st), release both 1.52 (minor updates) and 1.99.1 (2.0 preview 1, with lot tracking).
- Don't intentionally break anything, except in the usual way (as rarely as possible, with easy workarounds and deprecation periods).

## Goals

SM 2026-02:\
Some goals for 2.x:

- continue and improve 1.x's reliability
- excellent lots/capital gains tracking
- more interoperability/convertibility
- more speed
- more customisation paths

and:

- more use of AI as a dev tool; clarify policies
- more use of jj to simplify version management
- more aggressive cleanup and simplification of code/doc/process/finance..
- easier contribution

and for 1.x:

- continued installability/usability
- preserve the stable/known hledger 1.x feature set
- preserve the non-AI-assisted codebase; draw a line between pre and post-AI eras

