# COMMITS

## When committing / reviewing commits

Follow/encourage the [commit conventions](CONTRIBUTING.html#commit-messages).
Here they are in brief:
-  Commit messages must begin with a prefix, one or more colon-terminated words
   indicating the [topic](CONTRIBUTING.html#topics).
-  Commits causing user-visible changes must additionally begin with `feat:`, `imp:` or `fix:` 
   (feature, improvement, or bugfix). These will be mentioned in release notes.
-  Add a leading `;` to commits where a CI build is not needed, to reduce waste.
-  Add a `!` to highlight commits causing breaking/incompatible changes.
-  Mention any relevant issue numbers, usually parenthesised at the end. `(#NNNN)`
-  Try to write commit messages as changelog-ready documentation that will tell their
   intended audience (which might be users, installers, packagers, and/or developers) 
   what they need to know.

## When committing/pushing/merging:
- run `bin/commitlint` before push, to check recent commits
- or, run it automatically before each commit (`make installcommithook` to configure your local repo)
- it also runs in CI on github for pull requests, etc.

