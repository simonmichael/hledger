# COMMITS

<div class=pagetoc>

<!-- toc -->
</div>

In the hledger project we try to follow certain conventions for commit messages,
because good messages lead to good commits => good change docs => 
easier code review => quicker merging => faster delivery of quality software.
We'll check and help you polish messages as part of CI and code review.
(You can also set up a local commit hook, described below.)

Here's the typical format: <!-- keep synced with tools/commitlint -->

    [feat|imp|fix[!]:] topic: Summary

    [Longer description when useful]

More precisely:

- Commit messages must begin with one or more prefixes (colon-terminated words),
  indicating the type and/or [topic](ISSUES.md#topics).
- Commits causing user-visible changes must begin with `feat:`, `imp:` or `fix:` 
  (feature, improvement, or bugfix). These will be used in release notes.
  If they are breaking/incompatible changes, use `feat!:`, `imp!:` or `fix!:`.
- To skip CI builds on commits which would normally trigger one, add a `;` at the beginning.
  (Our CI does a lot of work, so you can use this to reduce energy waste 
   and carbon emissions from minor changes. Non-code commits do this automatically.)
- Mention any relevant issue numbers, usually parenthesised at the end. `(#NNNN)`
- Try to write commit messages as changelog/release-note-ready documentation that will tell their
  intended audience (which might be users, installers, packagers, and/or developers) 
  what they need to know.

Some examples:

- `feat: accounts: --types shows account types (#1820)`
- `imp!: journal: Remove deprecated account type code syntax from account directives.`
- `fix: types: Ensure auto postings can match against and be matched by type: queries.`
- `tools: commitlint: allow a git "fixup! " prefix`
- `doc: releasing: tweaks`

Some possible prefixes:

- `feat` - a new feature
- `imp`  - an improvement to existing features
- `fix`  - a bugfix
- `dev` - a generic developer change
- `ref` - refactoring
- `cln` - cleanup
- `doc` - documentation-related
- `test` - tests-related
- `ci`  - continuous integration-related
- Any of the standard [labels](ISSUES.md#labels) used in the issue tracker.

## How to check commits

Before committing, pushing, or merging, run `tools/commitlint` to check recent commit messages.
(See the script for more ways to select commits.) You can configure your local working copy
to do this automatically, by running `just installcommithook`.

commitlint also runs automatically on Github to check pull requests.

## See also

- <https://groups.google.com/g/hledger/c/t14xwHQMJqU/m/9frARXIdAAAJ>
- <https://conventionalcommits.org>
- <https://git.savannah.gnu.org/cgit/emacs.git/plain/CONTRIBUTE> -> Commit messages

