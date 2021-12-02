# Changelogs

<div class=pagetoc>
<!-- toc -->
</div>

## How to prepare changelogs & release notes

Changelogs:

- always maintain changelogs in master branch, not in release branches
- ./Shake changelogs
- edit the new changelog items (identify, filter, move to correct changelog, deduplicate, rewrite, sort/group)

Release notes:

- add a new section in site/src/release-notes.md
- copy/rewrite/summarise package changelogs 
- note any other items of interest
- list release contributors
- write release summary

## Frequently

especially after merging changes, and before cherry picking into release branch:

- dry run: `./Shake changelogs -n`
- add new changes: `./Shake changelogs`
- edit
  - drop things
  - move things
  - Add headings: Features, Improved, Fixes
  - rewrite things
  - format ([#ISSUE](https://github.com/simonmichael/hledger/issues/), AUTHOR) on its own line
- commit: `./Shake changelogs -c`

## After cherry-picking

changes to a release branch:

- in the master branch changelogs, move the corresponding changelog items under a pending release heading,
  creating that when necessary:
    ```
    # LATESTHASH

    ...CHANGES ONLY IN MASTER...

    # NEXTVER unreleased

    ...CHANGES CHERRYPICKED INTO RELEASE BRANCH...

    # LASTVER YYYY-MM-DD
    ```

## At release

- do final update/edits; check organisation, wording, formatting, issue links
- replace "unreleased" with the release date
- copy the new sections from master changelogs to release branch changelogs

## Old notes

Changelogs are plain text, but started including some markdown formatting
from 1.0. Should make consistent.

Changelogs started mentioning committer names from 2017/1,
for hledger-ui-1.1.1 (because they won't appear on the release notes).
Could do it just for minor releases but might as well do it for all.
Could do it for past releases but no pressing need.

In site/release-notes.md, we stopped mentioning minor releases
around 0.27. The old minor releases should probably be removed
or promoted to the same heading level as major releases.


