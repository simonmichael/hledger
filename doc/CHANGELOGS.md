# Changelogs

<div class=pagetoc>

<!-- toc -->
</div>

See also RELEASING.md.

## Update master changelogs often
Before release, and preferably daily/weekly as well:
- during normal development, do this only in master branch
- during release preparation.. unclear; keep the other branch's changelog in mind, they need to be synced at some point. Prioritise \[minor] release branch's changelog ?
- `./Shake changelogs`
- edit the new changelog items
    - open the five changelogs
      - in emacs: `C-x d */CHANGES.md`, `(`, split into 5 columns, open each one, narrow from last release heading to top
    - in each changelog
      - paste section headings from top comment if needed
      - process new items, from top-most section heading upward
        - drop boring items (changes not visible to end/API users, followups to other new items, minor doc updates..)
        - drop duplicated items/content (mostly between cli and lib)
        - move to top of appropriate changelog & section (create section when needed; consider a second pane to reduce scrolling)
        - drop prefixes, edit texts, check/adjust links
        - (in cli/web/ui, move any "API changes" parts to lib ?)
    - proof read/clean up all changelogs (Obsidian works well)
  - `./Shake changelogs -c`

## Update release & master changelogs at release time
Before release, after cherry-picking changes from master to the release branch:
- check out master in a separate working copy
- for each changelog
  - open master & release changelog in side-by-side windows
  - copy all appropriate new content from master to release
  - in release changelog, remove any previous prerelease heading, add a new release heading
  - in release ui/web changelogs, add/update "uses hledger X.Y" item if needed
  - if this is a major release, or a minor release with notable project updates, copy to master changelogs:
    - the ui/web "uses hledger X.Y"
    - the new release heading
- commit changelog updates in both working copies
- pull the master changelog updates back to main working copy
- destroy temporary working copy, emacs buffers

## Update release notes
- add a new section in site/src/release-notes.md
- copy/rewrite/summarise package changelogs 
- note any other items of interest
- list release contributors
- write release summary

## Old notes

Changelogs started using markdown from 1.0 onward. Should make consistent.

Changelogs started mentioning committer names from 2017/1,
for hledger-ui-1.1.1 (because they won't appear on the release notes).
Could do it just for minor releases but might as well do it for all.
Could do it for past releases but no pressing need.

In site/release-notes.md, we stopped mentioning minor releases
around 0.27. The old minor releases should probably be removed
or promoted to the same heading level as major releases.


