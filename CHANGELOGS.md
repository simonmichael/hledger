# Changelogs

<div class=pagetoc>

<!-- toc -->
</div>

## Update master changelogs often
Before release, and preferably daily/weekly as well:
- always first update changelogs in master branch, not in release branches
- `./Shake changelogs`
- edit the new changelog items
    - open changelogs
      - as VSC panes: 
        - explorer
        - unfold hledger, hledger-lib, hledger-ui, hledger-web directories (?)
        - CMD-f, changesmd, click filter icon
        - click project CHANGES (last one in list)
        - click editor pane, split it, click rightmost editor pane
        - click hledger CHANGES
        - click rightmost editor pane, split it, ... etc.
    - in each changelog (first ui & web; then project, cli, lib)
      - process new items (starting above top-most section heading, working upward)
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


