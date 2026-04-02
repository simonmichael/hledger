# Changelogs

This skill describes how to update hledger changelogs, which are in */CHANGES.md.
There is one for each hledger package and one for the overall project (in doc/).

## Drafting changelogs

`just changelogs` adds draft changelog entries to the changelogs.
For each package in the source tree, it lists git log messages since the commit hash saved in the topmost level 1 heading, 
and inserts them below that heading (and then replaces that heading with the latest commit hash).

## Polishing changelogs

Here is how to polish a draft changelog. By default, you should 
- work on one package at a time, in this order: hledger-web, hledger-ui, hledger, hledger-lib, project.
- and one phase at a time, usually doing phase 1 for all of the packages first
- and ask for review of each changed item or group of items.

### Polish phase 1: cleanup
- focus on the new release changelog, which is all the items between the first two level 1 markdown headings. These are the draft entries to be polished. No changes should be made elsewhere.
- each changelog has section headings in a html comment at the top (under the large figlet-generated title). These headings should be inserted at the bottom of the new release changelog, if they aren't already there.
- items towards the bottom of the new release changelog may have already been polished. These should be kept mostly as they are. For the rest,
- remove the semicolon prefix from change items.
- remove routine/boring/non-user-visible change items. But first, show them as a grouped and numbered list and ask for confirmation. Also save this list as a temp file for later review.
  Some examples of boring items: "update changelogs", "update cabal files", "update embedded manuals".
- items prefixed by ; are sometimes boring, but not always, so don't rely on that.
- remove duplicated/previously announced items. List these, showing where they were previously announced.
- items that mention "cli:" are user-visible CLI changes shared by all hledger tools (hledger, hledger-ui, hledger-web), and should be kept in each package's changelog.
- changes which are visible only to API users should be kept, grouped in the "API" section.
- group the "doc:" changes in the "Docs" section. Doc changes can be listed compactly with no blank line between items, and sorted.
- group other items in the most appropriate section. Eg "feat:" in Features, "fix:" in Fixes, "imp:" and bounds changes in Improvements, etc.
- If the prefix is followed by a !, the item should go in Breaking Changes.
- the prefix(es) can be removed once items have been moved into their section
- follow the layout of the (recent) previous releases' changelogs, below.
- The hledger-lib changelog is likely to have some end-user-visible items; these should be moved to the appropriate tool changelog (usually hledger/CHANGES.md).
  Only API-user-visible changes should remain in the hledger-lib changelog.
- In the project changelog:
  Tools/process/infrastructure/justfile items should be summarised compactly in "Infrastructure/Misc".
  Project-level doc items (these often have upper-case filenames) should be summarised compactly in "Doc updates".
  Examples and scripts/addons/"bin:" items from the project changelog should be moved to the hledger changelog's "Examples" and "Scripts/addons" sections respectively.

### Polish phase 2: edits
- improve the spelling (british preferred), capitalisation, grammar, flow, and clarity of each item.
  They don't have to be perfect, but follow the style and tone of the older changelogs below.
- simple clear english is preferred.
- sometimes a commit message is too brief, unclear, or not in the usual style.
  When necessary we can find out more by inspecting the corresponding commit(s), looking for issue number or text matches.
- author(s) and issue numbers usually appear on their own line at the end of each item.
- we prefer real author names if available. Sometimes we can convert a commit author's nickname to a real name by looking up their github user page.

### Polish phase 3: links
- each issue number should be enclosed in square brackets
- and at the end of the draft entries, markdown urls should be inserted for each issue, for hyperlinking.
  These look like:
  [#NNNN]: https://github.com/simonmichael/hledger/issues/NNNN

## Finalising changelogs

On release day, when changelogs are polished and reviewed, we use `just changelogs-finalise` to replace the headings with the release version and date, and commit.

