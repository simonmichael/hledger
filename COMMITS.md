# COMMITS

<div class=pagetoc>
<!-- toc -->
</div>

## When committing / reviewing commits

Follow/encourage the commit conventions (see below).
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

## When committing / pushing / merging:

- run `tools/commitlintnt` before push, to check recent commits
- or, run it automatically before each commit (`make installcommithook` to configure your local repo)
- it also runs in CI on github for pull requests, etc.

## Commit conventions

Since the 1.23 release cycle, we try to follow certain conventions for commit messages, to

- encourage considered, focussed, well documented changes
- reduce the cost of code review, maintaining changelogs and release notes, and releasing
- increase our throughput (rate of shipping useful, reliable, documented, maintainable features)

**hledger commit conventions:**

1. Commit messages in hledger's main repo follow this pattern:
   ```
   type: [optionaltopic:] summary
   
   [Optional description, more details here when needed.]
   ```

2. Every top-level commit must have a type prefix, ending with a colon and optional space. 
   This indicates the change's intended audience and the general type of change. 
   Here are the current types:

   - **Changes visible to end users** (including users of hledger-web's HTTP API).
     These will appear in release notes and changelogs:
 
     - `feat` - a new feature
     - `imp`  - an improvement to existing features
     - `fix`  - a bugfix
 
   - **Changes affecting packagers, builders, and library users**. 
    These will appear in changelogs:
 
     - `cha` - a generic package/lib change. Or, one of these specific types:
     - `pkg` - something to do with the haskell packages, dependencies etc.
     - `lib` - a change in the package's library API
     - ...some other type that seems useful...

   - **Changes interesting only to hledger developers/documentors/debuggers**.
     These will usually appear only in the commit history, not in changelogs or release notes:
 
     - `dev` - a generic developer change. Or, one of these specific types:
     - `ref` - refactoring
     - `cln` - cleanup
     - `doc` - documentation-related
     - `test` - tests-related
     - `ci`  - continuous integration-related
     - ...some other type that seems useful...
 
    There's a bit of ambiguity/overlap between the cha/dev types and topics.
    Eg the `doc` type indicates a boring doc change, but there's also a `doc` topic
    which might be used for interesting doc changes, as in `feat:doc:...`. TBD.

3. If this is a "breaking change", introducing a compatibility or
   migration issue, the type is followed by `!`, and the issue
   and advice to users are included in the description.
   This will most often be seen with the end-user types, eg:
   `feat!:`, `imp!:`, `fix!:`.

4. If the first character of the commit message is `;`, this commit
   (more precisely, the push ending with this commit) will be excluded
   from the usual CI checks. Our CI tends to do a lot of building, so 
   you can use this to save energy and carbon emissions when pushing 
   harmless changes.

5. A topic prefix, and maybe even a subtopic prefix, can be added
   before the summary if useful. These are standard prefixes similar
   to what I have been using for some time, see [topics](#topics). 
   They help with readability in the commit history, changelogs and release notes.

6. Any relevant issues should be mentioned, often parenthesised at 
   the end of the summary: `(#NNNN)`.

7. The summary, and description if any, communicate this change's
   purpose as clearly as possible to its intended audience: 
   end users, builders/packagers/library users, or developers/debuggers.
   The text (or its first sentence/first paragraphs) should be ready 
   for use in changelogs/release notes when applicable.

Crafting good commit messages (and thereby good commits, good change
documentation, easier code review, faster merging) is an art and a
habit. Just use your best judgement and we'll check and polish
as part of CI and code review. Examples will be added here in due course.

Related:

- <https://groups.google.com/g/hledger/c/t14xwHQMJqU/m/9frARXIdAAAJ>
- <https://conventionalcommits.org>
- <https://git.savannah.gnu.org/cgit/emacs.git/plain/CONTRIBUTE> -> Commit messages

