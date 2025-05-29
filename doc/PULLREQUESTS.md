# Pull requests

<div class=pagetoc>

<!-- toc -->
</div>

Shortcut urls:

- <http://prs.hledger.org>
- <http://draftprs.hledger.org>
- <http://readyprs.hledger.org>
- <http://assignedprs.hledger.org>
- <http://unassignedprs.hledger.org>

<!--
Misc. PR queries:

- ...
-->


Most contributed hledger code (and some of the project maintainer's code)
is submitted and reviewed via Github pull requests.
Here are some tips for contributing PRs to hledger.

## Code review is important

We aim to improve and sustain hledger's quality and maintainability over the long term.

Many PRs, especially small ones, and even some big ones, can be merged quickly. 
We love merging PRs quickly.

Some bigger or more risky PRs can require substantial review, discussion, changes, or re-submission. 
Sometimes this is a bigger task than the coding.
Much valuable design, quality control, and knowledge sharing happens at this time. 

Some PRs ultimately get rejected, but their discussion and exploration can still be a useful contribution.
We very much want to avoid wasted work, but it occasionally happens. 
Our process is evolving and imperfect.
All of this is a normal part of software development.

We hope you'll see it as a golden opportunity to collaborate with experts,
share and receive knowledge, refine your design/documentation/code,
and practice real-world development and communication skills.
Patience and persistence pays.

## The pull request

A PR should have a clear purpose, documented in its description. Mention any #ISSUENOs addressed.

Don't tackle too much at once. 
Smaller/more focussed PRs can be reviewed quicker and accepted (or rejected) quicker.

Consider showing a draft of documentation first (more on this below).

When you are not ready for the PR to be merged, please make it a Draft PR.
(Non-draft means "I think this is ready, you can merge any time".)
And use a normal PR title, ie don't write "WIP:" or "Draft:" in the title.

## The commit(s)

Commits should be easy to review.
Ideally each commit is complete, and has a single clear purpose,
which should be documented in the summary (and long description, if needed).
\#ISSUENOs can be mentioned in summary/description too when appropriate.

Within the above constraint, fewer, larger commits are preferred.

Keep in mind that commit messages are valuable documentation 
for future developers and troubleshooters. 
They are also the starting point for package changelogs and hledger release notes.
High-quality commit messages makes the release process quicker, and the resulting docs better. 

User-impacting commits should mention the user-visible changes, 
and be described in user-relevant language.
Library-user-impacting commits, eg API changes, ideally will also
be called out, and can described in more technical language.
Commits affecting hledger internals are less important, 
but you may notice some adhoc conventions if you browse the history.
In particular, you can optionally prefix the summary with short component codes (cf [Issues](#issues))
to facilitate history reading and changelog/release note production.

Rewrite and force-push your commits freely (rebase -i, push -f) to clean them up. 
Unless we decide to squash the PR into one commit, 
your commits will become part of hledger's history "for all time", 
so think about future developers trying to understand them, git bisect, etc.   

Rebase your commits against latest master for easiest review. Especially if they start to conflict.

We like to use some conventions in commit messages when it makes sense. These aren't mandatory, but appreciated:

- prepend a [topic label](ISSUES.md#labels) prefix, eg `cli: ` or `journal: `, for clarity and to help with changelog production
- prepend a semicolon (`;`) to indicate commits that 
  - need not trigger a CI workflows, reducing wasteful carbon emissions
  - and probably need not be mentioned in changelogs/release notes

## The docs

PRs should include appropriate updates to reference documentation, unless otherwise agreed.
Typically this means the manual source files (hledger*/hledger*.m4.md).
It can also involve
command line option names and descriptions,
other --help output,
hledger's commands list,
hledger-ui's help dialog,
hledger-web's help dialog,
etc.
Sometimes it means the developer docs, at least the ones in the main repo (READMEs).

Reviewers can understand your PR more efficiently once proposed doc changes are provided, and may postpone it otherwise.
We are happy to help with the docs if needed - just ask.

Updating rendered manuals (hledger.{1,info,txt,md,html}) is not required, and probably best avoided to reduce conflicts.
Updating other docs such as tutorials, how-tos, examples, or screenshots is not required,
though it's welcome (may be in a different repo).

## Documentation first

hledger follows documentation-driven design.
It is in fact highly effective, and highly recommended,
to write the new docs (help text/user manual/haddocks/developer README..) before writing any code.
You can share a rough draft on IRC, on the mail list, in an issue comment,
or in a "WIP" PR starting with just the proposed docs commit.

This is often the quickest road to getting something merged into hledger.
hledger's many parts interact in surprisingly complex ways.
The documentation-driven working style lets us discuss, clarify and reach a good-enough consensus economically,
after which coding/review/acceptance can go quicker.
<!--
changes can impact past and future users,
ease of contribution,
long-term maintenance costs,
product architecture,
compatibility with the larger plain text accounting ecosystem,
etc.
-->

## Related ideas

[Neil Mitchell’s Blog - The One PR Per Day Rule](https://neilmitchell.blogspot.com/2019/06/the-one-pr-per-day-rule.html)


