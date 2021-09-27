# Issues

<div class=pagetoc>
<!-- toc -->
</div>

The hledger project\'s issue tracker is on github. It contains:

-   BUG issues - failures in some part of the hledger project (the main
    hledger packages, docs, website..)
-   WISH issues - feature proposals, enhancement requests
-   uncategorised issues - we don\'t know what these are yet
-   pull requests - proposed changes to code and docs

Issues are also labelled according to their [topics](#topics), for organisation.

Some loose conventions:

- In bug titles, mention the hledger version in which the bug first appeared 
  (and avoid mentioning version numbers otherwise).
  This allows searches like
  [new issues in 1.22](https://github.com/simonmichael/hledger/issues?q=in%3Atitle+1.22+)
  and
  [regressions in 1.22](https://github.com/simonmichael/hledger/issues?q=in%3Atitle+1.22+label%3Aregression%21)


## Issue Urls

-   <http://bugs.hledger.org> - show open BUG issues
-   <http://wishes.hledger.org> - show open WISH issues
-   <http://issues.hledger.org> - show all issues, open or closed
-   <http://prs.hledger.org> - show open pull requests
-   <http://bugs.hledger.org/new> - create a new issue

## Labels

Labels are used to categorise:

-   the issue\'s type: \"A BUG\" or \"A WISH\", in shades of red (The A
    makes it appear as first label)
-   relevant subsystems/topics, in light blue. More about this below.
-   relevant platforms, in light purple
-   resolution if not fixed:
    \"closed:cant-reproduce/duplicate/invalid/wont-fix\", in dark grey
-   \"bounty\", in bright yellow: issues with bountysource funding
-   \"easy?\", in dim yellow: issues which are probably relatively easy
    to fix
-   \"imported\" etc., in white: miscellaneous information

## Topics

Short topic names, corresponding to hledger commands, input formats, output formats and other common themes,
are used to organise things in the hledger project. In particular,

- They are used as space saving descriptive prefixes for [commit messages](#commit-messages)
- They can be used as prefixes for issue/PR titles
- Issues and PRs are labelled with them (the light blue labels).

A more or less complete list can be seen at [open issues](#open-issues)
or in the issue tracker's labels list.

## Custodians

If you are interested in helping with a particular component for a
while, please add yourself as a custodian in Open Issues table above.
A custodian\'s job is to help manage the issues, rally the troops, and
drive the open issue count towards zero. The more custodians, the
better! By dividing up the work this way, we can scale and make forward
progress.

## Milestones and Projects

Milestones are used a little bit to plan releases. In 2017 we
experimented with projects, but in 2018 milestones are in favour again..

## Estimates

You might see some experiments in estimate tracking, where some issue
names might have a suffix noting estimated and spent time. Basic format:
\[ESTIMATEDTOTALTASKTIME\|TIMESPENTSOFAR\]. Examples: \`\`\` \[2\] two
hours estimated, no time spent \[..\] half an hour estimated (a dot is
\~a quarter hour, as in timedot format) \[1d\] one day estimated (a day
is \~4 hours) \[1w\] one week estimated (a week is \~5 days or \~20
hours) \[3\|2\] three hours estimated, about two hours spent so far
\[1\|1w\|2d\] first estimate one hour, second estimate one week, about
two days spent so far \`\`\` Estimates are always for the total time
cost (not time remaining). Estimates are not usually changed, a new
estimate is added instead. Numbers are very approximate, but better than
nothing.

## Trello

The [trello board](http://trello.hledger.org) (trello.hledger.org) is an
old collection of wishlist items. This should probably be considered
deprecated.

