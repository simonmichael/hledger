# Roadmap / Wishlist

Ideas about where the hledger project should be going next, as of 2019q4. 
Discussion welcome.

## Targets

Dated targets we are aiming for:

- **hledger 1.16** - 2019-12-01
  - ghc 8.8 support
  - updated home page, faq, manuals
  - basic bar charts
  - reduce install hassles with terminfo C lib

----

## Priorities

Current priorities for the project, grouped under four overarching themes:

<table>
<tr valign="top">
<td>

### Documentation

Improving the docs.
<br>
<br>

- home & faq
- manuals (more discoverability on web)
- contrib guide (include readmes, update)
- cookbook docs (survey, plan, update)

</td>
<td>

### "Effectiveness"

Improving approachability, just-works quality, practicality, real-world usefulness.

- ghc 8.8, stackage nightly
- install issues (C libs..)
- more powerful CSV conversion
- fill out holes in feature matrix

</td>
</tr>
<tr valign="top">
<td>

### Charts

Adding charts and more visual appeal.
<br>
<br>

- console charts, basic bar charts
- clarify architecture/UI for charts
- graphical charts using Chart and/or Vega
- review/design/add more attractive default output (see taskwarrior)

</td>
<td>

### Investment

Improving suitability for investment tracking.
[#1015](https://github.com/simonmichael/hledger/issues/1015)

- easy price fetching
- lots
- capital gains
- ?

</td>
</tr>
</table>

----

## Wishlist

A few high-level maintainer wishes, by topic. 
Discussing/moving any of these forward is a big help.

### Docs

- build the contributor/dev guide from CONTRIBUTING.md plus the READMEs; clean up
- move general PTA docs to plaintextaccounting.org
- simplify manuals' TOC/structure on the website

### Tests

- migrate to tasty, unblock ghc 8.8 support
- survey/document current testing setup (process, coverage, redundancy..); identify weaknesses; design a policy for tests; make a plan for achieving that
- start adding property tests/other new kinds of testing if appropriate

----

## Milestones

Some project achievements to unlock, for fun:

- <s>**1.0 release**</s>
- <s>**packaged in major distros, binaries available**</s>
- <s>**pretty good reference docs**</s>
- <s>100 committers</s>
- <s>1k github stars</s>
- <s>among top 50 haskell projects by github stars</s>
- <s>discussed on Hacker News</s>
- match ledger IRC channel
- multiple people providing support
- **pretty good tutorial docs**
- **mentioned in "what good Haskell software exists" discussions**
- **pretty good cookbook docs**
- 100 IRC chatters
- 2k github stars
- among top 40 haskell projects by github stars
- **match ledger speed**
- match ledger committers
- 200 committers
- 2.0 release
- match ledger stars
- match beancount mail list
- match ledger mail list
