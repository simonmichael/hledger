# hledger ROADMAP

Current ideas about where the hledger project should be going next.

## Next Targets

- hledger 1.19, 2020-09-01\
<s>account transactions register</s>,
stricter/more correct handling of unbalanced multicommodity transactions (#1177),
Track & show deposited lots (#1022),
Report unrealized capital gains/losses (#1029)


### Past Targets

- <s>hledger 1.18</s>, 2020-06-01\
<s>more effective CI setup</s>,
<s>updated home page, quickstart, tutorials etc.</s>,
negative matching in CSV rules,

- <s>hledger 1.17, 2020-03-01</s>\
<s>field matching in CSV rules</s>,
reduce install hassles with terminfo C lib (?),
more import/export options,
simple console charts,
refreshed home page, faq, tutorials, manuals,

- <s>hledger 1.16, 2019-12-01</s>\
<s>ghc 8.8 support</s>,
<s>more powerful CSV conversion</s>,
updated home page, faq, manuals,
reduce install hassles with terminfo C lib

----

## 2020 Priorities

<table>
<tr valign="top">
<td>

### Documentation

Improve the docs.
<br>
<br>

- <s>home</s> & faq
- <s>manuals (more discoverable structure of web manuals)</s>
- cookbook docs (survey, plan, update)
- contrib guide (update, build from readmes)

</td>
<td>

### Effectiveness

Improve getting-started experience, just-works quality, practicality, real-world usefulness.

- <s>ghc 8.8, get back in stackage nightly</s>
- install issues (C libs..)
- <s>more powerful CSV conversion</s>
- fill out holes in feature matrix

</td>
</tr>
<tr valign="top">
<td>

### Investment

Improve suitability for investment tracking
([#1015](https://github.com/simonmichael/hledger/issues/1015))

- <s>market price inference from transactions</s>
- easy market price fetching
- lot tracking
- capital gains reporting

</td>
<td>

### Charts

Add charts and more visual appeal.
<br>
<br>

- console charts, basic bar charts
- clarify architecture/UI for charts
- graphical charts using Chart/matplotlib/hvega
- review/design/add more attractive/colourful output (see eg taskwarrior)

</td>
</tr>
<tr valign="top">
<td>

### Correctness

<!-- added 2020-06: -->
More support for enforcing correctness & accounting rules.

- account names
- commodity symbols
- payees
- notes/descriptions ?
- account lifetimes ?
- account balance conditions ?
- transaction templates ?

</td>
</tr>
</table>

----

## Achievements

Some project achievements to unlock.
This is for fun.
("being a software maintainer means unending toil in mostly-obscurity, so you've got to seek out the fun where you can!" --[me](https://www.reddit.com/r/haskell/comments/eddwbu/top_nonprogrammingrelated_haskell_apps/))

- <s>**1.0 release**</s>
- <s>**packaged in major distros, binaries available**</s>
- <s>**pretty good reference docs**</s>
- <s>100 committers</s>
- <s>1k github stars</s>
- <s>among top 50 starred haskell projects on github</s>
- <s>discussed on Hacker News</s>
- <s>match ledger IRC channel size</s>
- <s>**mentioned in "what good Haskell software exists" discussions**</s>
- <s>among top 40 starred haskell projects on github</s>
- <s>multiple people providing support</s>
- **pretty good tutorial docs**
- **pretty good cookbook docs**
- 100 IRC chatters
- 2k github stars
- in top 30 github starred haskell projects
- **match ledger speed**
- match ledger committers
- 200 committers
- [best in class for investment tracking](https://github.com/simonmichael/hledger/issues/1015)
- 2.0 release
- in top 20 github starred haskell projects
- match ledger stars
- match beancount mail list
- match ledger mail list
- in top 10 github starred haskell projects
