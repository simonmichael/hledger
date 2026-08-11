# Decisions

A partial list of notable development decisions / design choices..

## 2022

### Replace "transaction price" terminology with "cost"

"Transaction price" never quite stuck. "Cost" is simpler, shorter, more intuitive, consistent with `--cost` and "cost reporting", and more distinct from "market price".

There is an (acceptable) ambiguity: "cost" could mean the `@ UNITCOST` price attached to the amount, or the total cost when the amount is converted (`QUANTITY * UNITCOST`).

Status: as of 2023Q1 this has been done in the manuals and is slowly ongoing in the code.

## 2023

### Plugin types

We will document and support where feasible several distinct kinds of plugin, written in haskell or other languages,
such as reader, processor, writer, formatter, command. See <https://hledger.org/scripting.html#plugin-types>.

## 2025

I think the keyword-first style for directives is right for us (`open 2025-01-01 ...`, not `2025-01-01 open ...`).
It avoids polluting/breaking transaction descriptions, it's similar to P, 
it keeps directives and transactions visually distinct,
and consistently beginning with letters and numbers respectively.

Yes we should support declaring aliases with alias: tags on account directives.

## 2026

### Release hledger 2.0 this year, with two main themes: lots and AI-assisted development

hledger 2 will explore ethical AI-assisted development, 
and will leverage that to ship automated lots and gains tracking.
There will be a substantial period for preview releases, discussion and testing before the 2.0 release.

### Shift "cost" terminology to "transacted cost" or "transacted price"

To distinguish transacted costs (@) from cost basis ({}).

### Compute realised gain from the disposal postings only

The synthetic `rgain`/`ugain` pair is sized from `Σ aquantity × (B − T)`
over non-acquire postings with both basis and transacted cost — not from
the entry's full cost-basis residual. This isolates real capital gain
from acquire-side bookkeeping mistakes (eg a typo'd `{B}` or a fee being
double-counted into basis).

### Don't enforce basis = transacted cost in acquisitions by default

Acquires with `{B} @ T` where `B ≠ T`, are accepted by default, for better compatibility
with other apps (hledger 1, Ledger, Beancount, rustledger, acc, etc.). 
Docs recommend users to always keep `B = T`, and to use the `basis` check to check this,
with reasons provided (prevent wrong gain caused by basis typos).
The new check might be moved into strict mode some day, but not yet.

### hledger-web is read only by default on a public address

When listening on a non-local address, hledger-web now defaults to read-only;
allowing writes requires saying so explicitly.
The safe default matters more than the convenience of the permissive one.

### Journal-adjacent data directories

CSV `source` and `archive` rules, and `import`, now work relative to the journal's data directory
rather than the rules file's directory, establishing a convention of journal-adjacent
`data/`, `data/archive/`, `rules/` and `prices/` directories.
Rationale: a journal plus its inputs, rules and fetched data should be one relocatable unit.

### Config files can no longer specify which command to run

Config files can no longer provide the first argument to specify which command to run -
that was confusing and made the CLI's argument parsing hard to reason about.
For similar reasons, `--conf`/`--no-conf` written inside a config file are also ignored.

### Consolidate documentation under the `help` command

`help` becomes the single entry point for hledger's docs:
it shows an overview by default,
the `commands` command is replaced by `help commands`,
--help is available via `help usage`,
it can open key website pages, etc.

### Detach from the tldr-pages project

Letting the tldr-pages project control part of our docs, and keeping in sync,
limited our content quite a lot (commands only, specific formatting rules)
and added lots of overhead. We'll leave the existing hledger docs in the tldr-pages repo,
for others to maintain. It's not essential that they be there, as having docs built in
to the program itself is more useful and efficient. But we'll keep using the format
for inspiration.
`--tldr` is renamed `--examples` and `help examples`.
Our local copy of the tldr pages, in `doc/tldr/`, will be moved under `examples/`,
which will become more integrated with `help examples`.

### Drop the `demo` command

The demos were too few, too costly to create and update,
and won't provide enough benefit over docs and examples.

### Syntax and parsing relaxations

### A single tab is accepted as the "two space delimiter"

For Ledger compatibility.

### Apostrophe and underscore are accepted as digit group marks.

For Switzerland and for programmers.

### Inferred amounts no longer affect a commodity's display precision.

Amounts inferred to balance an entry no longer influence global display precisions,
or the entry's local balancing precision. Only explicitly-written amounts
(and those inferred from balance assignments) do.
This changes behaviour for the better, avoiding unexpected/unwanted increases in display precision.

### Autodetect the base currency

hledger guesses a journal's base currency, eg for fetching prices, and shows it in `stats`.

### Policies for AI usage

These are tracked in AI.md. Eg,

- Significant AI usage must be disclosed
- AI assistance is not allowed in PRs from first-time contributors.
- AI assistance using OpenAI tools is not allowed.
- Significant AI usage must be tracked, approximately, one way or another.
  Eg logged in commit messages, or when that's not appropriate, logged in `doc/ai/ai.journal`.

### RULES.md

Repo policies in general, including AI policies, are gathered in doc/RULES.md.

### Discontinue the regression bounties

They are now a magnet for AI slop.
