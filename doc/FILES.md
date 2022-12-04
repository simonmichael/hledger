# Files

<div class=pagetoc>

<!-- toc -->
</div>

Overviews of files in the hledger project, as of 2022-12.

## hledger repo directories

<pre>
$ (cd ~/src/hledger; tre -d)
.
├── .github
│   ├── ISSUE_TEMPLATE
│   └── workflows
│       └── old
├── .sandstorm
├── bin
├── checks
├── doc
│   ├── haskellerz
│   ├── hcar
│   ├── mockups
│   └── profs
├── docker-static-arm32v7
├── docker
├── examples
│   ├── budgeting
│   ├── csv
│   ├── investing
│   ├── invoicing
│   │   ├── invoice-script
│   │   └── makefile
│   ├── reports
│   ├── systemd
│   └── templates
│       └── basic
├── hledger-install
├── hledger-lib
│   ├── Hledger
│   │   ├── Data
│   │   │   └── JournalChecks
│   │   ├── Read
│   │   ├── Reports
│   │   └── Utils
│   ├── Text
│   │   ├── Megaparsec
│   │   └── Tabular
│   ├── other
│   │   └── ledger-parse
│   │       └── Ledger
│   │           └── Parser
│   └── test
├── hledger-ui
│   ├── Hledger
│   │   └── UI
│   └── test
├── hledger-web
│   ├── Hledger
│   │   └── Web
│   │       ├── Handler
│   │       ├── Settings
│   │       └── Widget
│   ├── app
│   ├── config
│   ├── deploy
│   ├── static
│   │   ├── css
│   │   ├── fonts
│   │   └── js
│   ├── templates
│   └── test
├── hledger
│   ├── Hledger
│   │   └── Cli
│   │       └── Commands
│   ├── app
│   ├── bench
│   ├── embeddedfiles
│   ├── shell-completion
│   └── test
│       ├── addons
│       ├── balance
│       ├── cli
│       ├── errors
│       ├── i18n
│       ├── import
│       ├── journal
│       │   └── account-display-order
│       │       └── 1
│       ├── print
│       └── register
└── tools
</pre>

## site repo directories

<pre>
$ (cd ~/src/hledger/site; tre -d)
.
├── css
├── js
├── src
│   ├── 1.0
│   ├── 1.1
│   ├── 1.10
│   ├── 1.11
│   ├── 1.12
│   ├── 1.13
│   ├── 1.14
│   ├── 1.15
│   ├── 1.16
│   ├── 1.17
│   ├── 1.18
│   ├── 1.19
│   ├── 1.2
│   ├── 1.20
│   ├── 1.21
│   ├── 1.22
│   ├── 1.23
│   ├── 1.24
│   ├── 1.25
│   ├── 1.26
│   ├── 1.27
│   ├── 1.28
│   ├── 1.3
│   ├── 1.4
│   ├── 1.5
│   ├── 1.9
│   ├── dev
│   ├── fonts
│   ├── highslide
│   │   └── graphics
│   │       └── outlines
│   └── images
│       ├── hledger-ui
│       └── hledger-web
│           ├── normal
│           └── small
└── theme
    └── css
</pre>

## finance repo directories

<pre>
$ (cd ~/src/hledger/finance; tre -d)
.
</pre>
