% Inside hledger: an architectural tour and how-to
% Simon Michael
% April 29, 2021

# Introductions

`0:00 (10m)`

## Who am I ?

Simon Michael\
Independent software consultant and developer.\
<!-- Currently based in Hawaii. -->
<https://joyful.com>

Started in the home computer era (Ireland, 80s). ZX-80, Pet, VIC, C64, Amiga, PC, Mac, VAX/VMS, unix..

FOSS fan, contributor, project leader. Two big projects:

- **Zwiki**, 1996-2006, wiki engine, Python/Zope

- **hledger**, 2006-present, accounting tool, Haskell

## Overview of talk

- A quick intro to plain text accounting and hledger. (10m)

- A tour of hledger's 
architecture,
codebase, 
evolutions,
and design principles. (25m)

- How to script and develop hledger:
custom commands,
calling from other apps,
troubleshooting,
bug investigation,
effective pull requests. (25m)
  
- Q&A to go deeper on whatever you like. (30m)


## What's Plain Text Accounting ?

A name I proposed (and a website I built) to help unify and grow the
communities around 
Ledger, hledger, Beancount and similar accounting
tools.

<https://plaintextaccounting.org>

## The PTA ecosystem

The "big three" apps:

 * **Ledger** - John Wiegley's pioneering command line accounting app,
   inspiration for all the rest.\
   C++, 2003-present

 * **hledger** - Simon Michael's rewrite/remix.\
   Haskell, 2007-present

 * **Beancount** - Martin Blais' variant.\
   Python, 2008-present

Many more apps and add-on tools - <https://plaintextaccounting.org/#software>


## What's hledger ?

A suite of 
tools,
file formats,
and documentation
for doing "plain text accounting".

<https://hledger.org>

## hledger project goals

I was a Ledger user and new Haskeller. I started hledger with many goals:

- acquire a more robust, usable, evolving incarnation of Ledger
- develop a consistent, effective accounting habit
- get better at Haskell
- find out if Haskell was good for end-user apps and maintainer productivity
- grow a useful, long-lasting, financially sustainable FOSS accounting project

## An end-user Haskell application

hledger is something that's not yet common: 

An end user application (ie: not a programming tool),
written in Haskell, that aims to be

- easy to install, 
- dependable, 
- and pleasing to use, 
- on all major platforms, 
- by non-techies as well as power users.

## A successful FOSS project

15 years, 134 contributors, 80-100 chatters, some number of happy
users, useful to me every week.

Reasonably successful.

Not fulfilling my original goal of being financially
self-sustaining and minimally time-consuming !

## Tools

  * hledger
  * hledger-ui
  * hledger-web
  * hledger-iadd
  * hledger-interest
  * etc.

## File formats

  * journal
  * csv
  * timeclock
  * timedot

## Documentation

  * introductory
  * reference
  * cookbook
  * developer


# Architecture

`0:10 (25m)`

## A fast run through the evolution

- First commit
- First types
- First report
- Balance report
- Multi-column balance report
- Stateful parser
- Web interface
- Curses interface
- Package split
- Speed, features
- General currency conversion
- Valuation
- Always documentation

## A tour of the codebase and project today (2021Q2)


## Design principles


# How to..

`0:35 (25m)`

## Script hledger



  * <https://hledger.org/scripting.html>

## Integrate hledger in your app

  *  Run cli, capture csv/json

  *  Run hledger-web, use http-json api

  *  Link with hledger-lib and/or other hledger packages, call public functions

## Investigate a hledger bug

  *  Basic troubleshooting - docs, comparison, minimisation, reproduction, support

  *  Code debugging

## How to submit a pull request

  * <https://hledger.org/CONTRIBUTING.html#pull-requests>

# Q & A

`1:00 (30m)`

# End

`1:30`

```
$ git shortlog -s
Aerex
Aiken Cairncross
Alejandro García Montoro
Aleksandar Dimitrov
Alex Chen
Alvaro Fernando García
Amarandus
Amitai Burstein
Andreas Pauley
Andrew Jones
Andriy Mykhaylyk
Arnout Engelen
Ben Boeckel
Ben Creasy
Boyd Kelly
Brian Scott
Brian Wignall
Bryan Richter
Caleb Maclennan
Carel Fellinger
Carl Richard Theodor Schneider
Carlos Lopez-Camey
Christian G. Warden
Christoph Nicolai
Clint Adams
Colin Woodbury
Damien Cassou
David Reaver
David Zhang
Dmitry Astapov
Dominik Süß
Doug Goldstein
Eli Flanagan
Elijah Caine
Eric Kow
Eric Mertens
Everett Hildenbrandt
Evilham
Felix Van der Jeugt
Felix Yan
Fun Ilrys (Nissar Chababy)
Gabriel Ebner
Gaith Hallak
Gergely Risko
Hans-Peter Deifel
Henning Thielemann
Imuli
Jacek Generowicz
Jacob Weisz
Jakob Schöttl
Jakub Zárybnický
Jan Zerebecki
Jeff Richards
Jesse Rosenthal
Joachim Breitner
Joe Horsnell
Johann Klähn
Johannes Gerer
John Wiegley
Joseph Weston
Joshua Chia
Joshua Kehn
Judah Jacobson
Julien Moutinho
Justin Le
Kyle Marek-Spartz
Luca Molteni
Léo Gaspard
M Parker
Malte Brandy
Mark Hansen
Marko Kocić
Martin Michlmayr
Mateus Furquim
Matthias Kauer
Max Bolingbroke
Michael Kainer
Michael Sanders
Michael Snoyman
Michael Walker
Mick Dekkers
Mitchell Rosen
Moritz Kiefer
Mykola Orliuk
Nadrieril
Nicholas Niro
Nick Ingolia
Nicolas Wavrant
Nikhil Jha
Nissar Chababy
Nolan Darilek
Oliver Braun
Omari Norman
Pavan Rikhi
Pavlo Kerestey
Peter Simons
Pia Mancini
Rick Lupton
Roman Cheplyaka
Rui Chen
Ryan Desfosses
Sam Doshi
Sam Jeeves
Samuel May
Sergei Trofimovich
Sergey Astanin
Shubham Lagwankar
Simon Hengel
Simon Michael
SpicyCat
Stefano Rodighiero
Stephen Morgan
Steven R. Baker
TANIGUCHI Kohei
Thomas R. Koll
Tim Docker
Timofey ZAKREVSKIY
Trygve Laugstøl
Vladimir Sorokin
Vladimir Zhelezov
Wad
Xinruo Sun
afarrow
agander
aragaer
awjchen
azure-pipelines[bot]
flip111
gwern
jeevcat
jungle-boogie
legrostdg
trevorriles
zieone
```

<!-- 
https://pandoc.org/MANUAL.html#slide-shows

By default, the slide level is the highest heading level in the
hierarchy that is followed immediately by content, and not another
heading, somewhere in the document. In the example above, level-1
headings are always followed by level-2 headings, which are followed
by content, so the slide level is 2. This default can be overridden
using the --slide-level option.

The document is carved up into slides according to the following rules:
A horizontal rule always starts a new slide.
A heading at the slide level always starts a new slide.
Headings below the slide level in the hierarchy create headings within a slide.
Headings above the slide level in the hierarchy create “title slides,” which just contain the section title and help to break the slide show into sections. Non-slide content under these headings will be included on the title slide (for HTML slide shows) or in a subsequent slide with the same title (for beamer).
-->

