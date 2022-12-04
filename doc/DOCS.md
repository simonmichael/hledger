# Docs

<div class=pagetoc>

<!-- toc -->
</div>

## Four kinds of documentation

<div style="margin:1em 2em; font-style:italic;">
"There is a secret that needs to be understood in order to write good
software documentation: there isn’t one thing called documentation,
there are four. They are: tutorials, how-to guides, explanation and
technical reference. They represent four different purposes or
functions, and require four different approaches to their creation."
--[Daniele Procida] (https://news.ycombinator.com/item?id=21289832)
</div>

## hledger's documentation structure

2019: out of date, needs update.

Project documentation lives in a number of places:

- `site/*.md` is the hledger.org website content, which is generated with hakyll[-std] and pandoc
- haddock documentation in the code appears on Hackage
- short blurbs: cabal files, module headers, HCAR, GSOC project, ..
- `doc/notes.org` has some old developer notes
- developer reports (profiles, benchmarks, coverage..) in doc/profs, sometimes published at hledger.org/profs
- https://github.com/simonmichael/hledger/tree/master/doc


site/ is now a symlink to the separate hledger_site repo.

See also Shake.hs.

hledger doc files can be divided into several groups: 

1. Project admin/dev notes not published on the website.
   These are kept in this directory (doc/). They include: 
   ``` 
   doc/finance/      project finances
   doc/hcar/         Haskell Community and Activities Report entries
   doc/lib.m4        common macros used in package manuals
   doc/manpage.*     misc. templates for rendering package manuals
   doc/mockups/      exploratory developer mockups   
   doc/profs/        a place for long-term profiling/performance data
   ```
2. Project doc files required to be in the top directory:
   ```
   README.md         the main project readme, displayed on github
   LICENSE           the default project license
   ```
3. Code/API docs in haskell source files as haddock comments:
   ```
   hledger*/**/*.hs  haddock module and function docs in most source files
   ```
4. Per-package descriptions, readmes, changelogs, and reference manuals.
   These are in the respective package directories:
   ```
   hledger*/package.yaml    source for package metadata (description, etc.)
   hledger*/README          package readme, displayed on hackage
   hledger*/CHANGES         package changelog, displayed on hackage
   hledger*/hledger*.m4.md  package manual source file(s)
   ```
5. The project website and additional docs - home page, FAQ, tutorials, 
   how-tos, developer guide, etc. These are in the site directory:
   ```
   site/             hledger.org website content, templates, assets
   ```

## Workflows

The manuals and website are rendered from the top directory, 
primarily using Shake. First, build Shake:

```
$ make Shake
```

Then render the per-package manuals from markdown-m4 source files (*.m4.md) 
to text, man, info, and markdown formats. This requires some unix tools 
such as m4:

```
$ ./Shake manuals
```

The text/man/info manuals are embedded in the hledger executable, so a 
rebuild of that now will pick up the latest manuals:

```
$ stack build hledger
```

The website uses the markdown manuals. This copies them into the
website, edits them for web display, concatenates them to form the
one-page manual, and runs pandoc to render everything as html,
adding tables of contents and the site header/footer.

```
$ ./Shake website
```

## 201901 docs reorg (#920, WIP)

https://groups.google.com/forum/#!topic/hledger/t2nVr3zER8Q/discussion

> > On Oct 26, 2018, at 1:47 PM, Simon Michael <simon@joyful.com> wrote:
> >
> > A quick heads-up: I am feeling like stepping back from github wiki, and reorganising our docs like so:
> >
> > Two repos:
> >
> > 1. hledger - code and hard docs
> >
> >   - code and code docs (haddock docs & doctest examples)
> >   - developer docs (READMEs in md or org format)
> >   - product manuals (hledger*/hledger*.m4.md)
> >   - release notes and announcements
> >   - HCAR entries
> >
> > 2. hledger-site - website and soft docs
> >
> >   - hledger.org content, resources, site infrastructure
> >   - user cookbook, how-tos, articles
> >   - links to blog posts, discussions etc.
> >   - other resources relating to our web presence/marketing
> >
> > If you disagree, let's discuss. Some quick considerations:
> >
> > - moving docs to the wiki hasn't affected the contribution rate
> > - using the wiki increases our dependence on github and makes our work less self-contained and future-proof
> > - the wiki docs don't look great, aren't very flexible, & don't integrate well with our site & static docs
> > - using two docs systems increases complexity
> > - dev docs in the wiki are too far from the code, and compete with READMEs
> 
> PS:
> 
> - Why not go back to just one repo for everything ? Or if two repos, why not put all docs in one of them ? 
> 
> Dev docs are most discoverable and maintainable right there in the main repo, ie as READMEs. Likewise for API docs (haddocks) and the reference manuals (hledger*/hledger*m4.md). We want all of these updated in lock step with code/tooling changes.
> 
> Other ("soft") docs are needed, but these have a more relaxed process, schedule, and scope (eg bookkeeping advice). They occasionally generate a lot of noise in the commit log, and I think it's a good to keep that out of the code history. The website (home and other pages, site design, site infrastructure) generates similar commit storms and is somewhat independent of code, so it goes in the soft docs repo too.
> 
> These are my thoughts, but I have an open mind if you see a better way.
> 
> 	me (Simon Michael (sm) change) 	
> 10/27/18
> Still plenty of time to discuss and reconsider, but see also
> https://github.com/simonmichael/hledger/issues/920
> https://github.com/simonmichael/hledger/issues/921
> 
> I'll probably make a start on the first one (consolidating dev docs in main repo).
