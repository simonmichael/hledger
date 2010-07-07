#!/usr/bin/env runhaskell
{-o
hakyll build script for hledger.org
requires Hakyll >= 2.1, pandoc >= 1.5
-}

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import System.Process
import Text.Hakyll
import Text.Hakyll.HakyllMonad
import Text.Hakyll.Render
import Text.Hakyll.CreateContext (createPage, createCustomPage, createListing)
import Text.Pandoc
import Text.Printf

pandocparsercfg = defaultParserState {
  -- stateParseRaw        = False, -- ^ Parse raw HTML and LaTeX?
  -- stateParserContext   = NullState, -- ^ Inside list?
  -- stateQuoteContext    = NoQuote,   -- ^ Inside quoted environment?
  -- stateSanitizeHTML    = False,     -- ^ Sanitize HTML?
  -- stateKeys            = [],        -- ^ List of reference keys
  -- stateNotes           = [],        -- ^ List of notes
  -- stateTabStop         = 4,         -- ^ Tab stop
  -- stateStandalone      = False,     -- ^ Parse bibliographic info?
  -- stateTitle           = [],        -- ^ Title of document
  -- stateAuthors         = [],        -- ^ Authors of document
  -- stateDate            = [],        -- ^ Date of document
  -- stateStrict          = False,     -- ^ Use strict markdown syntax?
  stateSmart           = False     -- ^ Use smart typography?
  -- stateLiterateHaskell = False,     -- ^ Treat input as literate haskell
  -- stateColumns         = 80,        -- ^ Number of columns in terminal
  -- stateHeaderTable     = [],        -- ^ Ordered list of header types used
  -- stateIndentedCodeClasses = []     -- ^ Classes to use for indented code blocks
  }

pandocwritercfg = defaultWriterOptions {
  writerStandalone       = False -- ^ Include header and footer
 -- ,writerTemplate         = ""    -- ^ Template to use in standalone mode
 -- ,writerVariables        = []    -- ^ Variables to set in template
 -- ,writerIncludeBefore    = ""    -- ^ Text to include before the body
 -- ,writerIncludeAfter     = ""    -- ^ Text to include after the body
 -- ,writerTabStop          = 4     -- ^ Tabstop for conversion btw spaces and tabs
 -- ,writerTableOfContents  = False -- ^ Include table of contents
 -- ,writerS5               = False -- ^ We're writing S5
 -- ,writerXeTeX            = False -- ^ Create latex suitable for use by xetex
 -- ,writerHTMLMathMethod   = PlainMath -- ^ How to print math in HTML
 -- ,writerIgnoreNotes      = False     -- ^ Ignore footnotes (used in making toc)
 -- ,writerIncremental      = False     -- ^ Incremental S5 lists
 -- ,writerNumberSections   = False     -- ^ Number sections in LaTeX
 -- ,writerStrictMarkdown   = False     -- ^ Use strict markdown syntax
 -- ,writerReferenceLinks   = False     -- ^ Use reference links in writing markdown, rst
 -- ,writerWrapText         = True      -- ^ Wrap text to line length
 -- ,writerLiterateHaskell  = False     -- ^ Write as literate haskell
 -- ,writerEmailObfuscation = JavascriptObfuscation -- ^ How to obfuscate emails
 -- ,writerIdentifierPrefix = ""                    -- ^ Prefix for section & note ids in HTML
 }

hakyllcfg = (defaultHakyllConfiguration "http://hledger.org") {
--  absoluteUrl = "http://hledger.org" -- Absolute URL of the site.
 -- ,additionalContext = Context -- An additional context to use when rendering. This additional context is used globally.
 -- ,siteDirectory = FilePath -- Directory where the site is placed.
 -- ,cacheDirectory = FilePath -- Directory for cache files.
 -- ,enableIndexUrl = Bool -- Enable index links.
 -- ,previewPollDelay = Int -- Delay between polls in preview mode.
  pandocParserState = pandocparsercfg
 ,pandocWriterOptions = pandocwritercfg
 }

main = hakyllWithConfiguration hakyllcfg $ do
    -- renderChain [] $ createPage "t.markdown"
    mapM_ renderParentDirPage
      ["README.rst"
      ,"README2.rst"
      ,"NEWS.rst"
      ,"SCREENSHOTS.rst"
      ,"MANUAL.markdown"
      ,"CONTRIBUTORS.rst"
      ]
    mapM_ static
      ["style.css"
      ,"sshot.png"
      ,"watchhours.png"
      ,"hledger-screen-1.png"
      ,"hledger-charts-2.png"
      ]

-- Render a page from the parent directory as if it was in the hakyll
-- root dir, setting up a symbolic link when needed.
renderParentDirPage p = do
  liftIO $ system $ printf "[ -f %s ] || ln -s ../%s" p p
  renderChain ["site.tmpl"] $ createPage p

