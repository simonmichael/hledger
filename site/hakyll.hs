#!/usr/bin/env runhaskell
{-
hakyll build script for hledger.org
requires Hakyll >= 2.1, pandoc >= 1.5
-}

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import System.Process (system)
import Text.Hakyll (hakyllWithConfiguration, defaultHakyllConfiguration)
import Text.Hakyll.HakyllMonad (HakyllConfiguration(..))
import Text.Hakyll.Render (renderChain, static)
import Text.Hakyll.CreateContext (createPage, createCustomPage, createListing)
import Text.Pandoc (ParserState(..), WriterOptions(..), defaultParserState, defaultWriterOptions)
import Text.Printf

baseurl = "http://hledger.org"

main = hakyllWithConfiguration cfg $ do
    mapM_ renderParentDirPage
      ["README.rst"
      ,"DEVELOPMENT.rst"
      ,"NEWS.rst"
      ,"SCREENSHOTS.markdown"
      ,"MANUAL.markdown"
      ,"CONTRIBUTORS.rst"
      ]
    mapM_ static
      ["style.css"
      ,"highslide/highslide.js"
      ,"highslide/highslide.css"
      ,"highslide/highslide-ie6.css"
      ,"highslide/graphics/zoomin.cur"
      ,"highslide/graphics/zoomout.cur"
      ,"highslide/graphics/outlines/rounded-black.png"
      ,"highslide/graphics/outlines/rounded-white.png"
      ,"highslide/graphics/outlines/beveled.png"
      ,"highslide/graphics/outlines/drop-shadow.png"
      ,"highslide/graphics/outlines/glossy-dark.png"
      ,"highslide/graphics/outlines/outer-glow.png"
      ,"highslide/graphics/loader.gif"
      ,"highslide/graphics/loader.white.gif"
      ,"highslide/graphics/icon.gif"
      ,"highslide/graphics/resize.gif"
      ,"highslide/graphics/fullexpand.gif"
      ,"highslide/graphics/geckodimmer.png"
      ,"highslide/graphics/close.png"
      ,"highslide/graphics/closeX.png"
      ,"sshot.png"
      ,"watchhours.png"
      ,"hledger-screen-1.png"
      ,"hledger-charts-2.png"
      ,"hledger-web-journal.png"
      ]
    where
      -- Render a page from the parent directory as if it was in the hakyll
      -- root dir, setting up a symbolic link when needed.
      renderParentDirPage p = do
        liftIO $ system $ printf "[ -f %s ] || ln -s ../%s" p p
        renderChain ["site.tmpl"] $ createPage p

cfg :: HakyllConfiguration
cfg = (defaultHakyllConfiguration baseurl) {
  -- additionalContext = Context, -- An additional context to use when rendering. This additional context is used globally.
  -- siteDirectory = FilePath, -- Directory where the site is placed.
  -- cacheDirectory = FilePath, -- Directory for cache files.
  -- enableIndexUrl = False, -- Enable index links.
  -- previewPollDelay = Int, -- Delay between polls in preview mode.
  pandocParserState = defaultParserState {
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
  },
  pandocWriterOptions = defaultWriterOptions {
                            -- so we can have a TOC:
   writerStandalone       = True, -- ^ Include header and footer
   writerTemplate         = pandocTemplate, -- ^ Template to use in standalone mode
   -- writerVariables        = [],    -- ^ Variables to set in template
   -- writerIncludeBefore    = "",    -- ^ Text to include before the body
   -- writerIncludeAfter     = "",    -- ^ Text to include after the body
   -- writerTabStop          = 4,     -- ^ Tabstop for conversion btw spaces and tabs
   writerTableOfContents  = True -- ^ Include table of contents
   -- writerS5               = False, -- ^ We're writing S5
   -- writerXeTeX            = False, -- ^ Create latex suitable for use by xetex
   -- writerHTMLMathMethod   = PlainMath, -- ^ How to print math in HTML
   -- writerIgnoreNotes      = False,     -- ^ Ignore footnotes (used in making toc)
   -- writerIncremental      = False,     -- ^ Incremental S5 lists
   -- writerNumberSections   = False,     -- ^ Number sections in LaTeX
   -- writerStrictMarkdown   = False,     -- ^ Use strict markdown syntax
   -- writerReferenceLinks   = False,     -- ^ Use reference links in writing markdown, rst
   -- writerWrapText         = True,      -- ^ Wrap text to line length
   -- writerLiterateHaskell  = False,     -- ^ Write as literate haskell
   -- writerEmailObfuscation = JavascriptObfuscation, -- ^ How to obfuscate emails
   -- writerIdentifierPrefix = "",                    -- ^ Prefix for section & note ids in HTML
  }
 }

-- the body part of pandoc 1.5.1.1's html output template
pandocTemplate = "\
\$if(title)$\
\<h1 class=\"title\">$title$</h1>\
\$endif$\
\$for(include-before)$\
\$include-before$\
\$endfor$\
\$if(toc)$\
\$toc$\
\$endif$\
\$body$\
\$for(include-after)$\
\$include-after$\
\$endfor$\
\"
