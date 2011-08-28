#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-
hakyll (3.2) build script for hledger.org
-}


import Prelude hiding (id)
import Control.Arrow ((>>>)) --, (***), arr)
-- import Control.Category (id)
import Control.Monad
import Data.List
-- import Data.Monoid (mempty, mconcat)
import Hakyll
import System.Directory
import System.Process (system)
import Text.Pandoc
import Text.Printf

main = do
  symlinkPagesFromParentDir
  symlinkIndexHtml
  symlinkProfsDir
  hakyll $ do
    match "templates/*" $ compile templateCompiler
    match "css/*" css
    match "images/*" file
    match "js/**" file
    match "robots.txt" file
    match "*.md" page

symlinkPagesFromParentDir = do
  fs <- filter (".md" `isSuffixOf`) `fmap` getDirectoryContents ".."
  forM_ fs $ \f -> system $ printf "[ -f %s ] || ln -s ../%s" f f
symlinkIndexHtml = ensureSiteDir >> system "ln -sf README.html _site/index.html"
symlinkProfsDir = ensureSiteDir >> system "ln -sf ../../profs _site/profs"
ensureSiteDir = system "mkdir -p _site"
file = route idRoute >> compile copyFileCompiler
css = route idRoute >> compile compressCssCompiler
page = do
  route $ setExtension "html"
  compile $ pageCompilerWith pandocParserState pandocWriterOptions >>> applyTemplateCompiler "templates/default.html" >>> relativizeUrlsCompiler

pandocParserState = defaultParserState {-
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
   -- stateSmart           = False     -- ^ Use smart typography?
   -- stateLiterateHaskell = False,     -- ^ Treat input as literate haskell
   -- stateColumns         = 80,        -- ^ Number of columns in terminal
   -- stateHeaderTable     = [],        -- ^ Ordered list of header types used
   -- stateIndentedCodeClasses = []     -- ^ Classes to use for indented code blocks
 -}

pandocWriterOptions = defaultWriterOptions {
   writerStandalone       = True, -- ^ Include header and footer -- needs to be true to have a toc
   writerTemplate         =  -- ^ Template to use in standalone mode
    unlines
    [ "$if(title)$"
    , "<h1 class=\"title\">$title$</h1>"
    , "$endif$"
    , "$for(include-before)$"
    , "$include-before$"
    , "$endfor$"
    , "$if(toc)$"
    , "$toc$"
    , "$endif$"
    , "$body$"
    , "$for(include-after)$"
    , "$include-after$"
    , "$endfor$"
    ],
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
