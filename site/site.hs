#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{- hakyll script to build hledger.org -}

import Data.Monoid
import Hakyll
import System.Process
import Text.Pandoc.Options


symlinkProfsDir = ensureSiteDir >> system "ln -sf ../../profs _site/profs"
  where
    ensureSiteDir = system "mkdir -p _site"

main = do
  symlinkProfsDir

  hakyll $ do

    match ("images/*" .||. "js/**" .||. "robots.txt") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match ("doc/README.md") $ do
        route $ constRoute "index.html"
        compile $
          pandocCompilerWith def def
          >>= loadAndApplyTemplate "templates/frontpage.html" defaultContext
          >>= relativizeUrls

    match (("doc/*.md" .&&. complement "doc/README.md") .||. "0.22/*.md" .||. "0.21/*.md" .||. "0.20/*.md" .||. "0.19/*.md" .||. "0.18/*.md") $ do
        route   $ gsubRoute "doc/" (const "") `mappend` setExtension "html"
        compile $
          pandocCompilerWith
            def
            def{writerTableOfContents=True
               ,writerTOCDepth=5
               ,writerStandalone=True
               ,writerTemplate="<div id=toc>$toc$</div>\n$body$"
               }
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls
