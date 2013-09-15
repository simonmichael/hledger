#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll

import           Control.Monad
import           Data.List
import           System.Directory
import           System.Process
import           Text.Pandoc.Options
import           Text.Printf

main = do
  symlinkPagesFromParentDir
  symlinkProfsDir
  hakyll $ do
    match ("images/*" .||. "js/**" .||. "robots.txt") $ do
        route   idRoute
        compile copyFileCompiler
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    match "templates/*" $ compile templateCompiler
    match ("README.md") $ do
        route $ constRoute "index.html"
        compile $
          pandocCompilerWith def def
          >>= loadAndApplyTemplate "templates/frontpage.html" defaultContext
          >>= relativizeUrls
    match (("*.md" .&&. complement "README.md") .||. "0.21/*.md" .||. "0.20/*.md" .||. "0.19/*.md" .||. "0.18/*.md") $ do
        route   $ setExtension "html"
        compile $
          pandocCompilerWith
            def
            def{writerTableOfContents=True
               ,writerTOCDepth=4
               ,writerStandalone=True
               ,writerTemplate="<div id=toc>$toc$</div>\n$body$"
               }
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

symlinkPagesFromParentDir = do
  fs <- filter (".md" `isSuffixOf`) `fmap` getDirectoryContents ".."
  forM_ fs $ \f -> system $ printf "[ -f %s ] || ln -s ../%s" f f

symlinkProfsDir = ensureSiteDir >> system "ln -sf ../../profs _site/profs"

ensureSiteDir = system "mkdir -p _site"


