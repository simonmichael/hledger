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
  symlinkIndexHtml
  symlinkProfsDir
  hakyll $ do
    match ("images/*" .||. "js/**" .||. "robots.txt") $ do
        route   idRoute
        compile copyFileCompiler
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    match "templates/*" $ compile templateCompiler
    match "*.md" $ do
        route   $ setExtension "html"
        compile $
          pandocCompilerWith
            def
            def{writerTableOfContents=True
               ,writerStandalone=True
               ,writerTemplate="<div id=toc>$toc$</div>\n$body$"
               }
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

symlinkPagesFromParentDir = do
  fs <- filter (".md" `isSuffixOf`) `fmap` getDirectoryContents ".."
  forM_ fs $ \f -> system $ printf "[ -f %s ] || ln -s ../%s" f f

symlinkIndexHtml = ensureSiteDir >> system "ln -sf README.html _site/index.html"

symlinkProfsDir = ensureSiteDir >> system "ln -sf ../../profs _site/profs"

ensureSiteDir = system "mkdir -p _site"
