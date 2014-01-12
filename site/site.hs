#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{- hakyll script to build hledger.org -}

import Control.Applicative ((<$>))
import Control.Monad
import Data.List
import Data.Monoid         (mappend)
import Hakyll
import System.Directory
import System.FilePath
import System.Process
import Text.Pandoc.Options
import Text.Printf


docDir = "../doc"

-- hakyll's preview doesn't detect changes in symlinked files
-- symlinkDocs = do
--   filter (".md" `isSuffixOf`) <$> getDirectoryContents docDir
--     >>= mapM_ (\f -> system $ printf "[ -f %s ] || ln -s %s/%s" f docDir f)

copyDocsIfNewer = do
  fs <- filter (".md" `isSuffixOf`) <$> getDirectoryContents docDir
  forM_ fs $ \f -> do
    let f1 = docDir </> f
        f2 = "."    </> f
    t1 <- getModificationTime f1
    t2 <- getModificationTime f2
    when (t1 > t2) $ copyFile f1 f2

symlinkProfsDir = ensureSiteDir >> system "ln -sf ../../profs _site/profs"
  where
    ensureSiteDir = system "mkdir -p _site"

main = do
  copyDocsIfNewer
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

    match (("*.md" .&&. complement "README.md") .||. "0.22/*.md" .||. "0.21/*.md" .||. "0.20/*.md" .||. "0.19/*.md" .||. "0.18/*.md") $ do
        route   $ setExtension "html"
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
