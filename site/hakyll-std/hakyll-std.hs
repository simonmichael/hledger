#!/usr/bin/env stack
{- stack runghc --verbosity info --package hakyll --package pandoc -}  -- pandoc-1.17.0.3
{-# LANGUAGE OverloadedStrings #-}
{- |

A simple hakyll website builder suitable for software project sites,
intended to be used as-is without recompilation. Functionality:

- copies these static files to _site/ :
  *.{html,htm,css,js,gif,jpg,jpeg,png}
  {css,img,js,files}/** (** means everything below)
  site/{css,img,js,files,etc}/**
  doc/**.{html,htm,txt,gif,jpg,jpeg,png}

- renders these markdown files to _site/*.html :
  *.{md,mdwn,markdown}
  doc/**.{md,mdwn,markdown}

- applies this template file to markdown content:
  site.tmpl or site/site.tmpl (the first found)

- a single markdown list item containing the word "toc" is replaced by
  a table of contents based on headings

- syntax highlighting of fenced code blocks in markdown is enabled
  (if you provide suitable kate styles, eg a syntax.css)

Usage:

$ hakyll-std [--help|clean|build|preview|...]   # standard hakyll options

-}

import Control.Monad
import Data.Default
import Hakyll
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.Process (system)
-- import Text.Highlighting.Kate (pygments, kate, espresso, tango, haddock, monochrome, zenburn)
import Text.Pandoc.Options

import TableOfContents (tableOfContents)

import Debug.Trace
strace :: Show a => a -> a
strace a = trace (show a) a

filesToCopy =
  ["site/css/**"
  ,"site/js/**"
  ,"site/img/**"
  ,"site/images/**"
  ,"site/fonts/**"
  ,"site/files/**"
  ,"site/etc/**"
  ,"site/*.html"
  ,"site/*.htm"
  ,"site/*.gif"
  ,"site/*.jpg"
  ,"site/*.jpeg"
  ,"site/*.png"
  ,"site/*.css"
  ,"site/*.js"
  ,"css/**"
  ,"js/**"
  ,"img/**"
  ,"images/**"
  ,"fonts/**"
  ,"files/**"
  ,"doc/**.html"
  ,"doc/**.htm"
  ,"doc/**.txt"
  ,"doc/**.gif"
  ,"doc/**.jpg"
  ,"doc/**.jpeg"
  ,"doc/**.png"
  ,"*.html"
  ,"*.htm"
  ,"*.css"
  ,"*.js"
  ,"*.gif"
  ,"*.jpg"
  ,"*.jpeg"
  ,"*.png"
  ]

filesToRender =
  ["*.md"
  ,"*.mdwn"
  ,"*.markdown"
  ,"doc/**.md"
  ,"doc/**.mdwn"
  ,"doc/**.markdown"
  ]

-- http://hackage.haskell.org/package/pandoc-1.13/docs/src/Text-Pandoc-Options.html#ReaderOptions
pandocReaderOptions = def

-- http://hackage.haskell.org/package/pandoc-1.13/docs/src/Text-Pandoc-Options.html#WriterOptions
pandocWriterOptions = def
  {writerHighlight=True
  -- this would change the value of pandoc's $highlight-css$ var
  -- for now, let the user provide these styles
  -- ,writerHighlightStyle=tango
  }

pandocTransform = tableOfContents "right"

main = do
  args <- getArgs
  when (any (`elem` args) ["--version"]) $ do
    putStrLn "hakyll standard site builder v0.1"
    exitSuccess

  hakyll $ do

    match (foldl1 (.||.) filesToCopy) $ route idRoute >> compile copyFileCompiler

    -- there might or might not be a site template in ./ or ./site/
    mtmpl <- preprocess $ do
      t1 <- doesFileExist "site.tmpl"
      t2 <- doesFileExist "site/site.tmpl"
      return $ case (t1, t2) of (False, True)  -> Just "site/site.tmpl"
                                (True, _)      -> Just "site.tmpl"
                                (False, False) -> Nothing
    case mtmpl of
      Just tmpl -> match tmpl $ compile templateCompiler
      Nothing   -> return ()

    match (foldl1 (.||.) filesToRender) $ do
      route   $ setExtension "html"
      compile $
        pandocCompilerWithTransformM pandocReaderOptions pandocWriterOptions (return . pandocTransform)
        >>= (case mtmpl of
                Just tmpl -> loadAndApplyTemplate (fromCapture tmpl "") defaultContext
                Nothing   -> return)
        >>= relativizeUrls

    -- this fails the first time after a clean because it runs before README.html generation
    -- when ("build" `elem` args) $ preprocess linkReadmeToIndex

  -- can't do anything here, hakyll exits

linkReadmeToIndex = void $ system "ln -sf README.html _site/index.html"
