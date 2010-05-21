#!/usr/bin/env runhaskell

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import System.Process
import Text.Hakyll (hakyll)
import Text.Hakyll.Render
import Text.Hakyll.CreateContext (createPage, createCustomPage, createListing)
import Text.Printf

main = hakyll "http://hledger.org" $ do
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
