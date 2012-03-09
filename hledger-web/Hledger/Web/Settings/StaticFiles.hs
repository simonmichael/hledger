{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, CPP #-}
{-| 

This module exports routes for all the files in the static directory at
compile time, allowing compile-time verification that referenced files
exist. However, any files added during run-time can't be accessed this
way; use their FilePath or URL to access them.

This is a separate module to satisfy template haskell requirements.

-}
module Hledger.Web.Settings.StaticFiles where

import Prelude (IO)
import System.IO
import Yesod.Static
import qualified Yesod.Static as Static

import Prelude
import Hledger.Web.Settings (staticDir)

-- | use this to create your static file serving site
staticSite :: IO Static.Static
staticSite = do
#ifdef DEVELOPMENT
  putStrLn ("using web files from: " ++ staticDir ++ "/") >> hFlush stdout
  Static.staticDevel staticDir
#else
  putStrLn "using embedded web files" >> hFlush stdout
  return $(Static.embed staticDir)
#endif


-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(staticFiles staticDir)
