{-# LANGUAGE TemplateHaskell #-}
module Hledger.Web.Settings.StaticFiles where

import System.IO (stdout, hFlush)
import Yesod.Static (Static, embed, publicFiles, staticDevel)

import Hledger.Web.Settings (staticDir, development)

-- | use this to create your static file serving site
-- staticSite :: IO Static.Static
-- staticSite = if development then Static.staticDevel staticDir
--                             else Static.static      staticDir
--
-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
-- $(staticFiles Settings.staticDir)


staticSite :: IO Static
staticSite =
  if development
   then (do
            putStrLn ("Using web files from: " ++ staticDir ++ "/") >> hFlush stdout
            staticDevel staticDir)
   else (do
            -- putStrLn "Using built-in web files" >> hFlush stdout
            return $(embed staticDir))

$(publicFiles staticDir)
