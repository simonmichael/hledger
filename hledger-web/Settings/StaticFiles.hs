module Settings.StaticFiles where

import Prelude (IO, putStrLn, (++), (>>), return)
import System.IO (stdout, hFlush)
import Yesod.Static
import qualified Yesod.Static as Static
import Settings (staticDir)
import Settings.Development

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


staticSite :: IO Static.Static
staticSite =
  if development
   then (do
            putStrLn ("Using web files from: " ++ staticDir ++ "/") >> hFlush stdout
            Static.staticDevel staticDir)
   else (do
            -- putStrLn "Using built-in web files" >> hFlush stdout
            return $(Static.embed staticDir))

$(publicFiles staticDir)
