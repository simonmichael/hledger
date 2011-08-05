{-# LANGUAGE TemplateHaskell #-}
{-| 

Support files (static files and templates) used by the web app are
embedded in this module at compile time. Since hamlet can not easily use
these directly, we provide a way to write them out to the filesystem at
startup, when needed. This simplifies installation for end-users, and
customisation too.

-}
module Hledger.Web.EmbeddedFiles
    (
     files
    ,createFilesIfMissing
    )
where
import Control.Monad
import qualified Data.ByteString as B
import Data.FileEmbed (embedDir)
import System.Directory
import System.FilePath

import Hledger.Web.Settings (datadir)

-- | An embedded copy of all files below the the hledger-web data
-- directory (@.hledger/web/@) at compile time, as (FilePath,ByteString)
-- pairs.
files :: [(FilePath, B.ByteString)]
files = $(embedDir datadir)

-- | If the hledger-web data directory (@.hledger/web/@) does not exist in
-- the current directory, create and fill it with the web app support
-- files (templates, stylesheets, images etc.) Returns True if the
-- directory was missing.
createFilesIfMissing :: IO Bool
createFilesIfMissing = do
  exists <- doesDirectoryExist datadir
  if exists
   then return False
   else do
     createDirectoryIfMissing True datadir  
     setCurrentDirectory datadir
     forM_ files $ \(f,d) -> do
                              createDirectoryIfMissing True $ takeDirectory f
                              B.writeFile f d
     return True
