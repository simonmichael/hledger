{-# LANGUAGE TemplateHaskell #-}
{-| 

Support files used by the web app are embedded here at compile time via
template haskell magic.  This allows us minimise deployment hassle by
recreating them on the filesystem when needed (since hamlet can not use
the embedded files directly.)  Installing on the filesystem has the added
benefit of making them easily customisable.

-}
module Hledger.Web.Files
    (
     files
    ,createFilesIfMissing
    )
where
import Control.Monad
import qualified Data.ByteString as B
import Data.FileEmbed (embedDir)
import System.Directory

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
     forM_ files $ \(f,d) -> B.writeFile f d
     return True
