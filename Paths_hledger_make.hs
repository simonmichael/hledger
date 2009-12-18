-- dummy Paths_hledger for use when building with make
-- cabal build generates a more useful one

module Paths_hledger_make
where

import System.FilePath.Posix ((</>))

getDataFileName path = return $ "data" </> path
