-- stub version of cabal's Paths_hledger_web.hs used when not building with cabal
-- assumes "data" dir is in current directory 
module Paths_hledger_web_make where
import System.FilePath.Posix ((</>))
getDataFileName path = return $ "data" </> path
