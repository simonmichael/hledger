{-|
Read extra CLI arguments from a hledger config file.
Currently this reads only general options from ./hledger.conf if it exists.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Cli.Conf (
   getConf
  ,confArgsFor
)
where

import Control.Exception (IOException, catch, tryJust)
import Control.Monad (guard)
import Data.Either (fromRight)
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import System.IO.Error (isDoesNotExistError)

import Hledger (error', strip)


localConfPath = "hledger.conf"

-- | A hledger config file.
data Conf = Conf {
   confFile :: FilePath
  ,confText :: String
  ,confFormat :: Int
  ,confSections :: [ConfSection]
} deriving (Eq,Show)

-- | One section in a hledger config file.
data ConfSection = ConfSection {
   csName :: SectionName
  ,csArgs :: [Arg]
} deriving (Eq,Show)

type SectionName = String
type Arg = String

nullconf = Conf {
   confFile = ""
  ,confText = ""
  ,confFormat = 1
  ,confSections = []
}

-- | Try to read a hledger config file.
-- If none is found, this returns a null Conf.
-- Any other IO error will cause an exit.
getConf :: IO Conf
getConf = (do
  let f = localConfPath
  et <- tryJust (guard . isDoesNotExistError) $ readFile f
  let f' = either (const "") (const f) et
  let t  = fromRight "" et
  return $ nullconf {
     confFile = f'
    ,confText = t
    ,confFormat = 1
    ,confSections = parseConf t
    }
  ) `catch` \(e :: IOException) -> error' $ show e

-- | Parse the content of a hledger config file
-- (a limited prototype, only reads general options until the first [sectionheading]).
parseConf :: String -> [ConfSection]
parseConf s =
  let
    conflines = filter (\l -> not $ null l || "#" `isPrefixOf` l) $ map strip $ lines s
    (ls1,rest) = break (("[" `isPrefixOf`)) conflines  -- XXX also breaks on lines like " [..."
  in
    ConfSection "general" ls1 : parseConfSections rest

parseConfSections :: [String] -> [ConfSection]
parseConfSections _ = []

-- | Fetch all the arguments/options defined in a section with this name, if it exists.
-- This should be "general" for the unnamed first section, or a hledger command name.
confArgsFor :: SectionName -> Conf -> [Arg]
confArgsFor cmd Conf{confSections} =
  fromMaybe [] $ 
  M.lookup cmd $ 
  M.fromList [(csName,csArgs) | ConfSection{csName,csArgs} <- confSections]
