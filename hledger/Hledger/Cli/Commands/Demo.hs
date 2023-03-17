{-|
The @demo@ command lists and plays small hledger demos in the terminal, using asciinema.
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Hledger.Cli.Commands.Demo (
  demomode
 ,demo
) where

import Hledger
import Hledger.Cli.CliOptions
import Control.Monad (forM_)
import System.Exit (exitSuccess, exitFailure)
import Text.Printf
import Control.Concurrent (threadDelay)
import System.Process (callProcess)
import System.IO.Error (catchIOError)
import Safe (readMay, atMay, headMay)
import Data.List (isPrefixOf, find, isInfixOf)
import Data.Char (isDigit)
import Control.Applicative ((<|>))
import Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)

data Demo = Demo {
  dfilename :: FilePath,    -- file name
  dtitle    :: String,      -- asciinema title field, effectively a description
  _dcontent :: ByteString   -- asciinema v2 content
}

demos :: [Demo]
demos = map readDemo [
   ("install.cast", $(embedFileRelative "embeddedfiles/install.cast"   ))
  ,("add.cast",     $(embedFileRelative "embeddedfiles/add.cast"       )) 
  ,("reports.cast", $(embedFileRelative "embeddedfiles/reports.cast"   )) 
  ]

-- | Command line options for this command.
demomode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Demo.txt")
  []
  [generalflagsgroup3]
  []
  ([], Just $ argsFlag "[NUM|NAME|STR] [-- ASCIINEMAOPTS]")

-- | The demo command.
demo :: CliOpts -> Journal -> IO ()
demo CliOpts{rawopts_=rawopts, reportspec_=ReportSpec{_rsQuery=_query}} _j = do
  -- demos <- getCurrentDirectory >>= readDemos
  let args = listofstringopt "args" rawopts
  case args of
    [] -> do
      forM_ (zip [(1::Int)..] demos) $ \(i, Demo f t _) -> printf "%d) %-15s  %s\n" i f t
      exitSuccess

    (a:as) ->
      case findDemo demos a of
        Nothing -> do
          putStrLn $ a <> " not recognized"
          putStrLn "Usage: hledger-demo [NUM|NAME|STR], run with no arguments to see a list"
          exitFailure

        Just (Demo f t c) -> do
          printf "playing (space to pause, . to step, ctrl-c to quit):\n %-15s  %s\n" f t
          threadDelay 1000000
          putStr "\n"
          runAsciinemaPlay c as

readDemo :: (FilePath, ByteString) -> Demo
readDemo (name, content) = Demo name title content
  where
    title = maybe "" (readTitle . B.unpack) $ headMay $ B.lines content
      where
        readTitle s
          | "\"title\":" `isPrefixOf` s = takeWhile (/='"') $ drop 1 $ lstrip $ drop 8 s
          | null s = ""
          | otherwise = readTitle $ tail s

findDemo :: [Demo] -> String -> Maybe Demo
findDemo ds s
  | all isDigit s = readMay s >>= atMay ds . subtract 1  -- find by number
  | otherwise =
         find ((==sl).lowercase.dfilename)            ds  -- or by name, ignoring case
     <|> find ((sl `isInfixOf`).lowercase.dfilename)  ds  -- or by name substring
     <|> find ((sl `isInfixOf`).lowercase.dtitle)     ds  -- or by title substring
    where
      sl = lowercase s

-- | Run asciinema play, passing content to its stdin.
runAsciinemaPlay :: ByteString -> [String] -> IO ()
runAsciinemaPlay content args =
  withSystemTempFile "hledger-cast" $ \f h -> do  -- try piping to stdin also
    B.hPutStrLn h content >> hClose h
    callProcess "asciinema" ("play" : f : args)
      `catchIOError` \err -> do
        putStrLn $ "There was a problem. Is asciinema installed ?\n" <> show err  --  (or PowerSession on Windows)
        exitFailure

