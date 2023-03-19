{-|
The @demo@ command lists and plays small hledger demos in the terminal, using asciinema.
-}
{-
TODO
mirror common asciinema flags - -s, -i at least
support other asciinema operations - cat
hledger.org hosting
embedded links to hledger.org player
windows/PowerSession support
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
import System.Exit (exitFailure)
import Text.Printf
import Control.Concurrent (threadDelay)
import System.Process (callProcess)
import System.IO.Error (catchIOError)
import Safe (readMay, atMay, headMay)
import Data.List (isPrefixOf, find, findIndex, isInfixOf)
import Control.Applicative ((<|>))
import Data.ByteString as B (ByteString)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)

demos :: [Demo]
demos = map readDemo [
  $(embedFileRelative "embeddedfiles/install.cast"),  -- https://asciinema.org/a/567934 Installing hledger from source with hledger-install
  $(embedFileRelative "embeddedfiles/help.cast"),     -- https://asciinema.org/a/568112 Getting help
  $(embedFileRelative "embeddedfiles/demo.cast"),     -- https://asciinema.org/a/567944 Watching the built-in demos
  $(embedFileRelative "embeddedfiles/add.cast"),      -- https://asciinema.org/a/567935 The easiest way to start a journal (add)
  $(embedFileRelative "embeddedfiles/print.cast"),    -- https://asciinema.org/a/567936 Show full transactions (print)
  $(embedFileRelative "embeddedfiles/balance.cast")   -- https://asciinema.org/a/567937 Show account balances and changes (balance)
  ]

-- | An embedded asciinema cast, with some of the metadata separated out.
-- The original file name is not preserved.
data Demo = Demo {
  dtitle    :: String,      -- asciinema title field
  _dcontent :: ByteString   -- asciinema v2 content
}

-- | Command line options for this command.
demomode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Demo.txt")
  []
  [generalflagsgroup3]
  []
  ([], Just $ argsFlag optsstr)

optsstr = "[NUM|PREFIX|SUBSTR] [-- ASCIINEMAOPTS]"
usagestr = "Usage: hledger demo " <> optsstr

-- | The demo command.
demo :: CliOpts -> Journal -> IO ()
demo CliOpts{rawopts_=rawopts, reportspec_=ReportSpec{_rsQuery=_query}} _j = do
  -- demos <- getCurrentDirectory >>= readDemos
  let args = listofstringopt "args" rawopts
  case args of
    [] -> putStrLn usagestr >> printDemos
    (a:as) ->
      case findDemo demos a of
        Nothing -> do
          putStrLn $ "No demo \"" <> a <> "\" was found."
          putStrLn usagestr
          printDemos
          exitFailure
        Just (Demo t c) -> do
          let i = maybe 0 (1+) $ findIndex (\(Demo t2 _) -> t2 == t) demos  -- should succeed
          mw <- getTerminalWidth
          let line = red' $ replicate w '.' where w = fromMaybe (length t) mw
          printf "playing: %d) %s\nspace to pause, . to step, ctrl-c to quit\n" i (bold' t)
          putStrLn line
          putStrLn ""
          threadDelay 1000000
          runAsciinemaPlay c as
          putStrLn ""
          putStrLn line

readDemo :: ByteString -> Demo
readDemo content = Demo title content
  where
    title = maybe "" (readTitle . B.unpack) $ headMay $ B.lines content
      where
        readTitle s
          | "\"title\":" `isPrefixOf` s = takeWhile (/='"') $ drop 1 $ lstrip $ drop 8 s
          | null s = ""
          | otherwise = readTitle $ tail s

findDemo :: [Demo] -> String -> Maybe Demo
findDemo ds s =
      (readMay s >>= atMay ds . subtract 1)         -- try to find by number
  <|> find ((sl `isPrefixOf`).lowercase.dtitle) ds  -- or by title prefix (ignoring case)
  <|> find ((sl `isInfixOf`) .lowercase.dtitle) ds  -- or by title substring (ignoring case)
  where
    sl = lowercase s

printDemos :: IO ()
printDemos = putStrLn $ unlines $
  "Demos:" :
  -- "" :
  [show i <> ") " <> bold' t | (i, Demo t _) <- zip [(1::Int)..] demos]

-- | Run asciinema play, passing content to its stdin.
runAsciinemaPlay :: ByteString -> [String] -> IO ()
runAsciinemaPlay content args =
  withSystemTempFile "hledger-cast" $ \f h -> do  -- try piping to stdin also
    B.hPutStrLn h content >> hClose h
    callProcess "asciinema" ("play" : f : args)
      `catchIOError` \err -> do
        putStrLn $ "There was a problem. Is asciinema installed ?\n" <> show err  --  (or PowerSession on Windows)
        exitFailure

