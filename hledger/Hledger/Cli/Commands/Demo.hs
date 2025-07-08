{-|
The @demo@ command lists and plays small hledger demos in the terminal, using asciinema.
-}
{-
PROJECTS
improve cast output
 install
  command line editing glitches
  shrink / compress ?
 help
  screen corrupted by pager
 demo
  update (or drop till stable)
 add
 print
 balance
document cast production tips
 always clear screen after running pager/curses apps ?
 record with tall window to avoid showing pager in playback ?
improve functionality
 show "done" in final red line ?
 mirror common asciinema flags like -s, -i and/or set speed/max idle with optional arguments
 support other asciinema operations (cat)
 show hledger.org player urls
 windows/PowerSession support
 attract/continuous play mode
more casts
 clarify goals/target user(s)/scenarios
 identify and prioritise some casts needed
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Hledger.Cli.Commands.Demo (
  demomode
 ,demo
) where

import Text.Printf
import Control.Concurrent (threadDelay)
import System.Process (callProcess)
import System.IO.Error (catchIOError)
import Safe (readMay, atMay, headMay)
import Data.List (isPrefixOf, find, findIndex, isInfixOf, dropWhileEnd)
import Control.Applicative ((<|>))
import Data.ByteString as B (ByteString)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Safe (tailMay)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import System.Console.CmdArgs.Explicit (flagReq)

import Hledger
import Hledger.Cli.CliOptions
import System.Directory (findExecutable)
import Control.Monad (when)

demos :: [Demo]
demos = map readDemo [
  -- XXX these are confusing, redo
  -- (embedFileRelative "embeddedfiles/help.cast"),     -- https://asciinema.org/a/568112 Getting help
  -- (embedFileRelative "embeddedfiles/demo.cast"),     -- https://asciinema.org/a/567944 Watching the built-in demos
  $(embedFileRelative "embeddedfiles/add.cast"),      -- https://asciinema.org/a/567935 The easiest way to start a journal (add)
  $(embedFileRelative "embeddedfiles/print.cast"),    -- https://asciinema.org/a/567936 Show full transactions (print)
  $(embedFileRelative "embeddedfiles/balance.cast"),   -- https://asciinema.org/a/567937 Show account balances and changes (balance)
  $(embedFileRelative "embeddedfiles/install.cast")  -- https://asciinema.org/a/567934 Installing hledger from source with hledger-install
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
  [
   flagReq  ["speed","s"] (\s opts -> Right $ setopt "speed" s opts) "SPEED"
    ("playback speed (1 is original speed, .5 is half, 2 is double, etc (default: 2))")
  ]
  [generalflagsgroup3]
  []
  ([], Just $ argsFlag optsstr)

optsstr = "[NUM|PREFIX|SUBSTR]"
-- optsstr = "[NUM|PREFIX|SUBSTR] [-- ASCIINEMAOPTS]"
usagestr = "Usage: hledger demo " <> optsstr

-- | The demo command.
demo :: CliOpts -> Journal -> IO ()
demo CliOpts{rawopts_=rawopts, reportspec_=ReportSpec{_rsQuery=_query}} _j = do
  -- demos <- getCurrentDirectory >>= readDemos
  case listofstringopt "args" rawopts of
    [] -> putStrLn usagestr >> putStr listDemos
    (a:as) ->
      case findDemo demos a of
        Nothing -> error' $ unlines
          ["No demo \"" <> a <> "\" was found."
          ,usagestr
          ,listDemos
          ]
        Just (Demo t c) -> do
          -- check if asciinema is installed, first
          masciinema <- findExecutable "asciinema"
          when (isNothing masciinema) $ error' "Could not find 'asciinema'; please install that first."
          let
            -- try to preserve the original pauses a bit while also moving things along
            defidlelimit = 10
            defspeed     = 2
            speed =
              case maybestringopt "speed" rawopts of
                Nothing -> defspeed
                Just s -> fromMaybe err $ readMay s
                  where err = error' $ "could not parse --speed " <> s <> ", numeric argument expected"
            idx = maybe 0 (1+) $ findIndex (\(Demo t2 _) -> t2 == t) demos  -- should succeed
          mw <- getTerminalWidth
          let line = red' $ replicate w '.' where w = fromMaybe (length t) mw
          printf "playing: %d) %s\nspace to pause, . to step, ctrl-c to quit\n" idx (bold' t)
          putStrLn line
          putStrLn ""
          threadDelay 1000000
          -- XXX this used to see asciinema options after --, currently it doesn't
          runAsciinemaPlay speed defidlelimit c as
          putStrLn ""
          putStrLn line

readDemo :: ByteString -> Demo
readDemo content = Demo title content
  where
    title = maybe "" (readTitle . B.unpack) $ headMay $ B.lines content
      where
        readTitle s
          | "\"title\":" `isPrefixOf` s = takeWhile (/='"') $ drop 1 $ lstrip $ drop 8 s
          | otherwise = maybe "" readTitle $ tailMay s

findDemo :: [Demo] -> String -> Maybe Demo
findDemo ds s =
      (readMay s >>= atMay ds . subtract 1)         -- try to find by number
  <|> find ((sl `isPrefixOf`).lowercase.dtitle) ds  -- or by title prefix (ignoring case)
  <|> find ((sl `isInfixOf`) .lowercase.dtitle) ds  -- or by title substring (ignoring case)
  where
    sl = lowercase s

listDemos :: String
listDemos = unlines $
  "Demos:" :
  -- "" :
  [show i <> ") " <> bold' t | (i, Demo t _) <- zip [(1::Int)..] demos]

-- | Run asciinema play with the given speed and idle limit, passing the given content to its stdin.
runAsciinemaPlay :: Float -> Float -> ByteString -> [String] -> IO ()
runAsciinemaPlay speed idlelimit content args = do
  -- XXX try piping to stdin also
  withSystemTempFile "hledger-cast" $ \f h -> do
    -- don't add an extra newline here, it breaks asciinema 2.3.0 (#2094).
    -- XXX we could try harder and strip excess newlines/carriage returns+linefeeds here
    B.hPutStr h content >> hClose h
    callProcess "asciinema" (dbg8With (("asciinema: "++).unwords) $ concat [
       ["play"]
      ,["-s"<> showwithouttrailingzero speed]
      ,if idlelimit == 0 then [] else ["-i"<>showwithouttrailingzero idlelimit]
      ,[f]
      ,args
      ])
    `catchIOError` \err -> do
      printError $ unlines
        [""
        ,show err
        ,"Running asciinema failed. Trying 'asciinema --version':"
        ]
      callProcess "asciinema" ["--version"]
      `catchIOError` \_ -> error' "This also failed."
  where
    showwithouttrailingzero = dropWhileEnd (=='.') . dropWhileEnd (=='0') . show

