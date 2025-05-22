{- | 

Here are fancier helpers built on Debug.Trace, with these features:

- short, memorable, greppable function names
- pretty-printing of haskell values, using pretty-simple
- optional ANSI colour
- enabling/disabling debug output with --debug
- debug output levels from 1 to 9, selected by --debug N option
- --debug detected with unsafePerformIO for easy use in pure/IO/startup code
- debug output can be logged instead (for TUI apps)

The "dbg*" functions can be inserted temporarily at points of interest in your code
while debugging; or embedded there permanently, to be activated by --debug [N] on the command line.
They are intended to be relatively easy to remember, search for, and use.

Parsing the command line, detecting the program name, and logging is done with unsafePerformIO,
allowing these helpers to be used anywhere, eg before command line parsing or in pure code.
If you are working in GHCI and want to change the debug level, you'll need to reload this module.

The meaning of debug levels is up to you. Eg hledger uses them as follows:

@
Debug level:  What to show:
------------  ---------------------------------------------------------
0             normal command output only (no warnings, eg)
1             useful warnings, most common troubleshooting info (config file args, valuation..)
2             common troubleshooting info, more detail
3             report options selection
4             report generation
5             report generation, more detail
6             input file reading
7             input file reading, more detail
8             command line parsing
9             any other rarely needed / more in-depth info
@

It's not yet possible to select debug output by topic; that would be useful.

-}

-- Disabled until 0.1.2.0 is released with windows support:
--  This module also exports Debug.Trace and the breakpoint package's Debug.Breakpoint.

-- more:
-- http://hackage.haskell.org/packages/archive/TraceUtils/0.1.0.2/doc/html/Debug-TraceUtils.html
-- http://hackage.haskell.org/packages/archive/trace-call/0.1/doc/html/Debug-TraceCall.html
-- http://hackage.haskell.org/packages/archive/htrace/0.1/doc/html/Debug-HTrace.html
-- http://hackage.haskell.org/packages/archive/traced/2009.7.20/doc/html/Debug-Traced.html
-- https://hackage.haskell.org/package/debug

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hledger.Utils.Debug (

  -- * The program's debug level, from 0 (least debug output) to 9 (most).
  -- This is parsed from a command line --debug N option, or --debug meaning 1.
  -- The command line is read (once) by unsafePerformIO, allowing this to be used
  -- easily anywhere in your program.
  debugLevel

  -- * Tracing to stderr
  -- These print to stderr.
  -- This output will be interleaved with the program's normal output,
  -- which can be helpful for understanding code execution.
  --
  -- ,traceWith
  -- ,traceAt
  -- ,traceAtWith
  -- ,ptrace
  -- ,ptraceAt
  -- ,ptraceAtIO

  -- * Logging to a log file
  -- These append to a PROGRAM.log file in the current directory.
  -- PROGRAM is normally the name of the executable, but it can change
  -- eg when running in GHCI. So when using these, you should call
  -- @withProgName@ to ensure a stable program name.
  -- Eg: @main = withProgName "PROGRAM" $ do ...@.
  --
  -- ,log'
  -- ,logAt
  -- ,logIO
  -- ,logAtIO
  -- ,logWith
  -- ,logAtWith
  -- ,plogAt
  -- ,plogAtIO

  -- All @dbg*@ functions normally trace to stderr,
  -- but they will log to PROGRAM.log instead if the (internal) program name ends with ".log".
  -- Eg: @main = withProgName "PROGRAM.log" $ do ...@.
  -- This is intended for TUI programs where stderr output is hard to see.
  --
  -- They have an effect only when the program's debug level is at or above the
  -- level specified by an argument or by the function name.
  -- The many variants follow a consistent pattern and aim to reduce typing and cognitive load.

  -- * Trace/log a string
  ,dbgMsg
  ,dbg0Msg
  ,dbg1Msg
  ,dbg2Msg
  ,dbg3Msg
  ,dbg4Msg
  ,dbg5Msg
  ,dbg6Msg
  ,dbg7Msg
  ,dbg8Msg
  ,dbg9Msg

  -- * In IO
  ,dbgMsgIO
  ,dbg0MsgIO
  ,dbg1MsgIO
  ,dbg2MsgIO
  ,dbg3MsgIO
  ,dbg4MsgIO
  ,dbg5MsgIO
  ,dbg6MsgIO
  ,dbg7MsgIO
  ,dbg8MsgIO
  ,dbg9MsgIO

  -- * Trace/log a showable value, pretty-printed
  -- @dbg@ here clashes with Text.Megaparsec.Debug (dbg), so that module or this one
  -- should be imported qualified if you are using both.
  ,dbg
  ,dbg0
  ,dbg1
  ,dbg2
  ,dbg3
  ,dbg4
  ,dbg5
  ,dbg6
  ,dbg7
  ,dbg8
  ,dbg9

  -- * In IO
  ,dbgIO
  ,dbg0IO
  ,dbg1IO
  ,dbg2IO
  ,dbg3IO
  ,dbg4IO
  ,dbg5IO
  ,dbg6IO
  ,dbg7IO
  ,dbg8IO
  ,dbg9IO

  -- * With a custom show function
  ,dbgWith
  ,dbg0With
  ,dbg1With
  ,dbg2With
  ,dbg3With
  ,dbg4With
  ,dbg5With
  ,dbg6With
  ,dbg7With
  ,dbg8With
  ,dbg9With

  -- * Utilities, ghc-debug
  ,ghcDebugSupportedInLib
  ,GhcDebugMode(..)
  ,ghcDebugMode
  ,withGhcDebug'
  ,ghcDebugPause'

  -- * Utilities, other
  ,lbl_
  ,progName

  -- * Re-exports
  -- ,module Debug.Breakpoint
  ,module Debug.Trace

  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List hiding (uncons)
import Debug.Trace (trace, traceIO, traceShowId)
#ifdef GHCDEBUG
import GHC.Debug.Stub (pause, withGhcDebug)
#endif
import Safe (readDef)
import System.Environment (getProgName)
-- import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)

import Hledger.Utils.IO (progArgs, pshow, pshow')

-- | The program name as returned by @getProgName@.
-- It's best to set this explicitly at program startup with @withProgName@,
-- otherwise when running in GHCI (eg) it will change to "<interactive>".
-- Setting it with a ".log" suffix causes some functions below
-- to log instead of trace.
{-# NOINLINE modifiedProgName #-}
modifiedProgName :: String
modifiedProgName = unsafePerformIO getProgName

-- | The progam name, with any ".log" suffix removed.
progName :: String
progName =
  if ".log" `isSuffixOf` modifiedProgName
  then reverse $ drop 4 $ reverse modifiedProgName
  else modifiedProgName

-- | The programs debug output verbosity. The default is 0 meaning no debug output.
-- The @--debug@ command line flag sets it to 1, or @--debug=N@ sets it to
-- a higher value (the = is required). Uses unsafePerformIO. 
-- When running in GHCI, changing this requires reloading this module.
debugLevel :: Int
debugLevel = case dropWhile (/="--debug") progArgs of
               ["--debug"]   -> 1
               "--debug":n:_ -> readDef 1 n
               _             ->
                 case take 1 $ filter ("--debug" `isPrefixOf`) progArgs of
                   ['-':'-':'d':'e':'b':'u':'g':'=':v] -> readDef 1 v
                   _                                   -> 0

-- | Whether ghc-debug support is included in this build, and if so, how it will behave.
-- When hledger is built with the @ghcdebug@ cabal flag (off by default, because of extra deps),
-- it can listen (on unix ?) for connections from ghc-debug clients like ghc-debug-brick,
-- for pausing/resuming the program and inspecting memory usage and profile information.
--
-- With a ghc-debug-supporting build, ghc-debug can be enabled by running hledger with
-- a negative --debug level. There are three different modes:
-- --debug=-1 - run normally (can be paused/resumed by a ghc-debug client),
-- --debug=-2 - pause and await client commands at program start (not useful currently),
-- --debug=-3 - pause and await client commands at program end.
data GhcDebugMode =
    GDNotSupported
  | GDDisabled
  | GDNoPause
  | GDPauseAtStart
  | GDPauseAtEnd
  -- keep synced with ghcDebugMode
  deriving (Eq,Ord,Show)

-- | Is the hledger-lib package built with ghc-debug support ?
ghcDebugSupportedInLib :: Bool
ghcDebugSupportedInLib =
#ifdef GHCDEBUG
  True
#else
  False
#endif

-- | Should the program open a socket allowing control by ghc-debug-brick or similar ghc-debug client ?
-- See GhcDebugMode.
ghcDebugMode :: GhcDebugMode
ghcDebugMode =
#ifdef GHCDEBUG
  case debugLevel of
    _ | not ghcDebugSupportedInLib -> GDNotSupported
    (-1) -> GDNoPause
    (-2) -> GDPauseAtStart
    (-3) -> GDPauseAtEnd
    _    -> GDDisabled
    -- keep synced with GhcDebugMode
#else
  GDNotSupported
#endif

-- | When ghc-debug support has been built into the program and enabled at runtime with --debug=-N,
-- this calls ghc-debug's withGhcDebug; otherwise it's a no-op.
withGhcDebug' =
#ifdef GHCDEBUG
  if ghcDebugMode > GDDisabled then withGhcDebug else id
#else
  id
#endif

-- | When ghc-debug support has been built into the program, this calls ghc-debug's pause, otherwise it's a no-op.
ghcDebugPause' :: IO ()
ghcDebugPause' =
#ifdef GHCDEBUG
  pause
#else
  return ()
#endif


-- | Trace (print to stderr) a string if the program debug level is at
-- or above the specified level. At level 0, always prints. Otherwise,
-- uses unsafePerformIO.
traceAt :: Int -> String -> a -> a
traceAt level
    | level > 0 && debugLevel < level = const id
    | otherwise = trace

-- | Like traceAt, but sequences properly in IO.
traceAtIO :: (MonadIO m) => Int -> String -> m ()
traceAtIO level msg =
  if level > 0 && debugLevel < level
  then return ()
  else liftIO $ traceIO msg

-- -- | Trace a value with the given show function before returning it.
-- traceWith :: (a -> String) -> a -> a
-- traceWith f a = trace (f a) a

-- | Trace (print to stderr) a showable value using a custom show function,
-- if the program debug level is at or above the specified level.
-- At level 0, always prints. Otherwise, uses unsafePerformIO.
traceAtWith :: Int -> (a -> String) -> a -> a
traceAtWith level f a = traceAt level (f a) a

-- -- | Pretty-trace a showable value before returning it.
-- -- Like Debug.Trace.traceShowId, but pretty-printing and easier to type.
-- ptrace :: Show a => a -> a
-- ptrace = traceWith pshow

-- | Pretty-print a label and a showable value to the console
-- if the program debug level is at or above the specified level.
-- At level 0, always prints. Otherwise, uses unsafePerformIO.
ptraceAt :: Show a => Int -> String -> a -> a
ptraceAt level
    | level > 0 && debugLevel < level = const id
    | otherwise = \lbl a -> trace (labelledPretty True lbl a) a

-- Pretty-print a showable value with a label, with or without allowing ANSI color.
labelledPretty :: Show a => Bool -> String -> a -> String
labelledPretty allowcolour lbl a = lbl ++ ":" ++ nlorspace ++ intercalate "\n" ls'
  where
    ls = lines $ (if allowcolour then pshow else pshow') a
    nlorspace | length ls > 1 = "\n"
              | otherwise     = replicate (max 1 $ 11 - length lbl) ' '
    ls' | length ls > 1 = map (' ':) ls
        | otherwise     = ls

-- | Like ptraceAt, but sequences properly in IO.
ptraceAtIO :: (MonadIO m, Show a) => Int -> String -> a -> m ()
ptraceAtIO level label a =
  if level > 0 && debugLevel < level
  then return ()
  else liftIO $ traceIO (labelledPretty True label a)


-- | The debug log file: PROGNAME.log in the current directory.
-- See modifiedProgName.
debugLogFile :: FilePath
debugLogFile = progName ++ ".log"

-- | Log a string to the debug log before returning the second argument.
-- Uses unsafePerformIO.
log' :: String -> a -> a
log' s x = unsafePerformIO $ do
  evaluate (force s)  -- to complete any previous logging before we attempt more
  appendFile debugLogFile (s ++ "\n")
  return x

-- | Log a string to the debug log before returning the second argument,
-- if the program debug level is at or above the specified level.
-- At level 0, always logs. Otherwise, uses unsafePerformIO.
logAt :: Int -> String -> a -> a
logAt level str
  | level > 0 && debugLevel < level = id
  | otherwise = log' str

-- | Like log' but sequences properly in IO.
logIO :: MonadIO m => String -> m ()
logIO s = do
  liftIO $ evaluate (force s)  -- to complete any previous logging before we attempt more
  liftIO $ appendFile debugLogFile (s ++ "\n")

-- | Like logAt, but convenient to use in IO.
logAtIO :: (MonadIO m) => Int -> String -> m ()
logAtIO level str
  | level > 0 && debugLevel < level = return ()
  | otherwise = logIO str

-- -- | Log a value to the debug log with the given show function before returning it.
-- logWith :: (a -> String) -> a -> a
-- logWith f a = log' (f a) a

-- | Log a string to the debug log before returning the second argument,
-- if the program debug level is at or above the specified level.
-- At level 0, always logs. Otherwise, uses unsafePerformIO.
logAtWith :: Int -> (a -> String) -> a -> a
logAtWith level f a = logAt level (f a) a

-- | Pretty-log a label and showable value to the debug log,
-- if the program debug level is at or above the specified level. 
-- At level 0, always prints. Otherwise, uses unsafePerformIO.
plogAt :: (Show a) => Int -> String -> a -> a
plogAt level
  | level > 0 && debugLevel < level = const id
  | otherwise = \lbl a -> log' (labelledPretty False lbl a) a

-- | Like ptraceAt, but sequences properly in IO.
plogAtIO :: (MonadIO m, Show a) => Int -> String -> a -> m ()
plogAtIO level label a =
  if level > 0 && debugLevel < level
  then return ()
  else logIO (labelledPretty False label a)


-- | Should dbg* log to a file instead of tracing to stderr ?
-- True if the (internal) program name ends with ".log".
shouldLog :: Bool
shouldLog = ".log" `isSuffixOf` modifiedProgName


-- | Trace or log a string if the program debug level is at or above the specified level,
-- then return the second argument.
dbgMsg :: Int -> String -> a -> a
dbgMsg = if shouldLog then logAt else traceAt

dbg0Msg :: String -> a -> a
dbg0Msg = dbgMsg 0

dbg1Msg :: String -> a -> a
dbg1Msg = dbgMsg 1

dbg2Msg :: String -> a -> a
dbg2Msg = dbgMsg 2

dbg3Msg :: String -> a -> a
dbg3Msg = dbgMsg 3

dbg4Msg :: String -> a -> a
dbg4Msg = dbgMsg 4

dbg5Msg :: String -> a -> a
dbg5Msg = dbgMsg 5

dbg6Msg :: String -> a -> a
dbg6Msg = dbgMsg 6

dbg7Msg :: String -> a -> a
dbg7Msg = dbgMsg 7

dbg8Msg :: String -> a -> a
dbg8Msg = dbgMsg 8

dbg9Msg :: String -> a -> a
dbg9Msg = dbgMsg 9


-- | Like dbgMsg, but sequences properly in IO.
dbgMsgIO :: (MonadIO m) => Int -> String -> m ()
dbgMsgIO = if shouldLog then logAtIO else traceAtIO

dbg0MsgIO :: (MonadIO m) => String -> m ()
dbg0MsgIO = dbgMsgIO 0

dbg1MsgIO :: (MonadIO m) => String -> m ()
dbg1MsgIO = dbgMsgIO 1

dbg2MsgIO :: (MonadIO m) => String -> m ()
dbg2MsgIO = dbgMsgIO 2

dbg3MsgIO :: (MonadIO m) => String -> m ()
dbg3MsgIO = dbgMsgIO 3

dbg4MsgIO :: (MonadIO m) => String -> m ()
dbg4MsgIO = dbgMsgIO 4

dbg5MsgIO :: (MonadIO m) => String -> m ()
dbg5MsgIO = dbgMsgIO 5

dbg6MsgIO :: (MonadIO m) => String -> m ()
dbg6MsgIO = dbgMsgIO 6

dbg7MsgIO :: (MonadIO m) => String -> m ()
dbg7MsgIO = dbgMsgIO 7

dbg8MsgIO :: (MonadIO m) => String -> m ()
dbg8MsgIO = dbgMsgIO 8

dbg9MsgIO :: (MonadIO m) => String -> m ()
dbg9MsgIO = dbgMsgIO 9


-- | Trace or log a label and showable value, pretty-printed,
-- if the program debug level is at or above the specified level;
-- then return the value.
-- Traces to stderr or logs to a file depending on shouldLog.
dbg :: (Show a) => Int -> String -> a -> a
dbg = if shouldLog then plogAt else ptraceAt

dbg0 :: Show a => String -> a -> a
dbg0 = dbg 0

dbg1 :: Show a => String -> a -> a
dbg1 = dbg 1

dbg2 :: Show a => String -> a -> a
dbg2 = dbg 2

dbg3 :: Show a => String -> a -> a
dbg3 = dbg 3

dbg4 :: Show a => String -> a -> a
dbg4 = dbg 4

dbg5 :: Show a => String -> a -> a
dbg5 = dbg 5

dbg6 :: Show a => String -> a -> a
dbg6 = dbg 6

dbg7 :: Show a => String -> a -> a
dbg7 = dbg 7

dbg8 :: Show a => String -> a -> a
dbg8 = dbg 8

dbg9 :: Show a => String -> a -> a
dbg9 = dbg 9


-- | Like dbg, but sequences properly in IO.
dbgIO :: (MonadIO m, Show a) => Int -> String -> a -> m ()
dbgIO = if shouldLog then plogAtIO else ptraceAtIO

dbg0IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg0IO = dbgIO 0

dbg1IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg1IO = dbgIO 1

dbg2IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg2IO = dbgIO 2

dbg3IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg3IO = dbgIO 3

dbg4IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg4IO = dbgIO 4

dbg5IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg5IO = dbgIO 5

dbg6IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg6IO = dbgIO 6

dbg7IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg7IO = dbgIO 7

dbg8IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg8IO = dbgIO 8

dbg9IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg9IO = dbgIO 9


-- | Like dbgWith, but with a custom show function.
dbgWith :: Int -> (a -> String) -> a -> a
dbgWith = if shouldLog then logAtWith else traceAtWith

-- | Like dbgN, but taking a show function instead of a label.
dbg0With :: (a -> String) -> a -> a
dbg0With = dbgWith 0

dbg1With :: Show a => (a -> String) -> a -> a
dbg1With = dbgWith 1

dbg2With :: Show a => (a -> String) -> a -> a
dbg2With = dbgWith 2

dbg3With :: Show a => (a -> String) -> a -> a
dbg3With = dbgWith 3

dbg4With :: Show a => (a -> String) -> a -> a
dbg4With = dbgWith 4

dbg5With :: Show a => (a -> String) -> a -> a
dbg5With = dbgWith 5

dbg6With :: Show a => (a -> String) -> a -> a
dbg6With = dbgWith 6

dbg7With :: Show a => (a -> String) -> a -> a
dbg7With = dbgWith 7

dbg8With :: Show a => (a -> String) -> a -> a
dbg8With = dbgWith 8

dbg9With :: Show a => (a -> String) -> a -> a
dbg9With = dbgWith 9

-- | Helper for producing debug messages:
-- concatenates a name (eg a function name),
-- short description of the value being logged,
-- and string representation of the value.
lbl_ :: String -> String -> String -> String
lbl_ name desc val = name <> ": " <> desc <> ":" <> " " <> val

-- XXX the resulting function is constrained to only one value type
-- -- | A new helper for defining a local "dbg" function.
-- -- Given a debug level and a topic string (eg, a function name),
-- -- it generates a function which takes
-- -- - a description string,
-- -- - a value-to-string show function,
-- -- - and a value to be inspected,
-- -- debug-logs the topic, description and result of calling the show function on the value,
-- -- formatted nicely, at the specified debug level or above,
-- -- then returns the value.
-- dbg_ :: forall a. Show a => Int -> String -> (String -> (a -> String) -> a -> a)
-- dbg_ level topic =
--   \desc showfn val ->
--     dbgWith level (lbl_ topic desc . showfn) val
-- {-# HLINT ignore "Redundant lambda" #-}
