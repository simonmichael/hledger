{- | 

Here are fancier versions of Debug.Trace, with these features:

- unsafePerformIO-based for easy usage in pure code, IO code, and program startup code
- reasonably short and memorable function names
- pretty-printing haskell values, with or without colour, using pretty-simple
- enabling/disabling debug output with --debug
- multiple debug verbosity levels, from 1 to 9
- sending debug output to stderr or to a log file
- enabling logging based on program name

The basic "trace" functions print to stderr.
This debug output will be interleaved with the program's normal output, which can be
useful for understanding when code executes.

The "Log" functions log to a file instead.
The need for these is arguable, since a technically savvy user can redirect
stderr output to a log file, eg: @CMD 2>debug.log@.
But here is how they currently work:

The "traceLog" functions log to the program's debug log file,
which is @PROGNAME.log@ in the current directory,
where PROGNAME is the program name returned by @getProgName@.
When using this logging feature you should call @withProgName@ explicitly
at the start of your program to ensure a stable program name,
otherwise it can change to "<interactive>" eg when running in GHCI.
Eg: @main = withProgName "MYPROG" $ do ...@.

The "OrLog" functions can either print to stderr or log to a file.

- By default, they print to stderr.

- If the program name has been set (with @withProgName) to something ending with ".log", they log to that file instead.
  This can be useful for programs which should never print to stderr, eg TUI programs like hledger-ui.

The "At" functions produce output only when the program was run with a 
sufficiently high debug level, as set by a @--debug[=N]@ command line option.
N ranges from 1 (least debug output) to 9 (most debug output),
@--debug@ with no argument means 1.

The "dbgN*" functions are intended to be the most convenient API, to be embedded
at points of interest in your code. They combine the conditional output of "At",
the conditional logging of "OrLog", pretty printing, and short searchable function names.

Parsing the command line, detecting program name, and file logging is done with unsafePerformIO.
If you are working in GHCI, changing the debug level requires editing and reloading this file
(sometimes it's more convenient to add a dbg0 temporarily).

In hledger, debug levels are used as follows:

@
Debug level:  What to show:
------------  ---------------------------------------------------------
0             normal command output only (no warnings, eg)
1             useful warnings, most common troubleshooting info, eg valuation
2             common troubleshooting info, more detail
3             report options selection
4             report generation
5             report generation, more detail
6             input file reading
7             input file reading, more detail
8             command line parsing
9             any other rarely needed / more in-depth info
@

We don't yet have the ability to select debug output by topic. For now, here
are some standardish topic strings to search for in hledger debug messages:

acct
arg
budget
calc
csv
journalFinalise
multiBalanceReport
opts
precision
price
q
style
val

-}

-- Disabled until 0.1.2.0 is released with windows support:
--  This module also exports Debug.Trace and the breakpoint package's Debug.Breakpoint.

-- more:
-- http://hackage.haskell.org/packages/archive/TraceUtils/0.1.0.2/doc/html/Debug-TraceUtils.html
-- http://hackage.haskell.org/packages/archive/trace-call/0.1/doc/html/Debug-TraceCall.html
-- http://hackage.haskell.org/packages/archive/htrace/0.1/doc/html/Debug-HTrace.html
-- http://hackage.haskell.org/packages/archive/traced/2009.7.20/doc/html/Debug-Traced.html
-- https://hackage.haskell.org/package/debug
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hledger.Utils.Debug (

   debugLevel

  -- * Tracing to stderr
  ,traceWith
  ,traceAt
  ,traceAtWith
  ,ptrace
  ,ptraceAt
  ,ptraceAtIO

  -- * Logging to PROGNAME.log
  ,traceLog
  ,traceLogAt
  ,traceLogIO
  ,traceLogAtIO
  ,traceLogWith
  ,traceLogAtWith
  ,ptraceLogAt
  ,ptraceLogAtIO

  -- * Tracing or logging based on shouldLog
  ,traceOrLog
  ,traceOrLogAt
  ,ptraceOrLogAt
  ,ptraceOrLogAtIO
  ,traceOrLogAtWith

  -- * Pretty tracing/logging in pure code
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
  ,dbgExit

  -- * Pretty tracing/logging in IO
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

  -- * Tracing/logging with a show function
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

  -- * Utilities
  ,lbl_

  -- * Re-exports
  -- ,module Debug.Breakpoint
  ,module Debug.Trace

  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List hiding (uncons)
-- import Debug.Breakpoint
import Debug.Trace (trace, traceIO, traceShowId)
import Safe (readDef)
import System.Environment (getProgName)
import System.Exit (exitFailure)
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

-- | Trace a value with the given show function before returning it.
traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

-- | Pretty-trace a showable value before returning it.
-- Like Debug.Trace.traceShowId, but pretty-printing and easier to type.
ptrace :: Show a => a -> a
ptrace = traceWith pshow

-- | Trace (print to stderr) a string if the global debug level is at
-- or above the specified level. At level 0, always prints. Otherwise,
-- uses unsafePerformIO.
traceAt :: Int -> String -> a -> a
traceAt level
    | level > 0 && debugLevel < level = const id
    | otherwise = trace

-- | Trace (print to stderr) a showable value using a custom show function,
-- if the global debug level is at or above the specified level.
-- At level 0, always prints. Otherwise, uses unsafePerformIO.
traceAtWith :: Int -> (a -> String) -> a -> a
traceAtWith level f a = traceAt level (f a) a

-- | Pretty-print a label and a showable value to the console
-- if the global debug level is at or above the specified level.
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

-- | Like ptraceAt, but convenient to insert in an IO monad and
-- enforces monadic sequencing.
ptraceAtIO :: (MonadIO m, Show a) => Int -> String -> a -> m ()
ptraceAtIO level label a =
  if level > 0 && debugLevel < level
  then return ()
  else liftIO $ traceIO (labelledPretty True label a)

-- | Should the "trace or log" functions output to a file instead of stderr ?
-- True if the program name ends with ".log".
shouldLog :: Bool
shouldLog = ".log" `isSuffixOf` modifiedProgName

-- | The debug log file: PROGNAME.log in the current directory.
-- See modifiedProgName.
debugLogFile :: FilePath
debugLogFile = progName ++ ".log"

-- -- | The debug log file: debug.log in the current directory.
-- debugLogFile :: FilePath
-- debugLogFile = "debug.log"

-- | Log a string to the debug log before returning the second argument.
-- Uses unsafePerformIO.
traceLog :: String -> a -> a
traceLog s x = unsafePerformIO $ do
  evaluate (force s)  -- to complete any previous logging before we attempt more
  appendFile debugLogFile (s ++ "\n")
  return x

-- | Log a string to the debug log before returning the second argument,
-- if the global debug level is at or above the specified level.
-- At level 0, always logs. Otherwise, uses unsafePerformIO.
traceLogAt :: Int -> String -> a -> a
traceLogAt level str
  | level > 0 && debugLevel < level = id
  | otherwise = traceLog str

-- | Like traceLog but sequences properly in IO.
traceLogIO :: MonadIO m => String -> m ()
traceLogIO s = do
  liftIO $ evaluate (force s)  -- to complete any previous logging before we attempt more
  liftIO $ appendFile debugLogFile (s ++ "\n")

-- | Like traceLogAt, but convenient to use in IO.
traceLogAtIO :: MonadIO m => Int -> String -> m ()
traceLogAtIO level str
  | level > 0 && debugLevel < level = return ()
  | otherwise = traceLogIO str

-- | Log a value to the debug log with the given show function before returning it.
traceLogWith :: (a -> String) -> a -> a
traceLogWith f a = traceLog (f a) a

-- | Log a string to the debug log before returning the second argument,
-- if the global debug level is at or above the specified level.
-- At level 0, always logs. Otherwise, uses unsafePerformIO.
traceLogAtWith :: Int -> (a -> String) -> a -> a
traceLogAtWith level f a = traceLogAt level (f a) a

-- | Pretty-log a label and showable value to the debug log,
-- if the global debug level is at or above the specified level. 
-- At level 0, always prints. Otherwise, uses unsafePerformIO.
ptraceLogAt :: Show a => Int -> String -> a -> a
ptraceLogAt level
  | level > 0 && debugLevel < level = const id
  | otherwise = \lbl a -> traceLog (labelledPretty False lbl a) a

-- | Like ptraceAt, but convenient to insert in an IO monad and
-- enforces monadic sequencing.
ptraceLogAtIO :: (MonadIO m, Show a) => Int -> String -> a -> m ()
ptraceLogAtIO level label a =
  if level > 0 && debugLevel < level
  then return ()
  else traceLogIO (labelledPretty False label a)

-- | Trace or log a string depending on shouldLog,
-- before returning the second argument.
traceOrLog :: String -> a -> a
traceOrLog = if shouldLog then trace else traceLog

-- | Trace or log a string depending on shouldLog,
-- when global debug level is at or above the specified level,
-- before returning the second argument.
traceOrLogAt :: Int -> String -> a -> a
traceOrLogAt = if shouldLog then traceLogAt else traceAt

-- | Pretty-trace or log depending on shouldLog, when global debug level
-- is at or above the specified level.
ptraceOrLogAt :: Show a => Int -> String -> a -> a
ptraceOrLogAt = if shouldLog then ptraceLogAt else ptraceAt

-- | Like ptraceOrLogAt, but convenient in IO.
ptraceOrLogAtIO :: (MonadIO m, Show a) => Int -> String -> a -> m ()
ptraceOrLogAtIO = if shouldLog then ptraceLogAtIO else ptraceAtIO

-- | Trace or log, with a show function, depending on shouldLog.
traceOrLogAtWith :: Int -> (a -> String) -> a -> a
traceOrLogAtWith = if shouldLog then traceLogAtWith else traceAtWith

-- | Pretty-trace to stderr (or log to debug log) a label and showable value,
-- then return it.
dbg0 :: Show a => String -> a -> a
dbg0 = ptraceOrLogAt 0

-- | Pretty-trace to stderr (or log to debug log) a label and showable value
-- if the --debug level is high enough, then return the value.
-- Uses unsafePerformIO.
dbg1 :: Show a => String -> a -> a
dbg1 = ptraceOrLogAt 1

dbg2 :: Show a => String -> a -> a
dbg2 = ptraceOrLogAt 2

dbg3 :: Show a => String -> a -> a
dbg3 = ptraceOrLogAt 3

dbg4 :: Show a => String -> a -> a
dbg4 = ptraceOrLogAt 4

dbg5 :: Show a => String -> a -> a
dbg5 = ptraceOrLogAt 5

dbg6 :: Show a => String -> a -> a
dbg6 = ptraceOrLogAt 6

dbg7 :: Show a => String -> a -> a
dbg7 = ptraceOrLogAt 7

dbg8 :: Show a => String -> a -> a
dbg8 = ptraceOrLogAt 8

dbg9 :: Show a => String -> a -> a
dbg9 = ptraceOrLogAt 9

-- | Like dbg0, but also exit the program. Uses unsafePerformIO.
dbgExit :: Show a => String -> a -> a
dbgExit label a = unsafePerformIO $ dbg0IO label a >> exitFailure

-- | Like dbgN, but convenient to use in IO.
dbg0IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg0IO = ptraceOrLogAtIO 0

dbg1IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg1IO = ptraceOrLogAtIO 1

dbg2IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg2IO = ptraceOrLogAtIO 2

dbg3IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg3IO = ptraceOrLogAtIO 3

dbg4IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg4IO = ptraceOrLogAtIO 4

dbg5IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg5IO = ptraceOrLogAtIO 5

dbg6IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg6IO = ptraceOrLogAtIO 6

dbg7IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg7IO = ptraceOrLogAtIO 7

dbg8IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg8IO = ptraceOrLogAtIO 8

dbg9IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg9IO = ptraceOrLogAtIO 9

-- | Like dbgN, but taking a show function instead of a label.
dbg0With :: (a -> String) -> a -> a
dbg0With = traceOrLogAtWith 0

dbg1With :: Show a => (a -> String) -> a -> a
dbg1With = traceOrLogAtWith 1

dbg2With :: Show a => (a -> String) -> a -> a
dbg2With = traceOrLogAtWith 2

dbg3With :: Show a => (a -> String) -> a -> a
dbg3With = traceOrLogAtWith 3

dbg4With :: Show a => (a -> String) -> a -> a
dbg4With = traceOrLogAtWith 4

dbg5With :: Show a => (a -> String) -> a -> a
dbg5With = traceOrLogAtWith 5

dbg6With :: Show a => (a -> String) -> a -> a
dbg6With = traceOrLogAtWith 6

dbg7With :: Show a => (a -> String) -> a -> a
dbg7With = traceOrLogAtWith 7

dbg8With :: Show a => (a -> String) -> a -> a
dbg8With = traceOrLogAtWith 8

dbg9With :: Show a => (a -> String) -> a -> a
dbg9With = traceOrLogAtWith 9

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
--     traceOrLogAtWith level (lbl_ topic desc . showfn) val
-- {-# HLINT ignore "Redundant lambda" #-}
