{- | 

Here are fancier versions of Debug.Trace, with these features:

- pretty-printing haskell values, with or without colour, using pretty-simple
- enabling/disabling debug output with --debug
- multiple debug verbosity levels, from 1 to 9
- sending debug output to stderr or to a log file
- enabling logging based on program name
- reasonably short and memorable function names
- easy usage in pure code, IO code, and program startup code.

This module also exports Debug.Trace and the breakpoint package's Debug.Breakpoint.

The "trace" functions print to stderr.
This debug output will be interleaved with the program's normal output, which can be
useful for understanding when code executes.
On most systems you can redirect stderr to a log file if you prefer (eg: @CMD 2>debug.log@).

"traceLog" functions log to the program's debug log file.
That is @PROGNAME.log@ in the current directory,
where PROGNAME is  the executable name returned by @getProgName@.
If using the logging feature you should ensure a stable program name
by setting it explicitly with @withProgName@ at the start of your program
(since otherwise it will change to "<interactive>" when you are testing in GHCI).
Eg:
@main = withProgName "MYPROG" $ do ...@.

The "traceOrLog" and "dbg" functions normally print to stderr, but if the program name
has been set to "MYPROG,logging" (ie, with a ",logging" suffix), they will log to
MYPROG.log instead. This is useful eg for TUI programs (hledger-ui does this).

The "dbgN*" functions are intended to be added at points of interest in your code.
They (and the "*At*" functions) produce output only if the program was run with a 
sufficiently high debug level. This ranges from 0 (no debug output) to 9 (most debug output),
and it is set by the @--debug[=N]@ command line option. (@--debug@ with no argument means 1).

Parsing the command line for --debug, detecting program name, and file logging is done with unsafePerformIO.
If you are working in GHCI, changing the debug level requires editing and reloading this file
(sometimes it's more convenient to add a dbg0 temporarily).

In hledger, debug levels are used as follows:

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

-}

-- more:
-- http://hackage.haskell.org/packages/archive/TraceUtils/0.1.0.2/doc/html/Debug-TraceUtils.html
-- http://hackage.haskell.org/packages/archive/trace-call/0.1/doc/html/Debug-TraceCall.html
-- http://hackage.haskell.org/packages/archive/htrace/0.1/doc/html/Debug-HTrace.html
-- http://hackage.haskell.org/packages/archive/traced/2009.7.20/doc/html/Debug-Traced.html
-- https://hackage.haskell.org/package/debug

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

  -- * Re-exports
  ,module Debug.Breakpoint
  ,module Debug.Trace

  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List hiding (uncons)
import Debug.Breakpoint
import Debug.Trace (trace, traceIO, traceShowId)
import Safe (readDef)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)

import Hledger.Utils.Print (pshow, pshow')

-- | Global debug output level. This is the requested verbosity of
-- debug output printed to stderr. The default is 0 meaning no debug output.
-- The @--debug@ command line flag sets it to 1, or @--debug=N@ sets it to
-- a higher value (note: not @--debug N@ for some reason).  This uses
-- unsafePerformIO and can be accessed from anywhere and before normal
-- command-line processing. When running with :main in GHCI, you must
-- touch and reload this module to see the effect of a new --debug option.
-- {-# OPTIONS_GHC -fno-cse #-}
{-# NOINLINE debugLevel #-}
debugLevel :: Int
debugLevel = case dropWhile (/="--debug") args of
               ["--debug"]   -> 1
               "--debug":n:_ -> readDef 1 n
               _             ->
                 case take 1 $ filter ("--debug" `isPrefixOf`) args of
                   ['-':'-':'d':'e':'b':'u':'g':'=':v] -> readDef 1 v
                   _                                   -> 0
    where
      args = unsafePerformIO getArgs

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

-- | The program name, possibly ending with ",logging".
-- This should be set at program startup with @withProgName@,
-- otherwise it will vary, eg "<interactive>" in GHCI.
{-# NOINLINE modifiedProgName #-}
modifiedProgName :: String
modifiedProgName = unsafePerformIO getProgName

-- | Should the "trace or log" functions output to a file instead of stderr ?
-- True if the program name ends with ",logging".
shouldLog :: Bool
shouldLog = ",logging" `isSuffixOf` modifiedProgName

-- | The progam name, with any ",logging" suffix removed.
progName :: String
progName =
  if ",logging" `isSuffixOf` modifiedProgName
  then reverse $ drop 8 $ reverse modifiedProgName
  else modifiedProgName

-- | The debug log file: PROGNAME.log in the current directory.
-- See modifiedProgName.
debugLogFile :: FilePath
debugLogFile = progName ++ ".log"

-- -- | The debug log file: debug.log in the current directory.
-- debugLogFile :: FilePath
-- debugLogFile = "debug.log"

-- | Log a string to the debug log before returning the second argument.
-- Uses unsafePerformIO.
{-# NOINLINE traceLog #-}
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
  else return $ traceLog (labelledPretty False label a) ()

-- Trace or log a string depending on shouldLog,
-- before returning the second argument.
traceOrLog :: String -> a -> a
traceOrLog = if shouldLog then trace else traceLog

-- Trace or log a string depending on shouldLog,
-- when global debug level is at or above the specified level,
-- before returning the second argument.
traceOrLogAt :: Int -> String -> a -> a
traceOrLogAt = if shouldLog then traceLogAt else traceAt

-- Pretty-trace or log depending on shouldLog, when global debug level
-- is at or above the specified level.
ptraceOrLogAt :: Show a => Int -> String -> a -> a
ptraceOrLogAt = if shouldLog then ptraceLogAt else ptraceAt

-- Like ptraceOrLogAt, but convenient in IO.
ptraceOrLogAtIO :: (MonadIO m, Show a) => Int -> String -> a -> m ()
ptraceOrLogAtIO = if shouldLog then ptraceLogAtIO else ptraceAtIO

-- Trace or log, with a show function, depending on shouldLog.
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
{-# NOINLINE dbgExit #-}
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

