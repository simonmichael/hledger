{- | 

Convenient helpers for debug logging to stderr or a file.
The function names try to balance consistency, memorability, and ease of typing.
This module also exports Debug.Trace and the breakpoint package's Debug.Breakpoint.

The @dbgN*@ functions are intended to be added at points of interest in your code.
They will print labelled values to stderr, only if the program was run with a 
sufficiently high debug level. Debug level ranges from 0 (no output) to 9 (most output),
and is set by the @--debug[=N]@ command line option. (@--debug@ with no argument means 1).

The command line is parsed for --debug using unsafePerformIO, for easy use of these helpers
in existing code, or before normal command line parsing.
If you are working in GHCI, changing the debug level requires editing and reloading this file.
Sometimes it's more convenient to temporarily add dbg0's in your code and :reload.

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
  -- * Tracing to stderr
   debugLevel
  ,traceWith
  ,traceAt
  ,traceAtWith
  ,ptrace
  ,ptraceAt
  ,ptraceAtIO
  -- * Logging to a file
  -- ,debugLogLevel
  ,traceLog
  ,traceLogAt
  -- ,ptraceLogAt
  -- ,ptraceLogAtWith
  -- ,ptraceLogAtIO
  -- * Convenient pretty tracing in pure code
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
  -- * Convenient tracing with a show function
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
  -- * Convenient pretty tracing in IO
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
import Debug.Trace (trace)
import Safe (readDef)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)

import Hledger.Utils.Print (pshow)

-- XXX some of the below can be improved with pretty-simple, https://github.com/cdepillabout/pretty-simple#readme

-- | Trace a showable value with the given show function before returning it.
traceWith :: Show a => (a -> String) -> a -> a
traceWith f a = trace (f a) a

-- | Pretty-trace a showable value before returning it.
-- Like Debug.Trace.traceShowId, but pretty-printing and easier to type.
ptrace :: Show a => a -> a
ptrace = traceWith pshow

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
    | otherwise = \s a -> let ls = lines $ pshow a
                              nlorspace | length ls > 1 = "\n"
                                        | otherwise     = replicate (max 1 $ 11 - length s) ' '
                              ls' | length ls > 1 = map (' ':) ls
                                  | otherwise     = ls
                          in trace (s++":"++nlorspace++intercalate "\n" ls') a

-- | Like ptraceAt, but convenient to insert in an IO monad and
-- enforces monadic sequencing.
-- XXX These have a bug; they should use
-- traceIO, not trace, otherwise GHC can occasionally over-optimise
-- (cf lpaste a few days ago where it killed/blocked a child thread).
ptraceAtIO :: (MonadIO m, Show a) => Int -> String -> a -> m ()
ptraceAtIO lvl lbl x = liftIO $ ptraceAt lvl lbl x `seq` return ()

-- XXX separate file logging debug level and helpers - probably not needed
-- since you can just redirect stderr to a file, on unix at least.

-- -- | Global debug log level. Like debugLevel, but controls verbosity
-- -- of debug output logged to the debug log file.
-- -- {-# OPTIONS_GHC -fno-cse #-}
-- {-# NOINLINE debugLogLevel #-}
-- debugLogLevel :: Int
-- debugLogLevel = case dropWhile (/="--debug") args of
--                ["--debug-log"]   -> 1
--                "--debug-log":n:_ -> readDef 1 n
--                _             ->
--                  case take 1 $ filter ("--debug-log" `isPrefixOf`) args of
--                    ['-':'-':'d':'e':'b':'u':'g':'-':'l':'o':'g':'=':v] -> readDef 1 v
--                    _                                   -> 0
--     where
--       args = unsafePerformIO getArgs

-- | Log a string to ./debug.log before returning the second argument.
-- Uses unsafePerformIO.
-- {-# NOINLINE traceLog #-}
traceLog :: String -> a -> a
traceLog s x = unsafePerformIO $ do
  evaluate (force s)  -- to complete any previous logging before we attempt more
  appendFile "debug.log" (s ++ "\n")
  return x

-- | Log a string to ./debug.log before returning the second argument,
-- if the global debug level is at or above the specified level.
-- At level 0, always logs. Otherwise, uses unsafePerformIO.
traceLogAt :: Int -> String -> a -> a
traceLogAt level s
  | level > 0 && debugLevel < level = id
  | otherwise = traceLog s

-- -- | Pretty-log a label and showable value to ./debug.log,
-- -- if the global debug level is at or above the specified level.
-- -- At level 0, always prints. Otherwise, uses unsafePerformIO.
-- ptraceLogAt :: Show a => Int -> String -> a -> a
-- ptraceLogAt level
--   | level > 0 && debugLogLevel < level = const id
--   | otherwise = \lbl a ->
--     let 
--       ls = lines $ pshow' a
--       nlorspace | length ls > 1 = "\n"
--                 | otherwise     = replicate (max 1 $ 11 - length lbl) ' '
--       ls' | length ls > 1 = map (' ':) ls
--           | otherwise     = ls
--     in traceLog (lbl++":"++nlorspace++intercalate "\n" ls') a

-- -- | Like ptraceLogAt, but takes a custom show function instead of a label.
-- ptraceLogAtWith :: Show a => Int -> (a -> String) -> a -> a
-- ptraceLogAtWith level f
--     | level > 0 && debugLevel < level = id
--     | otherwise = \a -> let p = f a
--                             -- ls = lines p
--                             -- nlorspace | length ls > 1 = "\n"
--                             --           | otherwise     = " " ++ take (10 - length s) (repeat ' ')
--                             -- ls' | length ls > 1 = map (" "++) ls
--                             --     | otherwise     = ls
--                         -- in trace (s++":"++nlorspace++intercalate "\n" ls') a
--                         in trace p a

-- -- | Like ptraceAt, but convenient to insert in an IO monad and
-- -- enforces monadic sequencing.
-- -- XXX These have a bug; they should use
-- -- traceIO, not trace, otherwise GHC can occasionally over-optimise
-- -- (cf lpaste a few days ago where it killed/blocked a child thread).
-- ptraceLogAtIO :: (MonadIO m, Show a) => Int -> String -> a -> m ()
-- ptraceLogAtIO lvl lbl x = liftIO $ ptraceLogAt lvl lbl x `seq` return ()

-- | Pretty-trace and pretty-log a label and showable value
-- to stderr and the debug log, then return it.
dbg0 :: Show a => String -> a -> a
dbg0 = ptraceAt 0

-- | Pretty-trace a label and showable value to stderr if
-- --debug level is high enough,
-- and pretty-log to the debug log if --debug-log level is
-- high enough, then return the value.
-- Uses unsafePerformIO.
dbg1 :: Show a => String -> a -> a
dbg1 = ptraceAt 1

dbg2 :: Show a => String -> a -> a
dbg2 = ptraceAt 2

dbg3 :: Show a => String -> a -> a
dbg3 = ptraceAt 3

dbg4 :: Show a => String -> a -> a
dbg4 = ptraceAt 4

dbg5 :: Show a => String -> a -> a
dbg5 = ptraceAt 5

dbg6 :: Show a => String -> a -> a
dbg6 = ptraceAt 6

dbg7 :: Show a => String -> a -> a
dbg7 = ptraceAt 7

dbg8 :: Show a => String -> a -> a
dbg8 = ptraceAt 8

dbg9 :: Show a => String -> a -> a
dbg9 = ptraceAt 9

-- | Like dbg0, but also exit the program. Uses unsafePerformIO.
-- {-# NOINLINE dbgExit #-}
dbgExit :: Show a => String -> a -> a
dbgExit msg = const (unsafePerformIO exitFailure) . dbg0 msg

-- | Like dbgN, but taking a show function instead of a label.
dbg0With :: Show a => (a -> String) -> a -> a
dbg0With = traceAtWith 0

dbg1With :: Show a => (a -> String) -> a -> a
dbg1With = traceAtWith 1

dbg2With :: Show a => (a -> String) -> a -> a
dbg2With = traceAtWith 2

dbg3With :: Show a => (a -> String) -> a -> a
dbg3With = traceAtWith 3

dbg4With :: Show a => (a -> String) -> a -> a
dbg4With = traceAtWith 4

dbg5With :: Show a => (a -> String) -> a -> a
dbg5With = traceAtWith 5

dbg6With :: Show a => (a -> String) -> a -> a
dbg6With = traceAtWith 6

dbg7With :: Show a => (a -> String) -> a -> a
dbg7With = traceAtWith 7

dbg8With :: Show a => (a -> String) -> a -> a
dbg8With = traceAtWith 8

dbg9With :: Show a => (a -> String) -> a -> a
dbg9With = traceAtWith 9

-- | Like dbgN, but convenient to use in IO.
dbg0IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg0IO = ptraceAtIO 0

dbg1IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg1IO = ptraceAtIO 1

dbg2IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg2IO = ptraceAtIO 2

dbg3IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg3IO = ptraceAtIO 3

dbg4IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg4IO = ptraceAtIO 4

dbg5IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg5IO = ptraceAtIO 5

dbg6IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg6IO = ptraceAtIO 6

dbg7IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg7IO = ptraceAtIO 7

dbg8IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg8IO = ptraceAtIO 8

dbg9IO :: (MonadIO m, Show a) => String -> a -> m ()
dbg9IO = ptraceAtIO 9

