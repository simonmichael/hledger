{- | 

Helpers for debug logging to console or file.
This module also exports Debug.Trace and (from the breakpoint package) Debug.Breakpoint.
Uses Hledger.Utils.Print. See also additional helpers in Hledger.Utils.Parse,
Hledger.UI.UIUtils etc.

@dbg0@-@dbg9@ will pretty-print values to stderr
if the program was run with a sufficiently high @--debug=N@ argument. 
(@--debug@ with no argument means @--debug=1@; @dbg0@ always prints).

Uses unsafePerformIO for simple program-wide read-only access to the debug level
set by the --debug command-line flag. The @debugLevel@ global is set once at startup,
so in GHCI if you want to change it you must save this file and :reload.
(Sometimes it's more convenient to temporarily add dbg0's in your code and :reload.)

Debug level is a number from 1 (least output) to 9 (most output).
In hledger, debug levels are used as follows:

Debug level:  What to show:
------------  ---------------------------------------------------------
0             normal command output only (no warnings, eg)
1 (--debug)   useful warnings, most common troubleshooting info, eg valuation
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
  -- * Tracing
   traceWith
  -- * Pretty tracing
  ,ptrace
  -- ** Debug-level-aware tracing
  ,debugLevel
  ,traceAt
  ,traceAtWith
  ,ptraceAt
  ,ptraceAtWith
  -- ** Easiest form (recommended)
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
  -- ** More control
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
  -- ** For standalone lines in IO blocks
  ,ptraceAtIO
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
  -- ** Debug-logging to a file
  ,dlogTrace
  ,dlogTraceAt
  ,dlogAt
  ,dlog0
  ,dlog1
  ,dlog2
  ,dlog3
  ,dlog4
  ,dlog5
  ,dlog6
  ,dlog7
  ,dlog8
  ,dlog9
  -- ** Re-exports
  ,module Debug.Breakpoint
  ,module Debug.Trace
  )
where

import Control.DeepSeq (force)
import Control.Monad.IO.Class
import Data.List hiding (uncons)
import Debug.Breakpoint
import Debug.Trace
import Safe (readDef)
import System.Environment (getArgs)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (evaluate)

-- import Hledger.Utils.Parse
import Hledger.Utils.Print
-- import Text.Megaparsec (MonadParsec)

-- XXX some of the below can be improved with pretty-simple, https://github.com/cdepillabout/pretty-simple#readme

-- | Pretty trace. Easier alias for traceShowId + pShow.
ptrace :: Show a => a -> a
ptrace = traceWith pshow

-- | Like traceShowId, but uses a custom show function to render the value.
-- traceShowIdWith was too much of a mouthful.
traceWith :: Show a => (a -> String) -> a -> a
traceWith f a = trace (f a) a

-- | Global debug level, which controls the verbosity of debug errput
-- on the console. The default is 0 meaning no debug errput. The
-- @--debug@ command line flag sets it to 1, or @--debug=N@ sets it to
-- a higher value (note: not @--debug N@ for some reason).  This uses
-- unsafePerformIO and can be accessed from anywhere and before normal
-- command-line processing. When running with :main in GHCI, you must
-- touch and reload this module to see the effect of a new --debug option.
-- {-# OPTIONS_GHC -fno-cse #-}
{-# NOINLINE debugLevel #-}
-- Avoid using dbg* in this function (infinite loop).
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

-- | Like ptraceAt, but takes a custom show function instead of a label.
ptraceAtWith :: Show a => Int -> (a -> String) -> a -> a
ptraceAtWith level f
    | level > 0 && debugLevel < level = id
    | otherwise = \a -> let p = f a
                            -- ls = lines p
                            -- nlorspace | length ls > 1 = "\n"
                            --           | otherwise     = " " ++ take (10 - length s) (repeat ' ')
                            -- ls' | length ls > 1 = map (" "++) ls
                            --     | otherwise     = ls
                        -- in trace (s++":"++nlorspace++intercalate "\n" ls') a
                        in trace p a

-- "dbg" would clash with megaparsec.
-- | Pretty-print a label and the showable value to the console, then return it.
dbg0 :: Show a => String -> a -> a
dbg0 = ptraceAt 0

-- | Pretty-print a label and the showable value to the console when the global debug level is >= 1, then return it.
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

-- | Like dbg0, but takes a custom show function instead of a label.
dbg0With :: Show a => (a -> String) -> a -> a
dbg0With = ptraceAtWith 0

dbg1With :: Show a => (a -> String) -> a -> a
dbg1With = ptraceAtWith 1

dbg2With :: Show a => (a -> String) -> a -> a
dbg2With = ptraceAtWith 2

dbg3With :: Show a => (a -> String) -> a -> a
dbg3With = ptraceAtWith 3

dbg4With :: Show a => (a -> String) -> a -> a
dbg4With = ptraceAtWith 4

dbg5With :: Show a => (a -> String) -> a -> a
dbg5With = ptraceAtWith 5

dbg6With :: Show a => (a -> String) -> a -> a
dbg6With = ptraceAtWith 6

dbg7With :: Show a => (a -> String) -> a -> a
dbg7With = ptraceAtWith 7

dbg8With :: Show a => (a -> String) -> a -> a
dbg8With = ptraceAtWith 8

dbg9With :: Show a => (a -> String) -> a -> a
dbg9With = ptraceAtWith 9

-- | Like ptraceAt, but convenient to insert in an IO monad and
-- enforces monadic sequencing (plus convenience aliases).
-- XXX These have a bug; they should use
-- traceIO, not trace, otherwise GHC can occasionally over-optimise
-- (cf lpaste a few days ago where it killed/blocked a child thread).
ptraceAtIO :: (MonadIO m, Show a) => Int -> String -> a -> m ()
ptraceAtIO lvl lbl x = liftIO $ ptraceAt lvl lbl x `seq` return ()

-- XXX Could not deduce (a ~ ())
-- ptraceAtM :: (Monad m, Show a) => Int -> String -> a -> m a
-- ptraceAtM lvl lbl x = ptraceAt lvl lbl x `seq` return x

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

-- | Log a string to ./debug.log before returning the second argument.
-- Uses unsafePerformIO.
-- {-# NOINLINE dlogTrace #-}
dlogTrace :: String -> a -> a
dlogTrace s x = unsafePerformIO $ do
  evaluate (force s)  -- to complete any previous logging before we attempt more
  appendFile "debug.log" (s ++ "\n")
  return x

-- | Log a string to ./debug.log before returning the second argument,
-- if the global debug level is at or above the specified level.
-- At level 0, always logs. Otherwise, uses unsafePerformIO.
dlogTraceAt :: Int -> String -> a -> a
dlogTraceAt level s
  | level > 0 && debugLevel < level = id
  | otherwise = dlogTrace s

-- | Log and pretty-print a label and showable value to "./debug.log",
-- if the global debug level is at or above the specified level.
-- At level 0, always prints. Otherwise, uses unsafePerformIO.
dlogAt :: Show a => Int -> String -> a -> a
dlogAt level
  | level > 0 && debugLevel < level = const id
  | otherwise = \lbl a ->
    let 
      ls = lines $ pshow' a
      nlorspace | length ls > 1 = "\n"
                | otherwise     = replicate (max 1 $ 11 - length lbl) ' '
      ls' | length ls > 1 = map (' ':) ls
          | otherwise     = ls
    in dlogTrace (lbl++":"++nlorspace++intercalate "\n" ls') a

-- | Pretty-print a label and the showable value to ./debug.log if at or above
-- a certain debug level, then return it.
dlog0 :: Show a => String -> a -> a
dlog0 = dlogAt 0

dlog1 :: Show a => String -> a -> a
dlog1 = dlogAt 1

dlog2 :: Show a => String -> a -> a
dlog2 = dlogAt 2

dlog3 :: Show a => String -> a -> a
dlog3 = dlogAt 3

dlog4 :: Show a => String -> a -> a
dlog4 = dlogAt 4

dlog5 :: Show a => String -> a -> a
dlog5 = dlogAt 5

dlog6 :: Show a => String -> a -> a
dlog6 = dlogAt 6

dlog7 :: Show a => String -> a -> a
dlog7 = dlogAt 7

dlog8 :: Show a => String -> a -> a
dlog8 = dlogAt 8

dlog9 :: Show a => String -> a -> a
dlog9 = dlogAt 9
