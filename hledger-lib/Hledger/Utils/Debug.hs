{-# LANGUAGE CPP, FlexibleContexts #-}
-- | Debugging helpers

-- more:
-- http://hackage.haskell.org/packages/archive/TraceUtils/0.1.0.2/doc/html/Debug-TraceUtils.html
-- http://hackage.haskell.org/packages/archive/trace-call/0.1/doc/html/Debug-TraceCall.html
-- http://hackage.haskell.org/packages/archive/htrace/0.1/doc/html/Debug-HTrace.html
-- http://hackage.haskell.org/packages/archive/traced/2009.7.20/doc/html/Debug-Traced.html

module Hledger.Utils.Debug (
  module Hledger.Utils.Debug
  ,module Debug.Trace
#if __GLASGOW_HASKELL__ >= 704
  ,ppShow
#endif
)
where

import Control.Monad (when)
import Data.List
import Debug.Trace
import Safe (readDef)
import System.Environment (getArgs)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Text.Printf

#if __GLASGOW_HASKELL__ >= 704
import Text.Show.Pretty (ppShow)
#else
-- the required pretty-show version requires GHC >= 7.4
ppShow :: Show a => a -> String
ppShow = show
#endif


-- | Trace (print on stdout at runtime) a showable value.
-- (for easily tracing in the middle of a complex expression)
strace :: Show a => a -> a
strace a = trace (show a) a

-- | Labelled trace - like strace, with a label prepended.
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a

-- | Monadic trace - like strace, but works as a standalone line in a monad.
mtrace :: (Monad m, Show a) => a -> m a
mtrace a = strace a `seq` return a

-- | Custom trace - like strace, with a custom show function.
traceWith :: (a -> String) -> a -> a
traceWith f e = trace (f e) e

-- | Parsec trace - show the current parsec position and next input,
-- and the provided label if it's non-null.
ptrace :: Stream [Char] m t => String -> ParsecT [Char] st m ()
ptrace msg = do
  pos <- getPosition
  next <- take peeklength `fmap` getInput
  let (l,c) = (sourceLine pos, sourceColumn pos)
      s  = printf "at line %2d col %2d: %s" l c (show next) :: String
      s' = printf ("%-"++show (peeklength+30)++"s") s ++ " " ++ msg
  trace s' $ return ()
  where
    peeklength = 30

-- | Global debug level, which controls the verbosity of debug output
-- on the console. The default is 0 meaning no debug output. The
-- @--debug@ command line flag sets it to 1, or @--debug=N@ sets it to
-- a higher value (note: not @--debug N@ for some reason).  This uses
-- unsafePerformIO and can be accessed from anywhere and before normal
-- command-line processing. After command-line processing, it is also
-- available as the @debug_@ field of 'Hledger.Cli.Options.CliOpts'.
-- {-# OPTIONS_GHC -fno-cse #-} 
-- {-# NOINLINE debugLevel #-}
debugLevel :: Int
debugLevel = case snd $ break (=="--debug") args of
               "--debug":[]  -> 1
               "--debug":n:_ -> readDef 1 n
               _             ->
                 case take 1 $ filter ("--debug" `isPrefixOf`) args of
                   ['-':'-':'d':'e':'b':'u':'g':'=':v] -> readDef 1 v
                   _                                   -> 0

    where
      args = unsafePerformIO getArgs

-- | Convenience aliases for tracePrettyAt.
-- Pretty-print a message and the showable value to the console, then return it.
dbg :: Show a => String -> a -> a
dbg = tracePrettyAt 0

-- | Pretty-print a message and the showable value to the console when the debug level is >= 1, then return it. Uses unsafePerformIO.
dbg1 :: Show a => String -> a -> a
dbg1 = tracePrettyAt 1

dbg2 :: Show a => String -> a -> a
dbg2 = tracePrettyAt 2

dbg3 :: Show a => String -> a -> a
dbg3 = tracePrettyAt 3

dbg4 :: Show a => String -> a -> a
dbg4 = tracePrettyAt 4

dbg5 :: Show a => String -> a -> a
dbg5 = tracePrettyAt 5

dbg6 :: Show a => String -> a -> a
dbg6 = tracePrettyAt 6

dbg7 :: Show a => String -> a -> a
dbg7 = tracePrettyAt 7

dbg8 :: Show a => String -> a -> a
dbg8 = tracePrettyAt 8

dbg9 :: Show a => String -> a -> a
dbg9 = tracePrettyAt 9

-- | Convenience aliases for tracePrettyAtIO.
-- Like dbg, but convenient to insert in an IO monad.
dbgIO :: Show a => String -> a -> IO ()
dbgIO = tracePrettyAtIO 0

dbg1IO :: Show a => String -> a -> IO ()
dbg1IO = tracePrettyAtIO 1

dbg2IO :: Show a => String -> a -> IO ()
dbg2IO = tracePrettyAtIO 2

dbg3IO :: Show a => String -> a -> IO ()
dbg3IO = tracePrettyAtIO 3

dbg4IO :: Show a => String -> a -> IO ()
dbg4IO = tracePrettyAtIO 4

dbg5IO :: Show a => String -> a -> IO ()
dbg5IO = tracePrettyAtIO 5

dbg6IO :: Show a => String -> a -> IO ()
dbg6IO = tracePrettyAtIO 6

dbg7IO :: Show a => String -> a -> IO ()
dbg7IO = tracePrettyAtIO 7

dbg8IO :: Show a => String -> a -> IO ()
dbg8IO = tracePrettyAtIO 8

dbg9IO :: Show a => String -> a -> IO ()
dbg9IO = tracePrettyAtIO 9

-- | Pretty-print a message and a showable value to the console if the debug level is at or above the specified level.
-- dbtAt 0 always prints. Otherwise, uses unsafePerformIO.
tracePrettyAt :: Show a => Int -> String -> a -> a
tracePrettyAt lvl = dbgppshow lvl

tracePrettyAtIO :: Show a => Int -> String -> a -> IO ()
tracePrettyAtIO lvl lbl x = tracePrettyAt lvl lbl x `seq` return ()

-- XXX
-- Could not deduce (a ~ ())
-- from the context (Show a)
--   bound by the type signature for
--              dbgM :: Show a => String -> a -> IO ()
--   at hledger/Hledger/Cli/Main.hs:200:13-42
--   ‘a’ is a rigid type variable bound by
--       the type signature for dbgM :: Show a => String -> a -> IO ()
--       at hledger/Hledger/Cli/Main.hs:200:13
-- Expected type: String -> a -> IO ()
--   Actual type: String -> a -> IO a
--
-- tracePrettyAtM :: (Monad m, Show a) => Int -> String -> a -> m a
-- tracePrettyAtM lvl lbl x = tracePrettyAt lvl lbl x `seq` return x

-- | print this string to the console before evaluating the expression,
-- if the global debug level is non-zero.  Uses unsafePerformIO.
dbgtrace :: String -> a -> a
dbgtrace
    | debugLevel > 0 = trace
    | otherwise      = flip const

-- | Print a showable value to the console, with a message, if the
-- debug level is at or above the specified level (uses
-- unsafePerformIO).
-- Values are displayed with show, all on one line, which is hard to read.
dbgshow :: Show a => Int -> String -> a -> a
dbgshow level
    | debugLevel >= level = ltrace
    | otherwise           = flip const

-- | Print a showable value to the console, with a message, if the
-- debug level is at or above the specified level (uses
-- unsafePerformIO).
-- Values are displayed with ppShow, each field/constructor on its own line.
dbgppshow :: Show a => Int -> String -> a -> a
dbgppshow level
    | level > 0 && debugLevel < level = flip const
    | otherwise = \s a -> let p = ppShow a
                              ls = lines p
                              nlorspace | length ls > 1 = "\n"
                                        | otherwise     = " " ++ take (10 - length s) (repeat ' ')
                              ls' | length ls > 1 = map (" "++) ls
                                  | otherwise     = ls
                          in trace (s++":"++nlorspace++intercalate "\n" ls') a

-- -- | Print a showable value to the console, with a message, if the
-- -- debug level is at or above the specified level (uses
-- -- unsafePerformIO).
-- -- Values are displayed with pprint. Field names are not shown, but the
-- -- output is compact with smart line wrapping, long data elided,
-- -- and slow calculations timed out.
-- dbgpprint :: Data a => Int -> String -> a -> a
-- dbgpprint level msg a
--     | debugLevel >= level = unsafePerformIO $ do
--                               pprint a >>= putStrLn . ((msg++": \n") ++) . show
--                               return a
--     | otherwise           = a


-- | Like dbg, then exit the program. Uses unsafePerformIO.
dbgExit :: Show a => String -> a -> a
dbgExit msg = const (unsafePerformIO exitFailure) . dbg msg

-- | Print a message and parsec debug info (parse position and next
-- input) to the console when the debug level is at or above
-- this level. Uses unsafePerformIO.
-- pdbgAt :: GenParser m => Float -> String -> m ()
pdbg :: Stream [Char] m t => Int -> String -> ParsecT [Char] st m ()
pdbg level msg = when (level <= debugLevel) $ ptrace msg


