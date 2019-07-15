{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- | Debugging helpers

-- more:
-- http://hackage.haskell.org/packages/archive/TraceUtils/0.1.0.2/doc/html/Debug-TraceUtils.html
-- http://hackage.haskell.org/packages/archive/trace-call/0.1/doc/html/Debug-TraceCall.html
-- http://hackage.haskell.org/packages/archive/htrace/0.1/doc/html/Debug-HTrace.html
-- http://hackage.haskell.org/packages/archive/traced/2009.7.20/doc/html/Debug-Traced.html

module Hledger.Utils.Debug (
   pprint
  ,pshow
  ,ptrace
  ,traceWith
  ,debugLevel
  ,ptraceAt
  ,ptraceAtWith
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
  ,dbgExit
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
  ,plog
  ,plogAt
  ,traceParse
  ,dbgparse
  ,module Debug.Trace
)
where

import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Data.List hiding (uncons)
import qualified Data.Text as T
import           Debug.Trace
import           Hledger.Utils.Parse
import           Safe (readDef)
import           System.Environment (getArgs)
import           System.Exit
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Megaparsec
import           Text.Printf
import           Text.Show.Pretty (ppShow, pPrint)

-- | Pretty print. Easier alias for pretty-show's pPrint.
pprint :: Show a => a -> IO ()
pprint = pPrint

-- | Pretty show. Easier alias for pretty-show's ppShow.
pshow :: Show a => a -> String
pshow = ppShow

-- | Pretty trace. Easier alias for traceShowId + ppShow.
ptrace :: Show a => a -> a
ptrace = traceWith pshow

-- | Trace (print to stderr) a showable value using a custom show function.
traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

-- | Global debug level, which controls the verbosity of debug output
-- on the console. The default is 0 meaning no debug output. The
-- @--debug@ command line flag sets it to 1, or @--debug=N@ sets it to
-- a higher value (note: not @--debug N@ for some reason).  This uses
-- unsafePerformIO and can be accessed from anywhere and before normal
-- command-line processing. When running with :main in GHCI, you must
-- touch and reload this module to see the effect of a new --debug option.
-- After command-line processing, it is also available as the @debug_@
-- field of 'Hledger.Cli.CliOptions.CliOpts'.
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

-- | Pretty-print a label and a showable value to the console
-- if the global debug level is at or above the specified level.
-- At level 0, always prints. Otherwise, uses unsafePerformIO.
ptraceAt :: Show a => Int -> String -> a -> a
ptraceAt level
    | level > 0 && debugLevel < level = flip const
    | otherwise = \s a -> let p = ppShow a
                              ls = lines p
                              nlorspace | length ls > 1 = "\n"
                                        | otherwise     = " " ++ take (10 - length s) (repeat ' ')
                              ls' | length ls > 1 = map (" "++) ls
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

-- | Like dbg0, but also exit the program. Uses unsafePerformIO.
dbgExit :: Show a => String -> a -> a
dbgExit msg = const (unsafePerformIO exitFailure) . dbg0 msg

-- | Like ptraceAt, but convenient to insert in an IO monad (plus
-- convenience aliases).
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

-- | Log a label and a pretty-printed showable value to ./debug.log, then return it.
-- Can fail, see plogAt.
plog :: Show a => String -> a -> a
plog = plogAt 0

-- | Log a label and a pretty-printed showable value to ./debug.log,
-- if the global debug level is at or above the specified level.
-- At level 0, always logs. Otherwise, uses unsafePerformIO.
-- Tends to fail if called more than once, at least when built with -threaded
-- (Exception: debug.log: openFile: resource busy (file is locked)).
plogAt :: Show a => Int -> String -> a -> a
plogAt lvl
    | lvl > 0 && debugLevel < lvl = flip const
    | otherwise = \s a ->
        let p = ppShow a
            ls = lines p
            nlorspace | length ls > 1 = "\n"
                      | otherwise     = " " ++ take (10 - length s) (repeat ' ')
            ls' | length ls > 1 = map (" "++) ls
                | otherwise     = ls
            output = s++":"++nlorspace++intercalate "\n" ls'++"\n"
        in unsafePerformIO $ appendFile "debug.log" output >> return a

-- XXX redundant ? More/less robust than plogAt ?
-- -- | Like dbg, but writes the output to "debug.log" in the current directory.
-- dbglog :: Show a => String -> a -> a
-- dbglog label a =
--   (unsafePerformIO $
--     appendFile "debug.log" $ label ++ ": " ++ ppShow a ++ "\n")
--   `seq` a

-- | Print the provided label (if non-null) and current parser state
-- (position and next input) to the console. (See also megaparsec's dbg.)
traceParse :: String -> TextParser m ()
traceParse msg = do
  pos <- getSourcePos
  next <- (T.take peeklength) `fmap` getInput
  let (l,c) = (sourceLine pos, sourceColumn pos)
      s  = printf "at line %2d col %2d: %s" (unPos l) (unPos c) (show next) :: String
      s' = printf ("%-"++show (peeklength+30)++"s") s ++ " " ++ msg
  trace s' $ return ()
  where
    peeklength = 30

-- | Print the provided label (if non-null) and current parser state
-- (position and next input) to the console if the global debug level
-- is at or above the specified level. Uses unsafePerformIO.
-- (See also megaparsec's dbg.)
traceParseAt :: Int -> String -> TextParser m ()
traceParseAt level msg = when (level <= debugLevel) $ traceParse msg

-- | Convenience alias for traceParseAt
dbgparse :: Int -> String -> TextParser m ()
dbgparse level msg = traceParseAt level msg

