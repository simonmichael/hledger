{- | 
Helpers for pretty-formatting haskell values, pretty-printing to console,
deciding if ANSI colour should be used, and detecting an -o/--output-file option.
Uses unsafePerformIO for simple program-wide read-only access to some common
command-line flags/environment variables.

Limitations:
When running in GHCI, this module must be reloaded to see a change (because of unsafePerformIO).
The colour scheme may be somewhat hard-coded.

-}

module Hledger.Utils.Print (
  -- * Pretty showing as a string
   pshow
  ,pshow'
  -- * Pretty printing to stdout
  ,pprint
  ,pprint'
  -- * Command line arguments
  ,progArgs
  -- * Detecting --color/--colour/NO_COLOR
  ,colorOption
  ,useColorOnStdout
  ,useColorOnStderr
  -- * Detecting -o/--output-file
  ,outputFileOption
  ,hasOutputFile
  )
where

import           Data.List hiding (uncons)
import           Data.Maybe (isJust)
import qualified Data.Text.Lazy as TL
import           System.Console.ANSI (hSupportsANSIColor)
import           System.Environment (getArgs, lookupEnv)
import           System.IO (stdout, Handle, stderr)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Pretty.Simple  -- (defaultOutputOptionsDarkBg, OutputOptions(..), pShowOpt, pPrintOpt)

-- | pretty-simple options with colour enabled if allowed.
prettyopts = 
  (if useColorOnStderr then defaultOutputOptionsDarkBg else defaultOutputOptionsNoColor)
    { outputOptionsIndentAmount=2
    , outputOptionsCompact=True
    }

-- | pretty-simple options with colour disabled.
prettyopts' =
  defaultOutputOptionsNoColor
    { outputOptionsIndentAmount=2
    , outputOptionsCompact=True
    }

-- | Pretty show. Easier alias for pretty-simple's pShow.
pshow :: Show a => a -> String
pshow = TL.unpack . pShowOpt prettyopts

-- | Monochrome version of pshow.
pshow' :: Show a => a -> String
pshow' = TL.unpack . pShowOpt prettyopts'

-- | Pretty print. Easier alias for pretty-simple's pPrint.
pprint :: Show a => a -> IO ()
pprint = pPrintOpt CheckColorTty prettyopts

-- | Monochrome version of pprint.
pprint' :: Show a => a -> IO ()
pprint' = pPrintOpt CheckColorTty prettyopts'

-- Avoid using pshow, pprint, dbg* in the code below to prevent infinite loops.

-- | The command line arguments that were used at program startup.
-- Uses unsafePerformIO.
{-# NOINLINE progArgs #-}
progArgs :: [String]
progArgs = unsafePerformIO getArgs

-- | Read the value of the --color or --colour command line option provided at program startup
-- using unsafePerformIO. If this option was not provided, returns the empty string.
colorOption :: String
colorOption = 
  -- similar to debugLevel
  -- keep synced with color/colour flag definition in hledger:CliOptions
  let args = progArgs in
  case dropWhile (/="--color") args of
    -- --color ARG
    "--color":v:_ -> v
    _ ->
      case take 1 $ filter ("--color=" `isPrefixOf`) args of
        -- --color=ARG
        ['-':'-':'c':'o':'l':'o':'r':'=':v] -> v
        _ ->
          case dropWhile (/="--colour") args of
            -- --colour ARG
            "--colour":v:_ -> v
            _ ->
              case take 1 $ filter ("--colour=" `isPrefixOf`) args of
                -- --colour=ARG
                ['-':'-':'c':'o':'l':'o':'u':'r':'=':v] -> v
                _ -> ""

-- | Check the IO environment to see if ANSI colour codes should be used on stdout.
-- This is done using unsafePerformIO so it can be used anywhere, eg in
-- low-level debug utilities, which should be ok since we are just reading.
-- The logic is: use color if
-- the program was started with --color=yes|always
-- or (
--   the program was not started with --color=no|never
--   and a NO_COLOR environment variable is not defined
--   and stdout supports ANSI color and -o/--output-file was not used or is "-"
-- ).
-- {-# OPTIONS_GHC -fno-cse #-}
-- {-# NOINLINE useColorOnStdout #-}
useColorOnStdout :: Bool
useColorOnStdout = not hasOutputFile && useColorOnHandle stdout

-- Avoid using dbg*, pshow etc. in this function (infinite loop).
-- | Like useColorOnStdout, but checks for ANSI color support on stderr,
-- and is not affected by -o/--output-file.
-- {-# OPTIONS_GHC -fno-cse #-}
-- {-# NOINLINE useColorOnStdout #-}
useColorOnStderr :: Bool
useColorOnStderr = useColorOnHandle stderr

-- sorry, I'm just cargo-culting these pragmas:
-- {-# OPTIONS_GHC -fno-cse #-}
-- {-# NOINLINE useColorOnHandle #-}
useColorOnHandle :: Handle -> Bool
useColorOnHandle h = unsafePerformIO $ do
  no_color       <- isJust <$> lookupEnv "NO_COLOR"
  supports_color <- hSupportsANSIColor h
  let coloroption = colorOption
  return $ coloroption `elem` ["always","yes"]
       || (coloroption `notElem` ["never","no"] && not no_color && supports_color)

-- | Read the value of the -o/--output-file command line option provided at program startup,
-- if any, using unsafePerformIO.
outputFileOption :: Maybe String
outputFileOption = 
  -- keep synced with output-file flag definition in hledger:CliOptions.
  let args = progArgs in
  case dropWhile (not . ("-o" `isPrefixOf`)) args of
    -- -oARG
    ('-':'o':v@(_:_)):_ -> Just v
    -- -o ARG
    "-o":v:_ -> Just v
    _ ->
      case dropWhile (/="--output-file") args of
        -- --output-file ARG
        "--output-file":v:_ -> Just v
        _ ->
          case take 1 $ filter ("--output-file=" `isPrefixOf`) args of
            -- --output=file=ARG
            ['-':'-':'o':'u':'t':'p':'u':'t':'-':'f':'i':'l':'e':'=':v] -> Just v
            _ -> Nothing

-- | Check whether the -o/--output-file option has been used at program startup
-- with an argument other than "-", using unsafePerformIO.
-- {-# OPTIONS_GHC -fno-cse #-}
-- {-# NOINLINE hasOutputFile #-}
hasOutputFile :: Bool
hasOutputFile = outputFileOption `notElem` [Nothing, Just "-"]

