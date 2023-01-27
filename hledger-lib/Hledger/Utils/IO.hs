{- | 
Helpers for pretty-printing haskell values, reading command line arguments,
working with ANSI colours, files, and time.
Uses unsafePerformIO.

Limitations:
When running in GHCI, this module must be reloaded to see environmental changes.
The colour scheme may be somewhat hard-coded.

-}

{-# LANGUAGE LambdaCase #-}

module Hledger.Utils.IO (

  -- * Pretty showing/printing
  pshow,
  pshow',
  pprint,
  pprint',

  -- * Viewing with pager
  pager,

  -- * Command line arguments
  progArgs,
  outputFileOption,
  hasOutputFile,

  -- * ANSI color
  colorOption,
  useColorOnStdout,
  useColorOnStderr,
  color,
  bgColor,
  colorB,
  bgColorB,
  terminalIsLight,
  terminalLightness,
  terminalFgColor,
  terminalBgColor,

  -- * Errors
  error',
  usageError,

  -- * Files
  embedFileRelative,
  expandHomePath,
  expandPath,
  readFileOrStdinPortably,
  readFilePortably,
  readHandlePortably,
  -- hereFileRelative,

  -- * Time
  getCurrentLocalTime,
  getCurrentZonedTime,

  )
where

import           Control.Monad (when)
import           Data.Colour.RGBSpace (RGB(RGB))
import           Data.Colour.RGBSpace.HSL (lightness)
import           Data.FileEmbed (makeRelativeToProject, embedStringFile)
import           Data.List hiding (uncons)
import           Data.Maybe (isJust)
import           Data.Text (Text, pack)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.LocalTime
  (LocalTime, ZonedTime, getCurrentTimeZone, utcToLocalTime, utcToZonedTime)
import           Data.Word (Word16)
import           Language.Haskell.TH.Syntax (Q, Exp)
import           System.Console.ANSI
  (Color,ColorIntensity,ConsoleLayer(..), SGR(..), hSupportsANSIColor, setSGRCode, getLayerColor)
import           System.Directory (getHomeDirectory)
import           System.Environment (getArgs, lookupEnv)
import           System.FilePath (isRelative, (</>))
import           System.IO
  (Handle, IOMode (..), hGetEncoding, hSetEncoding, hSetNewlineMode,
   openFile, stdin, stdout, stderr, universalNewlineMode, utf8_bom)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Pager
import           Text.Pretty.Simple
  (CheckColorTty(CheckColorTty), OutputOptions(..), 
  defaultOutputOptionsDarkBg, defaultOutputOptionsNoColor, pShowOpt, pPrintOpt)

import Hledger.Utils.Text (WideBuilder(WideBuilder))

-- Pretty showing/printing with pretty-simple

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

-- "Avoid using pshow, pprint, dbg* in the code below to prevent infinite loops." (?)

-- | Display the given text on the terminal, using the user's $PAGER if the text is taller 
-- than the current terminal and stdout is interactive and TERM is not "dumb".
pager :: String -> IO ()
pager s = do
  dumbterm <- (== Just "dumb") <$> lookupEnv "TERM"
  (if dumbterm then putStrLn else printOrPage . pack) s

-- Command line arguments

-- | The command line arguments that were used at program startup.
-- Uses unsafePerformIO.
{-# NOINLINE progArgs #-}
progArgs :: [String]
progArgs = unsafePerformIO getArgs

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
hasOutputFile :: Bool
hasOutputFile = outputFileOption `notElem` [Nothing, Just "-"]
-- XXX shouldn't we check that stdout is interactive. instead ?

-- ANSI colour

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
--   and stdout supports ANSI color
--   and -o/--output-file was not used, or its value is "-"
-- ).
useColorOnStdout :: Bool
useColorOnStdout = not hasOutputFile && useColorOnHandle stdout

-- | Like useColorOnStdout, but checks for ANSI color support on stderr,
-- and is not affected by -o/--output-file.
useColorOnStderr :: Bool
useColorOnStderr = useColorOnHandle stderr

useColorOnHandle :: Handle -> Bool
useColorOnHandle h = unsafePerformIO $ do
  no_color       <- isJust <$> lookupEnv "NO_COLOR"
  supports_color <- hSupportsANSIColor h
  let coloroption = colorOption
  return $ coloroption `elem` ["always","yes"]
       || (coloroption `notElem` ["never","no"] && not no_color && supports_color)

-- | Wrap a string in ANSI codes to set and reset foreground colour.
color :: ColorIntensity -> Color -> String -> String
color int col s = setSGRCode [SetColor Foreground int col] ++ s ++ setSGRCode []

-- | Wrap a string in ANSI codes to set and reset background colour.
bgColor :: ColorIntensity -> Color -> String -> String
bgColor int col s = setSGRCode [SetColor Background int col] ++ s ++ setSGRCode []

-- | Wrap a WideBuilder in ANSI codes to set and reset foreground colour.
colorB :: ColorIntensity -> Color -> WideBuilder -> WideBuilder
colorB int col (WideBuilder s w) =
    WideBuilder (TB.fromString (setSGRCode [SetColor Foreground int col]) <> s <> TB.fromString (setSGRCode [])) w

-- | Wrap a WideBuilder in ANSI codes to set and reset background colour.
bgColorB :: ColorIntensity -> Color -> WideBuilder -> WideBuilder
bgColorB int col (WideBuilder s w) =
    WideBuilder (TB.fromString (setSGRCode [SetColor Background int col]) <> s <> TB.fromString (setSGRCode [])) w

-- | Detect whether the terminal currently has a light background colour,
-- if possible, using unsafePerformIO.
-- If the terminal is transparent, its apparent light/darkness may be different.
terminalIsLight :: Maybe Bool
terminalIsLight = (> 0.5) <$> terminalLightness

-- | Detect the terminal's current background lightness (0..1), if possible, using unsafePerformIO.
-- If the terminal is transparent, its apparent lightness may be different.
terminalLightness :: Maybe Float
terminalLightness = lightness <$> terminalColor Background

-- | Detect the terminal's current background colour, if possible, using unsafePerformIO.
terminalBgColor :: Maybe (RGB Float)
terminalBgColor = terminalColor Background

-- | Detect the terminal's current foreground colour, if possible, using unsafePerformIO.
terminalFgColor :: Maybe (RGB Float)
terminalFgColor = terminalColor Foreground

{-# NOINLINE terminalColor #-}
terminalColor :: ConsoleLayer -> Maybe (RGB Float)
terminalColor layer = unsafePerformIO $ do
  fmap fractionalRGB <$> getLayerColor layer  -- safe to run on non-interactive/dumb terminal
  where
    fractionalRGB :: (Fractional a) => RGB Word16 -> RGB a
    fractionalRGB (RGB r g b) = RGB (fromIntegral r / 65535) (fromIntegral g / 65535) (fromIntegral b / 65535)  -- chatgpt

-- Errors

-- | Simpler alias for errorWithoutStackTrace
error' :: String -> a
error' = errorWithoutStackTrace . ("Error: " <>)

-- | A version of errorWithoutStackTrace that adds a usage hint.
usageError :: String -> a
usageError = error' . (++ " (use -h to see usage)")

-- Files

-- | Convert a possibly relative, possibly tilde-containing file path to an absolute one,
-- given the current directory. ~username is not supported. Leave "-" unchanged.
-- Can raise an error.
expandPath :: FilePath -> FilePath -> IO FilePath -- general type sig for use in reader parsers
expandPath _ "-" = return "-"
expandPath curdir p = (if isRelative p then (curdir </>) else id) <$> expandHomePath p
-- PARTIAL:

-- | Expand user home path indicated by tilde prefix
expandHomePath :: FilePath -> IO FilePath
expandHomePath = \case
    ('~':'/':p)  -> (</> p) <$> getHomeDirectory
    ('~':'\\':p) -> (</> p) <$> getHomeDirectory
    ('~':_)      -> ioError $ userError "~USERNAME in paths is not supported"
    p            -> return p

-- | Read text from a file,
-- converting any \r\n line endings to \n,,
-- using the system locale's text encoding,
-- ignoring any utf8 BOM prefix (as seen in paypal's 2018 CSV, eg) if that encoding is utf8.
readFilePortably :: FilePath -> IO Text
readFilePortably f =  openFile f ReadMode >>= readHandlePortably

-- | Like readFilePortably, but read from standard input if the path is "-".
readFileOrStdinPortably :: String -> IO Text
readFileOrStdinPortably f = openFileOrStdin f ReadMode >>= readHandlePortably
  where
    openFileOrStdin :: String -> IOMode -> IO Handle
    openFileOrStdin "-" _ = return stdin
    openFileOrStdin f' m   = openFile f' m

readHandlePortably :: Handle -> IO Text
readHandlePortably h = do
  hSetNewlineMode h universalNewlineMode
  menc <- hGetEncoding h
  when (fmap show menc == Just "UTF-8") $  -- XXX no Eq instance, rely on Show
    hSetEncoding h utf8_bom
  T.hGetContents h

-- | Like embedFile, but takes a path relative to the package directory.
-- Similar to embedFileRelative ?
embedFileRelative :: FilePath -> Q Exp
embedFileRelative f = makeRelativeToProject f >>= embedStringFile

-- -- | Like hereFile, but takes a path relative to the package directory.
-- -- Similar to embedFileRelative ?
-- hereFileRelative :: FilePath -> Q Exp
-- hereFileRelative f = makeRelativeToProject f >>= hereFileExp
--   where
--     QuasiQuoter{quoteExp=hereFileExp} = hereFile

-- Time

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz t

getCurrentZonedTime :: IO ZonedTime
getCurrentZonedTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToZonedTime tz t

