{- | 
General and hledger-specific input/output-related helpers for
pretty-printing haskell values, error reporting, time, files, command line parsing,
terminals, pager output, ANSI colour/styles, etc.
-}

{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Utils.IO (

  -- * Pretty showing/printing
  pshow,
  pshow',
  pprint,
  pprint',

  -- * Errors
  error',
  usageError,
  warn,
  ansiFormatError,
  ansiFormatWarning,
  printError,
  exitWithErrorMessage,
  exitOnError,

  -- * Time
  getCurrentLocalTime,
  getCurrentZonedTime,

  -- * Files
  getHomeSafe,
  embedFileRelative,
  expandHomePath,
  expandPath,
  expandGlob,
  sortByModTime,
  openFileOrStdin,
  readFileOrStdinPortably,
  readFileOrStdinPortably',
  readFileStrictly,
  readFilePortably,
  readHandlePortably,
  readHandlePortably',
  -- hereFileRelative,
  inputToHandle,

  -- * Command line parsing
  progArgs,
  getOpt,
  parseYN,
  parseYNA,
  YNA(..),
  -- hasOutputFile,
  -- outputFileOption,

  -- * Terminal size
  getTerminalHeightWidth,
  getTerminalHeight,
  getTerminalWidth,

  -- * Pager output
  setupPager,
  findPager,
  runPager,

  -- * ANSI colour/styles

  -- ** hledger-specific

  colorOption,
  useColorOnStdout,
  useColorOnStderr,
  useColorOnStdoutUnsafe,
  useColorOnStderrUnsafe,
  bold',
  faint',
  black',
  red',
  green',
  yellow',
  blue',
  magenta',
  cyan',
  white',
  brightBlack',
  brightRed',
  brightGreen',
  brightYellow',
  brightBlue',
  brightMagenta',
  brightCyan',
  brightWhite',
  rgb',
  sgrresetall,

  -- ** Generic

  color,
  bgColor,
  colorB,
  bgColorB,
  -- XXX Types used with color/bgColor/colorB/bgColorB,
  -- not re-exported because clashing with UIUtils:
  -- Color(..),
  -- ColorIntensity(..),

  terminalIsLight,
  terminalLightness,
  terminalFgColor,
  terminalBgColor,

  )
where

import           Control.Concurrent (forkIO)
import           Control.Exception
import           Control.Monad (when, forM, guard, void)
import           Data.Char (toLower, isSpace)
import           Data.Colour.RGBSpace (RGB(RGB))
import           Data.Colour.RGBSpace.HSL (lightness)
import           Data.Colour.SRGB (sRGB)
import           Data.Encoding (DynEncoding)
import           Data.FileEmbed (makeRelativeToProject, embedStringFile)
import           Data.Functor ((<&>))
import           Data.List hiding (uncons)
import           Data.Maybe (isJust, catMaybes)
import           Data.Ord (comparing, Down (Down))
import qualified Data.Text as T
import           Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.LocalTime (LocalTime, ZonedTime, getCurrentTimeZone, utcToLocalTime, utcToZonedTime)
import           Data.Word (Word16)
import           Debug.Trace
import           Foreign.C.Error (Errno(..), ePIPE)
import           GHC.IO.Encoding (getLocaleEncoding, textEncodingName)
import           GHC.IO.Exception (IOException(..), IOErrorType (ResourceVanished))
import           Language.Haskell.TH.Syntax (Q, Exp)
import           Safe (headMay, maximumDef)
import           System.Console.ANSI (Color(..),ColorIntensity(..), ConsoleLayer(..), SGR(..), hSupportsANSIColor, setSGRCode, getLayerColor, ConsoleIntensity (..))
import           System.Console.Terminal.Size (Window (Window), size)
import           System.Directory (getHomeDirectory, getModificationTime, findExecutable)
import           System.Environment (getArgs, lookupEnv, setEnv, getProgName)
import           System.Exit (exitFailure)
import           System.FilePath (isRelative, (</>))
import "Glob"    System.FilePath.Glob (glob)
import           System.Info (os)
import           System.IO (Handle, IOMode (..), hClose, hGetEncoding, hIsTerminalDevice, hPutStr, hPutStrLn, hSetNewlineMode, hSetEncoding, openFile, stderr, stdin, stdout, universalNewlineMode, utf8_bom)
import qualified System.IO.Encoding as Enc
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process (CreateProcess(..), StdStream(CreatePipe), createPipe, shell, waitForProcess, withCreateProcess)
import           Text.Pretty.Simple (CheckColorTty(..), OutputOptions(..), defaultOutputOptionsDarkBg, defaultOutputOptionsNoColor, pShowOpt, pPrintOpt)

import Hledger.Utils.Text (WideBuilder(WideBuilder))


-- Pretty showing/printing
-- using pretty-simple

-- https://hackage.haskell.org/package/pretty-simple/docs/Text-Pretty-Simple.html#t:OutputOptions

-- | pretty-simple options with colour enabled if allowed.
prettyopts =
  (if useColorOnStderrUnsafe then defaultOutputOptionsDarkBg else defaultOutputOptionsNoColor)
    { outputOptionsIndentAmount = 2
    -- , outputOptionsCompact      = True  -- fills lines, but does not respect page width (https://github.com/cdepillabout/pretty-simple/issues/126)
    -- , outputOptionsPageWidth    = fromMaybe 80 $ unsafePerformIO getTerminalWidth
    }

-- | pretty-simple options with colour disabled.
prettyoptsNoColor =
  defaultOutputOptionsNoColor
    { outputOptionsIndentAmount=2
    }

-- | Pretty show. An easier alias for pretty-simple's pShow.
-- This will probably show in colour if useColorOnStderrUnsafe is true.
pshow :: Show a => a -> String
pshow = TL.unpack . pShowOpt prettyopts

-- | Monochrome version of pshow. This will never show in colour.
pshow' :: Show a => a -> String
pshow' = TL.unpack . pShowOpt prettyoptsNoColor

-- | Pretty print a showable value. An easier alias for pretty-simple's pPrint.
-- This will print in colour if useColorOnStderrUnsafe is true.
pprint :: Show a => a -> IO ()
pprint = pPrintOpt (if useColorOnStderrUnsafe then CheckColorTty else NoCheckColorTty) prettyopts

-- | Monochrome version of pprint. This will never print in colour.
pprint' :: Show a => a -> IO ()
pprint' = pPrintOpt NoCheckColorTty prettyoptsNoColor

-- "Avoid using pshow, pprint, dbg* in the code below to prevent infinite loops." (?)



-- Errors

-- | Call errorWithoutStackTrace, prepending a "Error:" label.
error' :: String -> a
error' = errorWithoutStackTrace . ("Error: "<>)

-- | Like error', but add a hint about using -h.
usageError :: String -> a
usageError = error' . (++ " (use -h to see usage)")

-- | Apply standard ANSI SGR formatting (red, bold) suitable for console error text.
ansiFormatError :: String -> String
ansiFormatError = (<> sgrresetall) . ((sgrbrightred <> sgrbold) <>)

-- | Show a message, with "Warning:" label, on stderr before returning the given value.
-- Also do some ANSI styling of the first line when allowed (using unsafe IO).
-- Currently we use this very sparingly in hledger; we prefer to either quietly work,
-- or loudly raise an error. (Varying output can make scripting harder.)
warn :: String -> a -> a
warn msg = trace msg'
  where
    msg' =
      (if useColorOnStderrUnsafe then modifyFirstLine ansiFormatWarning else id) $
      "Warning: "<> msg

-- | Apply standard ANSI SGR formatting (yellow, bold) suitable for console warning text.
ansiFormatWarning :: String -> String
ansiFormatWarning = (<> sgrresetall) . ((sgrbrightyellow <> sgrbold) <>)

-- Transform a string's first line.
-- Note, this won't add a trailing newline if there isn't one,
-- and it will remove one if there is one or more.
modifyFirstLine :: (String -> String) -> String -> String
modifyFirstLine f s = intercalate "\n" $ map f l <> ls where (l,ls) = splitAt 1 $ lines s  -- total

{- | Print an error message to stderr, with a consistent "programname: " prefix,
and applying ANSI styling (bold bright red) to the first line if that is supported and allowed.
-}
printError :: String -> IO ()
printError msg = do
  progname <- getProgName
  usecolor <- useColorOnStderr
  let
    style = if usecolor then modifyFirstLine ansiFormatError else id
    prefix =
      progname
        <> ": "
        -- error' prepends an "Error: " prefix. But that seems to have been removed when I catch the ErrorCall exception - unless I'm running in GHCI.
        -- Is it possible something in GHC or base is removing it ?
        -- Use a stupid heuristic for now: add it again unless already there.
        <> (if "Error:" `isPrefixOf` msg then "" else "Error: ")
  hPutStrLn stderr $ style $ prefix <> msg

{- | Print an error message with printError,
then exit the program with a non-zero exit code.
-}
exitWithErrorMessage :: String -> IO ()
exitWithErrorMessage msg = printError msg >> exitFailure

-- | This wraps a program's main routine so as to display more consistent
-- and useful error output for some common program-terminating exceptions,
-- independent of compiler version. It catches:
--
-- - UnicodeException - unicode errors in pure code
--
-- - IOException AKA IOError - I/O errors, including unicode errors during I/O
--
-- - ErrorCall - @error@ / @errorWithoutStackTrace@ calls
--
-- and:
-- 
-- - removes the trailing newlines added by some GHC 9.10.*
--
-- - removes "uncaught exception" output added by some GHC 9.12.*
--
-- - ensures a consistent "programname: " prefix
--
-- - applies ANSI styling (bold bright red) to the first line if that is supported and allowed
--
-- - for unicode exceptions, and I/O exceptions which look like they were
--   caused by a unicode error (usually text decoding failure),
--   it adds (english) text explaining the problem and what to do.
--
-- Some exceptions this does not handle:
-- ExitCode (exitSuccess / exitFailure / exitWith calls)
-- and UserInterrupt (control-C).
--
exitOnError :: IO () -> IO ()
exitOnError = flip catches
  [-- Handler (\(e::SomeException) -> error' $ pshow e),  -- debug
   Handler (\(e::UnicodeException) -> exitUnicode e)
  ,Handler (\(e::IOException) -> if isUnicodeError e then exitUnicode e else exitOther e)
  ,Handler (\(e::ErrorCall) -> exitOther e)
  ]

  where
    -- Many decoding failures do not produce a UnicodeException, unfortunately.
    -- So this fragile hack detects them from the error message.
    -- But there are many variant wordings and they probably change over time.
    -- It's not ideal.
    isUnicodeError :: Exception e => e -> Bool
    isUnicodeError ex = any (`isInfixOf` msg) unicodeerrorpatterns
      where
        msg = map toLower $ show ex
        unicodeerrorpatterns = [
            "illegal byte sequence"
          , "invalid byte sequence"
          , "cannot decode byte sequence"
          , "invalid character"
          , "invalid or incomplete multibyte"
          , "mkTextEncoding: invalid argument"
          ]

    exitUnicode :: Exception e => e -> IO ()
    exitUnicode ex = do
      enc <- getSystemEncoding
      let
        noencoding = map toLower enc == "ascii"
        msg = unlines $ [
            rstrip $ show ex
          , "Some text could not be decoded/encoded with the system text encoding: " <> enc
          ] ++
          if noencoding
          then [
            "Please configure a system locale with a text encoding to handle non-ascii text"
          ]
          else [
            -- advice suitable for programs which always use the system text encoding:
            "Please convert all data to the system encoding (eg with iconv),"
          , "or configure the system encoding to match your data (eg by setting LANG)."
          ]
      exitWithErrorMessage msg

    exitOther :: Exception e => e -> IO ()
    exitOther = exitWithErrorMessage . rstrip . show

    rstrip = reverse . dropWhile isSpace . reverse

-- I18n

-- encoding has a similar getSystemEncoding :: IO (Maybe DynEncoding)
-- but it returns Nothing on Windows or if there's an error.

-- | Get the name of the text encoding used by the current locale, using GHC's API.
getSystemEncoding :: IO String
getSystemEncoding = do
  localeEncoding <- getLocaleEncoding
  return $ textEncodingName localeEncoding

-- -- | Get the name of the text encoding currently configured for stdout, using GHC's API.
-- getStdoutEncoding :: IO (Maybe String)
-- getStdoutEncoding = do
--   mEncoding <- hGetEncoding stdout
--   return $ fmap textEncodingName mEncoding

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



-- Files

-- | Like getHomeDirectory, but in case of IO error (home directory not found, not understood, etc.), returns "".
getHomeSafe :: IO (Maybe FilePath)
getHomeSafe = fmap Just getHomeDirectory `catch` (\(_ :: IOException) -> return Nothing)

-- | Expand a tilde (representing home directory) at the start of a file path.
-- ~username is not supported. Can raise an error.
expandHomePath :: FilePath -> IO FilePath
expandHomePath = \case
    ('~':'/':p)  -> (</> p) <$> getHomeDirectory
    ('~':'\\':p) -> (</> p) <$> getHomeDirectory
    ('~':_)      -> ioError $ userError "~USERNAME in paths is not supported"
    p            -> return p

-- | Given a current directory, convert a possibly relative, possibly tilde-containing
-- file path to an absolute one.
-- ~username is not supported. Leaves "-" unchanged. Can raise an error.
expandPath :: FilePath -> FilePath -> IO FilePath -- general type sig for use in reader parsers
expandPath _ "-" = return "-"
expandPath curdir p = (if isRelative p then (curdir </>) else id) <$> expandHomePath p  -- PARTIAL:

-- | Like expandPath, but treats the expanded path as a glob, and returns
-- zero or more matched absolute file paths, alphabetically sorted.
-- Can raise an error.
expandGlob :: FilePath -> FilePath -> IO [FilePath]
expandGlob curdir p = expandPath curdir p >>= glob <&> sort  -- PARTIAL:

-- | Given a list of existing file paths, sort them by modification time, most recent first.
sortByModTime :: [FilePath] -> IO [FilePath]
sortByModTime fs = do
  ftimes <- forM fs $ \f -> do {t <- getModificationTime f; return (t,f)}
  return $ map snd $ sortBy (comparing Data.Ord.Down) ftimes

-- | Like readFilePortably, but read all of the file before proceeding.
readFileStrictly :: FilePath -> IO T.Text
readFileStrictly f = readFilePortably f >>= \t -> evaluate (T.length t) >> return t

-- | Read text from a file,
-- converting any \r\n line endings to \n,,
-- using the system locale's text encoding,
-- ignoring any utf8 BOM prefix (as seen in paypal's 2018 CSV, eg) if that encoding is utf8.
readFilePortably :: FilePath -> IO T.Text
readFilePortably f =  openFile f ReadMode >>= readHandlePortably

-- | Like readFilePortably, but read from standard input if the path is "-".
readFileOrStdinPortably :: String -> IO T.Text
readFileOrStdinPortably = readFileOrStdinPortably' Nothing

-- | Like readFileOrStdinPortably, but take an optional converter.
readFileOrStdinPortably' :: Maybe DynEncoding -> String -> IO T.Text
readFileOrStdinPortably' c f = openFileOrStdin f >>= readHandlePortably' c

-- | Open a file for reading, using the standard System.IO.openFile.
-- This opens the handle in text mode, using the initial system locale's text encoding.
openFileOrStdin :: String -> IO Handle
openFileOrStdin "-" = return stdin
openFileOrStdin f' = openFile f' ReadMode

-- readHandlePortably' with no text encoding specified.
readHandlePortably :: Handle -> IO T.Text
readHandlePortably = readHandlePortably' Nothing

-- | Read text from a handle with a specified encoding, using the encoding package.
-- Or if no encoding is specified, it uses the handle's current encoding,
-- after first changing it to UTF-8BOM if it was UTF-8, to allow a Byte Order Mark at the start.
-- Also it converts Windows line endings to newlines.
-- If decoding fails, this throws an IOException (or possibly a UnicodeException or something else from the encoding package).
readHandlePortably' :: Maybe DynEncoding -> Handle -> IO T.Text
readHandlePortably' Nothing h = do
  hSetNewlineMode h universalNewlineMode
  menc <- hGetEncoding h
  when (fmap show menc == Just "UTF-8") $ hSetEncoding h utf8_bom
  T.hGetContents h
readHandlePortably' (Just e) h =
  -- convert newlines manually, because Enc.hGetContents uses bytestring's hGetContents
  T.replace "\r\n" "\n" . T.pack <$> let ?enc = e in Enc.hGetContents h

-- | Create a handle from which the given text can be read.
-- Its encoding will be UTF-8BOM.
inputToHandle :: T.Text -> IO Handle
inputToHandle t = do
  (r, w) <- createPipe
  hSetEncoding r utf8_bom
  hSetEncoding w utf8_bom
  -- use a separate thread so that we don't deadlock if we can't write all of the text at once
  forkIO $ T.hPutStr w t >> hClose w
  return r

-- | Like embedFile, but takes a path relative to the package directory.
embedFileRelative :: FilePath -> Q Exp
embedFileRelative f = makeRelativeToProject f >>= embedStringFile

-- -- | Like hereFile, but takes a path relative to the package directory.
-- -- Similar to embedFileRelative ?
-- hereFileRelative :: FilePath -> Q Exp
-- hereFileRelative f = makeRelativeToProject f >>= hereFileExp
--   where
--     QuasiQuoter{quoteExp=hereFileExp} = hereFile



-- Command line parsing

-- | The program's command line arguments.
-- Uses unsafePerformIO; tends to stick in GHCI until reloaded,
-- and may or may not detect args provided by a hledger config file.
{-# NOINLINE progArgs #-}
progArgs :: [String]
progArgs = unsafePerformIO getArgs
-- XX currently this affects:
--  the enabling of orderdates and assertions checks in journalFinalise
--  a few cases involving --color (see useColorOnStdoutUnsafe)
--  --debug

-- | Given one or more long or short option names, read the rightmost value of this option from the command line arguments.
-- If the value is missing raise an error.
-- Concatenated short flags (-a -b written as -ab) are not supported.
getOpt :: [String] -> IO (Maybe String)
getOpt names = do
  rargs <- reverse . splitFlagsAndVals <$> getArgs
  let flags = map toFlag names
  return $
    case break ((`elem` flags)) rargs of
      (_,[])        -> Nothing
      ([],flag:_)   -> error' $ flag <> " requires a value"
      (argsafter,_) -> Just $ last argsafter

-- | Given a list of command line arguments, split any of the form --flag=VAL or -fVAL into two list items.
-- Concatenated short flags (-a -b written as -ab) are not supported.
splitFlagsAndVals :: [String] -> [String]
splitFlagsAndVals = concatMap $
  \case
    a@('-':'-':_) | '=' `elem` a -> let (x,y) = break (=='=') a in [x, drop 1 y]
    a@('-':f:_:_) | not $ f=='-' -> [take 2 a, drop 2 a]
    a -> [a]

-- | Convert a short or long flag name to a flag with leading hyphen(s).
toFlag [c] = ['-',c]
toFlag s   = '-':'-':s

-- | Parse y/yes/always or n/no/never to true or false, or return an error message.
parseYN :: String -> Either String Bool
parseYN s
  | l `elem` ["y","yes","always"] = Right True
  | l `elem` ["n","no","never"]   = Right False
  | otherwise = Left $ "value should be one of " <> (intercalate ", " ["y","yes","n","no"])
  where l = map toLower s

data YNA = Yes | No | Auto deriving (Eq,Show)

-- | Parse y/yes/always or n/no/never or a/auto to a YNA choice, or return an error message.
parseYNA :: String -> Either String YNA
parseYNA s
  | l `elem` ["y","yes","always"] = Right Yes
  | l `elem` ["n","no","never"]   = Right No
  | l `elem` ["a","auto"]         = Right Auto
  | otherwise = Left $ "value should be one of " <> (intercalate ", " ["y","yes","n","no","a","auto"])
  where l = map toLower s

-- | Is there a --output-file or -o option in the command line arguments ?
-- Uses getOpt; sticky in GHCI until reloaded, may not always be affected by a hledger config file, etc.
hasOutputFile :: IO Bool
hasOutputFile = do
  mv <- getOpt ["output-file","o"]
  return $
    case mv of
      Nothing  -> False
      Just "-" -> False
      _        -> True

-- -- | Get the -o/--output-file option's value, if any, from the command line arguments.
-- -- Uses getOpt; sticky in GHCI until reloaded, may not always be affected by a hledger config file, etc.
-- outputFileOption :: IO (Maybe String)
-- outputFileOption = getOpt ["output-file","o"]



-- Terminal size

-- [NOTE: Alternative methods of getting the terminal size]
-- terminal-size uses the TIOCGWINSZ ioctl to get the window size on Unix
-- systems, which may not be completely portable according to people in
-- #linux@liberachat.
--
-- If this turns out to be the case, supplementary coverage can be given by
-- using the terminfo package.
--
-- Conversely, terminfo on its own is not a full solution, firstly because it
-- only works on Unix (not Windows), and secondly since in some scenarios (eg
-- stripped-down build systems) the terminfo database may be limited and lack
-- the correct entries. (A hack that sometimes works but which isn't robust
-- enough to be relied upon is to set TERM=dumb -- while this advice does appear
-- in some places, it's not guaranteed to work)
--
-- In any case, $LINES/$COLUMNS should not be used as a source for the terminal
-- size - they are not available or do not update reliably in all shells.
--
-- See #2332 for details

-- | An alternative to ansi-terminal's getTerminalSize, based on
-- the more robust-looking terminal-size package.
--
-- Tries to get stdout's terminal's current height and width.
getTerminalHeightWidth :: IO (Maybe (Int,Int))
getTerminalHeightWidth = fmap (fmap unwindow) size
  where unwindow (Window h w) = (h,w)

getTerminalHeight :: IO (Maybe Int)
getTerminalHeight = fmap fst <$> getTerminalHeightWidth

getTerminalWidth :: IO (Maybe Int)
getTerminalWidth  = fmap snd <$> getTerminalHeightWidth



-- Pager output
-- somewhat hledger-specific

-- Configure some preferred options for the `less` pager,
-- by modifying the LESS environment variable in this program's environment.
-- If you are using some other pager, this will have no effect.
-- By default, this sets the following options, appending them to LESS's current value:
--
--   --chop-long-lines
--   --hilite-unread
--   --ignore-case
--   --mouse
--   --no-init
--   --quit-at-eof
--   --quit-if-one-screen
--   --RAW-CONTROL-CHARS
--   --shift=8
--   --squeeze-blank-lines
--   --use-backslash
--
-- You can choose different options by setting the HLEDGER_LESS variable;
-- if set, its value will be used instead of LESS.
-- Or you can force hledger to use your exact LESS settings,
-- by setting HLEDGER_LESS equal to LESS.
--
setupPager :: IO ()
setupPager = do
  let
    -- keep synced with doc above
    deflessopts = unwords [
       "--chop-long-lines"
      ,"--hilite-unread"
      ,"--ignore-case"
      ,"--mouse"
      ,"--no-init"
      ,"--quit-at-eof"
      ,"--quit-if-one-screen"
      ,"--RAW-CONTROL-CHARS"
      ,"--shift=8"
      ,"--squeeze-blank-lines"
      ,"--use-backslash"
      -- ,"--use-color"  #2335 rejected by older less versions (eg 551)
      ]
  mhledgerless <- lookupEnv "HLEDGER_LESS"
  mless        <- lookupEnv "LESS"
  setEnv "LESS" $
    case (mhledgerless, mless) of
      (Just hledgerless, _) -> hledgerless
      (_, Just less)        -> if deflessopts `isInfixOf` less then less else unwords [less, deflessopts]
      _                     -> deflessopts

-- | Display the given text on the terminal, trying to use a pager ($PAGER, less, or more)
-- when appropriate, otherwise printing to standard output. Uses maybePagerFor.
--
-- hledger's output may contain ANSI style/color codes
-- (if the terminal supports them and they are not disabled by --color=no or NO_COLOR),
-- so the pager should be configured to handle these.
-- setupPager tries to configure that automatically when using the `less` pager.
--
runPager :: String -> IO ()
runPager s = do
  mpager <- maybePagerFor s
  case mpager of
    Nothing -> putStr s
    Just pager -> do
      withCreateProcess (shell pager){std_in=CreatePipe} $
        \mhin _ _ p -> do
          -- Pipe in the text on stdin.
          case mhin of
            Nothing  -> return ()  -- shouldn't happen
            Just hin -> void $ forkIO $   -- Write from another thread to avoid deadlock ? Maybe unneeded, but just in case.
              (hPutStr hin s >> hClose hin)  -- Be sure to close the pipe so the pager knows we're done.
                -- If the pager quits early, we'll receive an EPIPE error; hide that.
                `catch` \(e::IOException) -> case e of
                  IOError{ioe_type=ResourceVanished, ioe_errno=Just ioe, ioe_handle=Just hdl} | Errno ioe==ePIPE, hdl==hin
                    -> return ()
                  _ -> throwIO e
          void $ waitForProcess p

-- | Should a pager be used for displaying the given text on stdout, and if so, which one ?
-- Uses a pager if findPager finds one and none of the following conditions are true:
-- We're running in a native MS Windows environment like cmd or powershell.
-- Or the --pager=n|no option is in effect.
-- Or the -o/--output-file option is in effect.
-- Or INSIDE_EMACS is set, to something other than "vterm".
-- Or the terminal's current height and width can't be detected.
-- Or the output text is less wide and less tall than the terminal.
-- Throws an error if the --pager option's value could not be parsed.
maybePagerFor :: String -> IO (Maybe String)
maybePagerFor output = do
  let
    ls = lines output
    oh = length ls
    ow = maximumDef 0 $ map length ls
    windows = os == "mingw32"
  pagerno    <- maybe False (not . either error' id . parseYN) <$> getOpt ["pager"]
  outputfile <- hasOutputFile
  emacsterm  <- lookupEnv "INSIDE_EMACS" <&> (`notElem` [Nothing, Just "vterm"])
  mhw        <- getTerminalHeightWidth
  mpager     <- findPager
  return $ do
    guard $ not $ windows || pagerno || outputfile || emacsterm
    (th,tw) <- mhw
    guard $ oh > th || ow > tw
    mpager

-- | Try to find a pager executable robustly, safely handling various error conditions
-- like an unset PATH var or the specified pager not being found as an executable.
-- The pager can be specified by a path or program name in the PAGER environment variable.
-- If that is unset or has a problem, "less" is tried, then "more".
-- If successful, the pager's path or program name is returned.
findPager :: IO (Maybe String)  -- XXX probably a ByteString in fact ?
findPager = do
  mpagervar <- lookupEnv "PAGER"
  let pagers = [p | Just p <- [mpagervar]] <> ["less", "more"]
  headMay . catMaybes <$> mapM findExecutable pagers



-- ANSI colour/styles
-- Some of these use unsafePerformIO to read info.

-- hledger-specific:

-- | Get the value of the rightmost --color or --colour option from the program's command line arguments.
-- Throws an error if the option's value could not be parsed.
colorOption :: IO YNA
colorOption = maybe Auto (either error' id . parseYNA) <$> getOpt ["color","colour"]

-- | Should ANSI color and styles be used with this output handle ?
-- Considers colorOption, the NO_COLOR environment variable, and hSupportsANSIColor.
useColorOnHandle :: Handle -> IO Bool
useColorOnHandle h = do
  no_color       <- isJust <$> lookupEnv "NO_COLOR"
  supports_color <- hSupportsANSIColor h
  yna            <- colorOption
  return $ yna==Yes || (yna==Auto && not no_color && supports_color)

-- | Should ANSI color and styles be used for standard output ?
-- Considers useColorOnHandle stdout and hasOutputFile.
useColorOnStdout :: IO Bool
useColorOnStdout = do
  nooutputfile <- not <$> hasOutputFile
  usecolor <- useColorOnHandle stdout
  return $ nooutputfile && usecolor

-- | Should ANSI color and styles be used for standard error output ?
-- Considers useColorOnHandle stderr; is not affected by an --output-file option.
useColorOnStderr :: IO Bool
useColorOnStderr = useColorOnHandle stderr

-- | Like useColorOnStdout, but using unsafePerformIO. Useful eg for low-level debug code.
-- Sticky in GHCI until reloaded, may not always be affected by --color in a hledger config file, etc.
useColorOnStdoutUnsafe :: Bool
useColorOnStdoutUnsafe = unsafePerformIO useColorOnStdout

-- | Like useColorOnStdoutUnsafe, but for stderr.
useColorOnStderrUnsafe :: Bool
useColorOnStderrUnsafe = unsafePerformIO useColorOnStderr

-- | Detect whether ANSI should be used on stdout using useColorOnStdoutUnsafe,
-- and if so prepend and append the given SGR codes to a string.
-- Currently used in a few places (the commands list, the demo command, the recentassertions error message);
-- see useColorOnStdoutUnsafe's limitations.
ansiWrapUnsafe :: SGRString -> SGRString -> String -> String
ansiWrapUnsafe pre post s = if useColorOnStdoutUnsafe then pre<>s<>post else s

type SGRString = String

sgrbold          = setSGRCode [SetConsoleIntensity BoldIntensity]
sgrfaint         = setSGRCode [SetConsoleIntensity FaintIntensity]
sgrnormal        = setSGRCode [SetConsoleIntensity NormalIntensity]
sgrresetfg       = setSGRCode [SetDefaultColor Foreground]
sgrresetbg       = setSGRCode [SetDefaultColor Background]
sgrresetall      = sgrresetfg <> sgrresetbg <> sgrnormal
sgrblack         = setSGRCode [SetColor Foreground Dull Black]
sgrred           = setSGRCode [SetColor Foreground Dull Red]
sgrgreen         = setSGRCode [SetColor Foreground Dull Green]
sgryellow        = setSGRCode [SetColor Foreground Dull Yellow]
sgrblue          = setSGRCode [SetColor Foreground Dull Blue]
sgrmagenta       = setSGRCode [SetColor Foreground Dull Magenta]
sgrcyan          = setSGRCode [SetColor Foreground Dull Cyan]
sgrwhite         = setSGRCode [SetColor Foreground Dull White]
sgrbrightblack   = setSGRCode [SetColor Foreground Vivid Black]
sgrbrightred     = setSGRCode [SetColor Foreground Vivid Red]
sgrbrightgreen   = setSGRCode [SetColor Foreground Vivid Green]
sgrbrightyellow  = setSGRCode [SetColor Foreground Vivid Yellow]
sgrbrightblue    = setSGRCode [SetColor Foreground Vivid Blue]
sgrbrightmagenta = setSGRCode [SetColor Foreground Vivid Magenta]
sgrbrightcyan    = setSGRCode [SetColor Foreground Vivid Cyan]
sgrbrightwhite   = setSGRCode [SetColor Foreground Vivid White]
sgrrgb r g b     = setSGRCode [SetRGBColor Foreground $ sRGB r g b]

-- | Set various ANSI styles/colours in a string, only if useColorOnStdoutUnsafe says we should.
bold' :: String -> String
bold'  = ansiWrapUnsafe sgrbold sgrnormal

faint' :: String -> String
faint'  = ansiWrapUnsafe sgrfaint sgrnormal

black' :: String -> String
black'  = ansiWrapUnsafe sgrblack sgrresetfg

red' :: String -> String
red'  = ansiWrapUnsafe sgrred sgrresetfg

green' :: String -> String
green'  = ansiWrapUnsafe sgrgreen sgrresetfg

yellow' :: String -> String
yellow'  = ansiWrapUnsafe sgryellow sgrresetfg

blue' :: String -> String
blue'  = ansiWrapUnsafe sgrblue sgrresetfg

magenta' :: String -> String
magenta'  = ansiWrapUnsafe sgrmagenta sgrresetfg

cyan' :: String -> String
cyan'  = ansiWrapUnsafe sgrcyan sgrresetfg

white' :: String -> String
white'  = ansiWrapUnsafe sgrwhite sgrresetfg

brightBlack' :: String -> String
brightBlack'  = ansiWrapUnsafe sgrbrightblack sgrresetfg

brightRed' :: String -> String
brightRed'  = ansiWrapUnsafe sgrbrightred sgrresetfg

brightGreen' :: String -> String
brightGreen'  = ansiWrapUnsafe sgrbrightgreen sgrresetfg

brightYellow' :: String -> String
brightYellow'  = ansiWrapUnsafe sgrbrightyellow sgrresetfg

brightBlue' :: String -> String
brightBlue'  = ansiWrapUnsafe sgrbrightblue sgrresetfg

brightMagenta' :: String -> String
brightMagenta'  = ansiWrapUnsafe sgrbrightmagenta sgrresetfg

brightCyan' :: String -> String
brightCyan'  = ansiWrapUnsafe sgrbrightcyan sgrresetfg

brightWhite' :: String -> String
brightWhite'  = ansiWrapUnsafe sgrbrightwhite sgrresetfg

rgb' :: Float -> Float -> Float -> String -> String
rgb' r g b  = ansiWrapUnsafe (sgrrgb r g b) sgrresetfg

-- Generic:

-- | Wrap a string in ANSI codes to set and reset foreground colour.
-- ColorIntensity is @Dull@ or @Vivid@ (ie normal and bold).
-- Color is one of @Black@, @Red@, @Green@, @Yellow@, @Blue@, @Magenta@, @Cyan@, @White@.
-- Eg: @color Dull Red "text"@.
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

-- | Detect the terminal's current foreground or background colour, if possible, using unsafePerformIO.
{-# NOINLINE terminalColor #-}
terminalColor :: ConsoleLayer -> Maybe (RGB Float)
terminalColor = unsafePerformIO . getLayerColor'

-- A version of ansi-terminal's getLayerColor that is less likely to leak escape sequences to output,
-- and that returns a RGB of Floats (0..1) that is more compatible with the colour package.
-- This does nothing in a non-interactive context (eg when piping stdout to another command),
-- inside emacs (emacs shell buffers show the escape sequence for some reason),
-- or in a non-colour-supporting terminal.
getLayerColor' :: ConsoleLayer -> IO (Maybe (RGB Float))
getLayerColor' l = do
  inemacs       <- not.null <$> lookupEnv "INSIDE_EMACS"
  interactive   <- hIsTerminalDevice stdout
  supportscolor <- hSupportsANSIColor stdout
  if inemacs || not interactive || not supportscolor then return Nothing
  else fmap fractionalRGB <$> getLayerColor l
  where
    fractionalRGB :: (Fractional a) => RGB Word16 -> RGB a
    fractionalRGB (RGB r g b) = RGB (fromIntegral r / 65535) (fromIntegral g / 65535) (fromIntegral b / 65535)  -- chatgpt

