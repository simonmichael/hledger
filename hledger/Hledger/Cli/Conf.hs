{-|
Read extra CLI arguments from a hledger config file.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}

module Hledger.Cli.Conf (
   getConf
  ,confLookup
)
where

import Control.Monad (void, forM, when)
import Control.Monad.Identity (Identity)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import System.Directory (getHomeDirectory, getXdgDirectory, XdgDirectory (XdgConfig), doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import Text.Megaparsec
import Text.Megaparsec.Char

import Hledger (error', strip, words', RawOpts, expandPath)
import Hledger.Read.Common
import Hledger.Utils.Parse
import Hledger.Utils.Debug
import Safe (lastDef)
import Hledger.Data.RawOptions (collectopts)


-- | A hledger config file.
data Conf = Conf {
   confFile :: FilePath
  -- ,confText :: String
  ,confFormat :: Int
  ,confSections :: [ConfSection]
} deriving (Eq,Show)

-- | One section in a hledger config file.
data ConfSection = ConfSection {
   csName :: SectionName
  ,csArgs :: [Arg]
} deriving (Eq,Show)

-- | The name of a config file section, with surrounding brackets and whitespace removed.
type SectionName = String

-- | A command line argument to be passed to CmdArgs.process.
-- It seems this should be a single command line argument (or flag or flag value).
-- If it contains spaces, those are treated as part of a single argument, as with CMD a "b c".
type Arg = String

nullconf = Conf {
   confFile = ""
  ,confFormat = 1
  ,confSections = []
}

-- | The --conf or --no-conf or default config file specified by command line options.
data ConfFileSpec =
    SomeConfFile FilePath  -- ^ use config file specified with --conf
  | NoConfFile             -- ^ don't use any config file (--no-conf)
  | AutoConfFile           -- ^ use the config file found by directory search (default)
  deriving (Eq,Show)

-- Get the conf file specification from options,
-- considering the rightmost --conf or --no-conf option if any.
confFileSpecFromRawOpts :: RawOpts -> ConfFileSpec
confFileSpecFromRawOpts = lastDef AutoConfFile . collectopts cfsFromRawOpt
  where
    cfsFromRawOpt ("conf",f)    = Just $ SomeConfFile f
    cfsFromRawOpt ("no-conf",_) = Just $ NoConfFile
    cfsFromRawOpt _             = Nothing

-- config reading

-- | Fetch all the arguments/options defined in a section with this name, if it exists.
-- This should be "general" for the unnamed first section, or a hledger command name.
confLookup :: SectionName -> Conf -> [Arg]
confLookup cmd Conf{confSections} =
  maybe [] (concatMap words') $  -- XXX PARTIAL
  M.lookup cmd $
  M.fromList [(csName,csArgs) | ConfSection{csName,csArgs} <- confSections]

-- | Try to read a hledger config from a config file specified by --conf,
-- or the first config file found in any of several default file paths.
-- If --no-conf was used, or if no file was specified or found, this returns a null Conf.
-- If a specified file, or the first file found, can not be read or parsed, this raises an error.
-- Otherwise this returns the parsed Conf, and the file path.
getConf :: RawOpts -> IO (Conf, Maybe FilePath)
getConf rawopts = do
  -- As in Cli.hs, conf debug output always goes to stderr;
  -- that's ok as conf is a hledger cli feature for now.
  case confFileSpecFromRawOpts rawopts of
    NoConfFile     -> return $ traceAt 1 "ignoring config files" (nullconf, Nothing)
    SomeConfFile f -> getCurrentDirectory >>= flip expandPath f >>= readConfFile . dbg1 "using specified config file"
    AutoConfFile   -> do
      defconfpaths <- defaultConfFilePaths
      conffiles <- fmap catMaybes $ forM defconfpaths $ \f -> do
        exists <- doesFileExist f
        return $ if exists then Just f else Nothing
      case conffiles of
        f:_ -> dbg8IO "found config files" conffiles >> dbg1IO "using nearest config file" f >> readConfFile f
        []  -> return $ traceAt 1 "no config file found" (nullconf, Nothing)

-- | Read this config file and parse its contents, or raise an error.
readConfFile :: FilePath -> IO (Conf, Maybe FilePath)
readConfFile f = do
  -- avoid GHC 9.10.1's ugly stack trace when calling readFile on a nonexistent file
  exists <- doesFileExist f
  when (not exists) $ error' $ f <> " does not exist"

  ecs <- readFile f <&> parseConf f . T.pack
  case ecs of
    Left err -> error' $ errorBundlePretty err -- customErrorBundlePretty err
    Right cs -> return (nullconf{
       confFile     = f
      ,confFormat   = 1
      ,confSections = cs
      },
      Just f
      )

-- | Get the possible paths for a hledger config file, depending on the current directory.
defaultConfFilePaths :: IO [FilePath]
defaultConfFilePaths = do
  ds   <- confDirs
  home <- getHomeDirectory
  return $ dbg8 "possible config file paths" $
    flip map ds $ \d -> d </> if d==home then ".hledger.conf" else "hledger.conf"

-- | Get the directories to check for a hledger config file.
confDirs :: IO [FilePath]
confDirs = do
  xdgc <- getXdgDirectory XdgConfig "hledger"
  home <- getHomeDirectory
  here <- getCurrentDirectory
  dirs <- getDirsUpToRoot here
  let dirs2 = if home `elem` dirs then dirs else dirs <> [home]
  let dirs3 = if xdgc `elem` dirs2 then dirs2 else dirs2 <> [xdgc]
  return $ dbg8 "searching config dirs" dirs3

-- | Get this directory and all of its parents up to /.
getDirsUpToRoot :: FilePath -> IO [FilePath]
getDirsUpToRoot dir = return $ go [] dir
  where
    go seen d = if
      | d `elem` seen || length seen >= 100 -> []  -- just in case
      | d=="/"    -> [d]
      | otherwise -> d : go (d:seen) (takeDirectory d)

-- config file parsing

parseConf :: FilePath -> Text -> Either (ParseErrorBundle Text HledgerParseErrorData) [ConfSection]
parseConf = runParser confp

dp :: String -> TextParser m ()
dp = const $ return ()  -- no-op
-- dp = dbgparse 1  -- trace parse state at this --debug level

whitespacep, commentlinesp, restoflinep :: TextParser Identity ()
whitespacep   = void $ {- dp "whitespacep"   >> -} many spacenonewline
commentlinesp = void $ {- dp "commentlinesp" >> -} many (emptyorcommentlinep2 "#")
restoflinep   = void $ {- dp "restoflinep"   >> -} whitespacep >> emptyorcommentlinep2 "#"

confp :: TextParser Identity [ConfSection]  -- a monadic TextParser to allow reusing other hledger parsers
confp = do
  dp "confp"
  commentlinesp
  genas <- many arglinep
  let s = ConfSection "general" genas
  ss <- many $ do
    (n, ma) <- sectionstartp
    as <- many arglinep
    return $ ConfSection n (maybe as (:as) ma)
  eof
  return $ s:ss

-- parse a section name and possibly arguments written on the same line
sectionstartp :: TextParser Identity (String, Maybe String)
sectionstartp = do
  dp "sectionstartp"
  char '['
  n <- fmap strip $ some $ noneOf "]#\n"
  char ']'
  -- dp "sectionstartp2"
  whitespacep
  -- dp "sectionstartp3"
  ma <- fmap (fmap strip) $ optional $ some $ noneOf "#\n"
  -- dp "sectionstartp4"
  restoflinep
  -- dp "sectionstartp5"
  commentlinesp
  -- dp "sectionstartp6"
  return (n, ma)

arglinep :: TextParser Identity String
arglinep = do
  dp "arglinep"
  notFollowedBy $ char '['
  -- dp "arglinep2"
  whitespacep
  -- dp "arglinep3"
  a <- some $ noneOf "#\n"
  -- dp "arglinep4"
  restoflinep
  commentlinesp
  return $ strip a


-- initialiseAndParseJournal :: ErroringJournalParser IO ParsedJournal -> InputOpts
--                           -> FilePath -> Text -> ExceptT String IO Journal
-- initialiseAndParseJournal parser iopts f txt =
--     prettyParseErrors $ runParserT (evalStateT parser initJournal) f txt
--   where
--     y = first3 . toGregorian $ _ioDay iopts
--     initJournal = nulljournal{jparsedefaultyear = Just y, jincludefilestack = [f]}
--     -- Flatten parse errors and final parse errors, and output each as a pretty String.
--     prettyParseErrors :: ExceptT FinalParseError IO (Either (ParseErrorBundle Text HledgerParseErrorData) a)
--                       -> ExceptT String IO a
--     prettyParseErrors = withExceptT customErrorBundlePretty . liftEither
--                     <=< withExceptT (finalErrorBundlePretty . attachSource f txt)
