{-|
Read extra CLI arguments from a hledger config file.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}

module Hledger.Cli.Conf (
   Conf
  ,SectionName
  ,getConf
  ,getConf'
  ,nullconf
  ,confLookup
  ,activeConfFile
  ,activeLocalConfFile
  ,activeUserConfFile
  ,confFiles
  ,userConfFiles
  ,parseConf
)
where

import Control.Exception (handle)
import Control.Monad (void, forM)
import Control.Monad.Identity (Identity)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Safe (headMay, lastDef)
import System.Directory (getHomeDirectory, getXdgDirectory, XdgDirectory (XdgConfig), doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import Text.Megaparsec as M
import Text.Megaparsec.Char

import Hledger (error', strip, words', RawOpts, expandPath)
import Hledger.Read.Common
import Hledger.Utils.Parse
import Hledger.Utils.Debug
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
-- If a specified file, or the first file found, can not be read or parsed, this returns an error message.
-- Otherwise this returns the parsed Conf, and the file path.
getConf :: RawOpts -> IO (Either String (Conf, Maybe FilePath))
getConf rawopts = do
  -- As in Cli.hs, conf debug output always goes to stderr;
  -- that's ok as conf is a hledger cli feature for now.
  case confFileSpecFromRawOpts rawopts of
    NoConfFile     -> return $ Right $ traceAt 1 "ignoring config files" (nullconf, Nothing)
    SomeConfFile f -> getCurrentDirectory >>= flip expandPath f >>= readConfFile . dbg1 "using specified config file"
    AutoConfFile   -> do
      fs <- confFiles
      case fs of
        f:_ -> dbg8IO "found config files" fs >> dbg1IO "using nearest config file" f >> readConfFile f
        []  -> return $ Right $ traceAt 1 "no config file found" (nullconf, Nothing)

-- | Like getConf but throws an error on failure.
getConf' :: RawOpts -> IO (Conf, Maybe FilePath)
getConf' rawopts = getConf rawopts >>= either (error' . show) return

-- | Read this config file and parse its contents, or return an error message.
readConfFile :: FilePath -> IO (Either String (Conf, Maybe FilePath))
readConfFile f = handle (\(e::IOError) -> return $ Left $ show e) $ do
  -- avoid GHC 9.10.1's ugly stack trace when calling readFile on a nonexistent file
  exists <- doesFileExist f
  case exists of
    False -> return $ Left $ f <> " does not exist"
    True -> do
      ecs <- readFile f <&> parseConf f . T.pack
      case ecs of
        Left err -> return $ Left $ errorBundlePretty err -- customErrorBundlePretty err
        Right cs -> return $ Right (nullconf{
          confFile     = f
          ,confFormat   = 1
          ,confSections = cs
          },
          Just f
          )

-- -- | Like readConf, but throw an error on failure.
-- readConfFile' :: FilePath -> IO (Conf, Maybe FilePath)
-- readConfFile' f = readConfFile f >>= either (error' . show) return

-- | Get the highest precedence config file, based on the current directory.
activeConfFile :: IO (Maybe FilePath)
activeConfFile = headMay <$> confFiles

-- | Get the highest precedence local config file: 
-- a config file in the current directory or above, that is not a user-wide config file.
activeLocalConfFile :: IO (Maybe FilePath)
activeLocalConfFile = do
  ufs <- userConfFiles
  mf <- headMay <$> confFiles
  return $ case mf of
    Just f | f `notElem` ufs -> Just f
    _ -> Nothing

-- | Get the highest precedence user-wide config file, based on the current directory.
-- (This may not be the active config file.)
activeUserConfFile :: IO (Maybe FilePath)
activeUserConfFile = headMay <$> userConfFiles

-- | Get the possibleConfFiles which exist, based on the current directory.
confFiles :: IO [FilePath]
confFiles = possibleConfFiles >>= existingFiles

-- | Get the possibleUserConfFiles which exist, based on the current directory.
userConfFiles :: IO [FilePath]
userConfFiles = possibleUserConfFiles >>= existingFiles

-- | Filter a list of paths to just the existing files.
existingFiles :: [FilePath] -> IO [FilePath]
existingFiles fs =
  fmap catMaybes $ forM fs $ \f -> do
    exists <- doesFileExist f
    return $ if exists then Just f else Nothing

-- | Get the possible paths for a hledger config file, highest precedence first:
-- hledger.conf in the current directory, 
-- hledger.conf in any parent directory, 
-- .hledger.conf in the home directory,
-- or hledger.conf in the XdgConfig directory.
possibleConfFiles :: IO [FilePath]
possibleConfFiles = do
  ds   <- possibleConfDirs
  home <- getHomeDirectory
  return $ dbg8 "possible config file paths" $
    flip map ds $ \d -> d </> if d==home then ".hledger.conf" else "hledger.conf"

-- | Like possibleConfFiles, but consider only user-wide hledger config files:
-- .hledger.conf in the home directory,
-- or hledger.conf in the XdgConfig directory.
possibleUserConfFiles :: IO [FilePath]
possibleUserConfFiles = do
  home <- getHomeDirectory
  xdgc <- getXdgDirectory XdgConfig "hledger"
  let ds = [home,xdgc]
  return $ dbg8 "possible user config file paths" $
    flip map ds $ \d -> d </> if d==home then ".hledger.conf" else "hledger.conf"

-- | Get the directories where a hledger config file could be, highest precedence first:
-- the current directory, any parent directory, the home directory, or the XdgConfig directory.
possibleConfDirs :: IO [FilePath]
possibleConfDirs = do
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
-- dp = dbgparse 0  -- trace parse state at this --debug level

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
  restoflinep <|> whitespacep  -- whitespace / same-line comment, possibly with no newline
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
