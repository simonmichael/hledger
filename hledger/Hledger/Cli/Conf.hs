{-|
Read extra CLI arguments from a hledger config file.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Hledger.Cli.Conf (
   getConf
  ,confLookup
)
where

import Control.Monad (void, forM)
import Control.Monad.Identity (Identity)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Safe (headMay)
import System.Directory (getHomeDirectory, getXdgDirectory, XdgDirectory (XdgConfig), doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import Text.Megaparsec
import Text.Megaparsec.Char

import Hledger (error', strip, words', RawOpts, boolopt, maybestringopt, expandPath)
import Hledger.Read.Common
import Hledger.Utils.Parse
import Hledger.Utils.Debug


-- | A hledger config file.
data Conf = Conf {
   confFile :: FilePath
  ,confText :: String
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
  ,confText = ""
  ,confFormat = 1
  ,confSections = []
}

-- config reading

-- | Fetch all the arguments/options defined in a section with this name, if it exists.
-- This should be "general" for the unnamed first section, or a hledger command name.
confLookup :: SectionName -> Conf -> [Arg]
confLookup cmd Conf{confSections} =
  maybe [] (concatMap words') $  -- XXX PARTIAL
  M.lookup cmd $
  M.fromList [(csName,csArgs) | ConfSection{csName,csArgs} <- confSections]

-- | Try to read a hledger config file from several places,
-- or from a file specified by --conf,
-- returning its path and a Conf parsed from it.
-- If no config file is found, or --no-conf is used, this returns a null Conf.
-- Any other IO or parse failure will raise an error.
getConf :: RawOpts -> IO (Conf, Maybe FilePath)
getConf rawopts = do
  defconfpaths <- confFilePaths
  existingconfpaths <- fmap catMaybes $ forM defconfpaths $ \f -> do
    x <- doesFileExist f
    return $ if x then Just f else Nothing
  dbg1IO "found config files" existingconfpaths
  let noconf = boolopt "no-conf" rawopts
  let mconffile0 = maybestringopt "conf" rawopts
  mconffile <- case mconffile0 of
    Just f  -> Just <$> (getCurrentDirectory >>= flip expandPath f)
    Nothing -> return Nothing
  let confpaths = maybe defconfpaths (:[]) mconffile
  mconftxt <- readFirstConfFile confpaths
  if
    | noconf -> return $ traceAt 1 "ignoring config files" (nullconf, Nothing)
    | Nothing <- mconftxt -> return $ traceAt 1 "no config file found" (nullconf, Nothing)
    | Just (f,s) <- mconftxt ->
      case parseConf f (T.pack s) of
        Left err -> error' $ errorBundlePretty err -- customErrorBundlePretty err
        Right ss -> return (nullconf{
           confFile     = f
          ,confText     = s
          ,confFormat   = 1
          ,confSections = ss
          }, Just f)

-- | Get the possible paths for a hledger config file, depending on the current directory.
confFilePaths :: IO [FilePath]
confFilePaths = do
  ds   <- confDirs
  home <- getHomeDirectory
  return $ dbg8 "possible config files" $
    flip map ds $ \d -> d </> if d==home then ".hledger.conf" else "hledger.conf"

-- | Get the directories to check for a hledger config file.
confDirs :: IO [FilePath]
confDirs = do
  xdgc <- getXdgDirectory XdgConfig "hledger"
  home <- getHomeDirectory
  here <- getCurrentDirectory
  -- lowdirs <- getDirsUpToHomeOrRoot
  -- highdirs <-
  --   case lastMay lowdirs of
  --     Just d | d==home -> getDirsUpToRoot $ takeDirectory d
  --     _                -> return []
  -- let dirs = lowdirs <> [xdgdir] <> highdirs
  dirs <- getDirsUpToRoot here
  let dirs2 = if home `elem` dirs then dirs else dirs <> [home]
  let dirs3 = if xdgc `elem` dirs2 then dirs2 else dirs2 <> [xdgc]
  return $ dbg1 "searching config dirs" dirs3

-- -- | Get this directory and all of its parents up to ~ or /.
-- getDirsUpToHomeOrRoot :: IO [FilePath]
-- getDirsUpToHomeOrRoot = do
--   home <- getHomeDirectory
--   let
--     go d =
--       if d=="/" || d==home
--       then [d]
--       else d : go (takeDirectory d)
--   dbg1 "dirs up to home or root" .
--     go <$> getCurrentDirectory

-- | Get this directory and all of its parents up to /.
getDirsUpToRoot :: FilePath -> IO [FilePath]
getDirsUpToRoot dir = return $ go [] dir
  where
    go seen d = if
      | d `elem` seen || length seen >= 100 -> []  -- just in case
      | d=="/"    -> [d]
      | otherwise -> d : go (d:seen) (takeDirectory d)
  -- dbg1 "dirs up to root" .
  --   go <$> getCurrentDirectory

-- | Read the first of these files that exists.
readFirstConfFile :: [FilePath] -> IO (Maybe (FilePath, String))
readFirstConfFile fs = do
  let dd = dbg1With (("using config file: "<>).fst)
  mapM (fmap (fmap dd).readConfFile) fs <&> headMay . catMaybes

-- | Read this file and return its path and contents, if it exists.
readConfFile :: FilePath -> IO (Maybe (FilePath, String))
readConfFile f = do
  exists <- doesFileExist f
  if exists then readFile f <&> (Just.(f,)) else return Nothing

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
