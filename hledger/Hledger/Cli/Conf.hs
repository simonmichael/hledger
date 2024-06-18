{-|
Read extra CLI arguments from a hledger config file.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Cli.Conf (
   getConf
  ,confLookup
)
where

import Control.Exception (IOException, catch, tryJust)
import Control.Monad (guard, void)
import Control.Monad.Identity (Identity)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T (pack)
import System.IO.Error (isDoesNotExistError)
import Text.Megaparsec -- hiding (parse)
import Text.Megaparsec.Char

import Hledger (error', strip, words')
import Hledger.Read.Common
import Hledger.Utils.Parse


localConfPath = "hledger.conf"

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

-- | Try to read a hledger config file.
-- If none is found, this returns a null Conf.
-- Any other IO error will cause an exit.
getConf :: IO Conf
getConf = (do
  let f = localConfPath
  es <- tryJust (guard . isDoesNotExistError) $ readFile f
  case es of
    Left _ -> return nullconf
    Right s ->
      case parseConf f (T.pack s) of
        Left err -> error' $ errorBundlePretty err -- customErrorBundlePretty err
        Right ss -> return nullconf{
           confFile     = f
          ,confText     = s
          ,confFormat   = 1
          ,confSections = ss
          }
  ) `catch` \(e :: IOException) -> error' $ show e

-- config file parsing

parseConf :: FilePath -> Text -> Either (ParseErrorBundle Text HledgerParseErrorData) [ConfSection]
parseConf = runParser confp

dp :: String -> TextParser m ()
dp = const $ return ()  -- no-op
-- dp = dbgparse 1  -- trace parse state at this --debug level

whitespacep, commentlinesp, restoflinep :: TextParser Identity ()
whitespacep   = void $ {- dp "whitespacep"   >> -} many spacenonewline
commentlinesp = void $ {- dp "commentlinesp" >> -} many (emptyorcommentlinep' "#")
restoflinep   = void $ {- dp "restoflinep"   >> -} whitespacep >> emptyorcommentlinep' "#"

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
