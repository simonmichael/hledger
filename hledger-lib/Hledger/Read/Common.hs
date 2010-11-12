{-|

Common utilities for hledger data readers, such as the context (state)
that is kept while parsing a journal.

-}

module Hledger.Read.Common
where

import Control.Monad.Error
import Hledger.Data.Utils
import Hledger.Data.Types (Journal, Commodity)
import Hledger.Data.Journal
import System.Directory (getHomeDirectory)
import System.FilePath(takeDirectory,combine)
import System.Time (getClockTime)
import Text.ParserCombinators.Parsec


-- | A hledger data reader is a triple of format name, format-detecting predicate, and a parser to Journal.
data Reader = Reader {rFormat   :: String
                     ,rDetector :: FilePath -> String -> Bool
                     ,rParser   :: FilePath -> String -> ErrorT String IO Journal
                     }

-- | A JournalUpdate is some transformation of a "Journal". It can do I/O
-- or raise an error.
type JournalUpdate = ErrorT String IO (Journal -> Journal)

juSequence :: [JournalUpdate] -> JournalUpdate
juSequence us = liftM (foldr (.) id) $ sequence us

-- | Given a JournalUpdate-generating parsec parser, file path and data string,
-- parse and post-process a Journal so that it's ready to use, or give an error.
parseJournalWith :: (GenParser Char JournalContext JournalUpdate) -> FilePath -> String -> ErrorT String IO Journal
parseJournalWith p f s = do
  tc <- liftIO getClockTime
  tl <- liftIO getCurrentLocalTime
  case runParser p emptyCtx f s of
    Right updates -> liftM (journalFinalise tc tl f s) $ updates `ap` return nulljournal
    Left err      -> throwError $ show err

-- | Some state kept while parsing a journal file.
data JournalContext = Ctx {
      ctxYear      :: !(Maybe Integer)   -- ^ the default year most recently specified with Y
    , ctxCommodity :: !(Maybe Commodity) -- ^ the default commodity recently specified with D
    , ctxAccount   :: ![String]          -- ^ the current stack of parent accounts specified by !account
    } deriving (Read, Show)

emptyCtx :: JournalContext
emptyCtx = Ctx { ctxYear = Nothing, ctxCommodity = Nothing, ctxAccount = [] }

setYear :: Integer -> GenParser tok JournalContext ()
setYear y = updateState (\ctx -> ctx{ctxYear=Just y})

getYear :: GenParser tok JournalContext (Maybe Integer)
getYear = liftM ctxYear getState

setCommodity :: Commodity -> GenParser tok JournalContext ()
setCommodity c = updateState (\ctx -> ctx{ctxCommodity=Just c})

getCommodity :: GenParser tok JournalContext (Maybe Commodity)
getCommodity = liftM ctxCommodity getState

pushParentAccount :: String -> GenParser tok JournalContext ()
pushParentAccount parent = updateState addParentAccount
    where addParentAccount ctx0 = ctx0 { ctxAccount = normalize parent : ctxAccount ctx0 }
          normalize = (++ ":") 

popParentAccount :: GenParser tok JournalContext ()
popParentAccount = do ctx0 <- getState
                      case ctxAccount ctx0 of
                        [] -> unexpected "End of account block with no beginning"
                        (_:rest) -> setState $ ctx0 { ctxAccount = rest }

getParentAccount :: GenParser tok JournalContext String
getParentAccount = liftM (concat . reverse . ctxAccount) getState

-- | Convert a possibly relative, possibly tilde-containing file path to an absolute one.
-- using the current directory from a parsec source position. ~username is not supported.
expandPath :: (MonadIO m) => SourcePos -> FilePath -> m FilePath
expandPath pos fp = liftM mkAbsolute (expandHome fp)
  where
    mkAbsolute = combine (takeDirectory (sourceName pos))
    expandHome inname | "~/" `isPrefixOf` inname = do homedir <- liftIO getHomeDirectory
                                                      return $ homedir ++ drop 1 inname
                      | otherwise                = return inname

fileSuffix :: FilePath -> String
fileSuffix = reverse . takeWhile (/='.') . reverse . dropWhile (/='.')
