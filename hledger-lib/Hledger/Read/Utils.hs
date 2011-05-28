{-|
Utilities common to hledger journal readers.
-}

module Hledger.Read.Utils
where

import Control.Monad.Error
import Data.List
import System.Directory (getHomeDirectory)
import System.FilePath(takeDirectory,combine)
import System.Time (getClockTime)
import Text.ParserCombinators.Parsec

import Hledger.Data.Types (Journal, JournalContext(..), Commodity, JournalUpdate)
import Hledger.Utils
import Hledger.Data.Dates (getCurrentYear)
import Hledger.Data.Journal (nullctx, nulljournal, journalFinalise)


juSequence :: [JournalUpdate] -> JournalUpdate
juSequence us = liftM (foldr (.) id) $ sequence us

-- | Given a JournalUpdate-generating parsec parser, file path and data string,
-- parse and post-process a Journal so that it's ready to use, or give an error.
parseJournalWith :: (GenParser Char JournalContext (JournalUpdate,JournalContext)) -> FilePath -> String -> ErrorT String IO Journal
parseJournalWith p f s = do
  tc <- liftIO getClockTime
  tl <- liftIO getCurrentLocalTime
  y <- liftIO getCurrentYear
  case runParser p nullctx{ctxYear=Just y} f s of
    Right (updates,ctx) -> do
                           j <- updates `ap` return nulljournal
                           case journalFinalise tc tl f s ctx j of
                             Right j'  -> return j'
                             Left estr -> throwError estr
    Left e -> throwError $ show e

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
