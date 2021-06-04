{-|
A history-aware add command to help with data entry.
|-}

{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TypeOperators, FlexibleContexts, OverloadedStrings, PackageImports, LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Add (
   addmode
  ,add
  ,appendToJournalFileOrStdout
  ,journalAddTransaction
)
where

import Prelude ()
import "base-compat-batteries" Prelude.Compat hiding (fail)
import Control.Exception as E
import Control.Monad (when)
import Control.Monad.Trans.Class
import Control.Monad.State.Strict (evalState, evalStateT)
import Control.Monad.Trans (liftIO)
import Data.Char (toUpper, toLower)
import Data.Either (isRight)
import Data.Functor.Identity (Identity(..))
import "base-compat-batteries" Data.List.Compat
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Calendar (Day)
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)
import Safe (headDef, headMay, atMay)
import System.Console.CmdArgs.Explicit (flagNone)
import System.Console.Haskeline (runInputT, defaultSettings, setComplete)
import System.Console.Haskeline.Completion (CompletionFunc, completeWord, isFinished, noCompletion, simpleCompletion)
import System.Console.Wizard (Wizard, defaultTo, line, output, retryMsg, linePrewritten, nonEmpty, parser, run)
import System.Console.Wizard.Haskeline
import System.IO ( stderr, hPutStr, hPutStrLn )
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Register (postingsReportAsText)
import Hledger.Cli.Utils (journalSimilarTransaction)


addmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Add.txt")
  [flagNone ["no-new-accounts"]  (setboolopt "no-new-accounts") "don't allow creating new accounts"]
  [generalflagsgroup2]
  []
  ([], Just $ argsFlag "[DATE [DESCRIPTION [ACCOUNT1 [AMOUNT1 [ACCOUNT2 [ETC...]]]]]]")

-- | State used while entering transactions.
data EntryState = EntryState {
   esOpts               :: CliOpts           -- ^ command line options
  ,esArgs               :: [String]          -- ^ command line arguments remaining to be used as defaults
  ,esToday              :: Day               -- ^ today's date
  ,esDefDate            :: Day               -- ^ the default date for next transaction
  ,esJournal            :: Journal           -- ^ the journal we are adding to
  ,esSimilarTransaction :: Maybe Transaction -- ^ the most similar historical txn
  ,esPostings           :: [Posting]         -- ^ postings entered so far in the current txn
  } deriving (Show)

defEntryState = EntryState {
   esOpts               = defcliopts
  ,esArgs               = []
  ,esToday              = nulldate
  ,esDefDate            = nulldate
  ,esJournal            = nulljournal
  ,esSimilarTransaction = Nothing
  ,esPostings           = []
}

data RestartTransactionException = RestartTransactionException deriving (Show)
instance Exception RestartTransactionException

-- data ShowHelpException = ShowHelpException deriving (Show)
-- instance Exception ShowHelpException

-- | Read multiple transactions from the console, prompting for each
-- field, and append them to the journal file.  If the journal came
-- from stdin, this command has no effect.
add :: CliOpts -> Journal -> IO ()
add opts j
    | journalFilePath j == "-" = return ()
    | otherwise = do
        hPutStrLn stderr $ "Adding transactions to journal file " <> journalFilePath j
        showHelp
        today <- getCurrentDay
        let es = defEntryState{esOpts=opts
                              ,esArgs=listofstringopt "args" $ rawopts_ opts
                              ,esToday=today
                              ,esDefDate=today
                              ,esJournal=j
                              }
        getAndAddTransactions es `E.catch` (\(_::UnexpectedEOF) -> putStr "")

showHelp = hPutStr stderr $ unlines [
     "Any command line arguments will be used as defaults."
    ,"Use tab key to complete, readline keys to edit, enter to accept defaults."
    ,"An optional (CODE) may follow transaction dates."
    ,"An optional ; COMMENT may follow descriptions or amounts."
    ,"If you make a mistake, enter < at any prompt to go one step backward."
    ,"To end a transaction, enter . when prompted."
    ,"To quit, enter . at a date prompt or press control-d or control-c."
    ]

-- | Loop reading transactions from the console, prompting, validating
-- and appending each one to the journal file, until end of input or
-- ctrl-c (then raise an EOF exception).  If provided, command-line
-- arguments are used as defaults; otherwise defaults come from the
-- most similar recent transaction in the journal.
getAndAddTransactions :: EntryState -> IO ()
getAndAddTransactions es@EntryState{..} = (do
  let defaultPrevInput = PrevInput{prevDateAndCode=Nothing, prevDescAndCmnt=Nothing, prevAccount=[], prevAmountAndCmnt=[]}
  mt <- runInputT (setComplete noCompletion defaultSettings) (System.Console.Wizard.run $ haskeline $ confirmedTransactionWizard defaultPrevInput es [])
  case mt of
    Nothing -> error "Could not interpret the input, restarting"  -- caught below causing a restart, I believe  -- PARTIAL:
    Just t -> do
      j <- if debug_ esOpts > 0
           then do hPutStrLn stderr "Skipping journal add due to debug mode."
                   return esJournal
           else do j' <- journalAddTransaction esJournal esOpts t
                   hPutStrLn stderr "Saved."
                   return j'
      hPutStrLn stderr "Starting the next transaction (. or ctrl-D/ctrl-C to quit)"
      getAndAddTransactions es{esJournal=j, esDefDate=tdate t}
  )
  `E.catch` (\(_::RestartTransactionException) ->
                 hPutStrLn stderr "Restarting this transaction." >> getAndAddTransactions es)

data TxnParams = TxnParams
  { txnDate :: Day
  , txnCode :: Text
  , txnDesc :: Text
  , txnCmnt :: Text
  } deriving (Show)

data PrevInput = PrevInput
  { prevDateAndCode   :: Maybe String
  , prevDescAndCmnt   :: Maybe String
  , prevAccount       :: [String]
  , prevAmountAndCmnt :: [String]
  } deriving (Show)

data AddingStage = EnterDateAndCode
                 | EnterDescAndComment (Day, Text)
                 | EnterAccount TxnParams
                 | EnterAmountAndComment TxnParams String
                 | EndStage Transaction
                 | EnterNewPosting TxnParams (Maybe Posting)

confirmedTransactionWizard :: PrevInput -> EntryState -> [AddingStage] -> Wizard Haskeline Transaction
confirmedTransactionWizard prevInput es [] = confirmedTransactionWizard prevInput es [EnterDateAndCode]
confirmedTransactionWizard prevInput es@EntryState{..} stack@(currentStage : _) = case currentStage of
  EnterDateAndCode -> dateAndCodeWizard prevInput es >>= \case
    Just (date, code) -> do
      let es' = es
            { esArgs = drop 1 esArgs
            , esDefDate = date
            }
          dateAndCodeString = formatTime defaultTimeLocale yyyymmddFormat date
                            ++ T.unpack (if T.null code then "" else " (" <> code <> ")")
          yyyymmddFormat = iso8601DateFormat Nothing
      confirmedTransactionWizard prevInput{prevDateAndCode=Just dateAndCodeString} es' (EnterDescAndComment (date, code) : stack)
    Nothing ->
      confirmedTransactionWizard prevInput es stack

  EnterDescAndComment (date, code) -> descriptionAndCommentWizard prevInput es >>= \case
    Just (desc, comment) -> do
      let mbaset = journalSimilarTransaction esOpts esJournal desc
          es' = es
            { esArgs = drop 1 esArgs
            , esPostings = []
            , esSimilarTransaction = mbaset
            }
          descAndCommentString = T.unpack $ desc <> (if T.null comment then "" else "  ; " <> comment)
          prevInput' = prevInput{prevDescAndCmnt=Just descAndCommentString}
      when (isJust mbaset) . liftIO $ do
          hPutStrLn stderr "Using this similar transaction for defaults:"
          T.hPutStr stderr $ showTransaction (fromJust mbaset)
      confirmedTransactionWizard prevInput' es' ((EnterNewPosting TxnParams{txnDate=date, txnCode=code, txnDesc=desc, txnCmnt=comment} Nothing) : stack)
    Nothing ->
      confirmedTransactionWizard prevInput es (drop 1 stack)

  EnterNewPosting txnParams@TxnParams{..} posting -> case (esPostings, posting) of
    ([], Nothing) ->
      confirmedTransactionWizard prevInput es (EnterAccount txnParams : stack)
    (_, Just _) ->
      confirmedTransactionWizard prevInput es (EnterAccount txnParams : stack)
    (_, Nothing) -> do
      let t = nulltransaction{tdate=txnDate
                             ,tstatus=Unmarked
                             ,tcode=txnCode
                             ,tdescription=txnDesc
                             ,tcomment=txnCmnt
                             ,tpostings=esPostings
                             }
      case balanceTransaction balancingOpts t of -- imprecise balancing (?)
        Right t' ->
          confirmedTransactionWizard prevInput es (EndStage t' : stack)
        Left err -> do
          liftIO (hPutStrLn stderr $ "\n" ++ (capitalize err) ++ "please re-enter.")
          let notFirstEnterPost stage = case stage of
                EnterNewPosting _ Nothing -> False
                _ -> True
          confirmedTransactionWizard prevInput es{esPostings=[]} (dropWhile notFirstEnterPost stack)

  EnterAccount txnParams -> accountWizard prevInput es >>= \case
    Just account
      | account `elem` [".", ""] ->
          case (esPostings, postingsBalanced esPostings) of
            ([],_)    -> liftIO (hPutStrLn stderr "Please enter some postings first.") >> confirmedTransactionWizard prevInput es stack
            (_,False) -> liftIO (hPutStrLn stderr "Please enter more postings to balance the transaction.") >> confirmedTransactionWizard prevInput es stack
            (_,True)  -> confirmedTransactionWizard prevInput es (EnterNewPosting txnParams Nothing : stack)
      | otherwise -> do
          let prevAccount' = replaceNthOrAppend (length esPostings) account (prevAccount prevInput)
          confirmedTransactionWizard prevInput{prevAccount=prevAccount'} es{esArgs=drop 1 esArgs} (EnterAmountAndComment txnParams account : stack)
    Nothing -> do
      let notPrevAmountAndNotEnterDesc stage = case stage of
            EnterAmountAndComment _ _ -> False
            EnterDescAndComment _ -> False
            _ -> True
      confirmedTransactionWizard prevInput es{esPostings=init esPostings} (dropWhile notPrevAmountAndNotEnterDesc stack)

  EnterAmountAndComment txnParams account -> amountAndCommentWizard prevInput es >>= \case
    Just (amount, comment) -> do
      let posting = nullposting{paccount=T.pack $ stripbrackets account
                               ,pamount=mixedAmount amount
                               ,pcomment=comment
                               ,ptype=accountNamePostingType $ T.pack account
                               }
          amountAndCommentString = showAmount amount ++ T.unpack (if T.null comment then "" else "  ;" <> comment)
          prevAmountAndCmnt' = replaceNthOrAppend (length esPostings) amountAndCommentString (prevAmountAndCmnt prevInput)
          es' = es{esPostings=esPostings++[posting], esArgs=drop 2 esArgs}
      confirmedTransactionWizard prevInput{prevAmountAndCmnt=prevAmountAndCmnt'} es' (EnterNewPosting txnParams (Just posting) : stack)
    Nothing -> confirmedTransactionWizard prevInput es (drop 1 stack)

  EndStage t -> do
    output . T.unpack $ showTransaction t
    y <- let def = "y" in
         retryMsg "Please enter y or n." $
          parser ((fmap (\c -> if c == '<' then Nothing else Just c)) . headMay . map toLower . strip) $
          defaultTo' def $ nonEmpty $
          line $ green $ printf "Save this transaction to the journal ?%s: " (showDefault def)
    case y of
      Just 'y' -> return t
      Just _   -> throw RestartTransactionException
      Nothing  -> confirmedTransactionWizard prevInput es (drop 2 stack)
  where
    replaceNthOrAppend n newElem xs = take n xs ++ [newElem] ++ drop (n + 1) xs

dateAndCodeWizard PrevInput{..} EntryState{..} = do
  let def = headDef (T.unpack $ showDate esDefDate) esArgs
  retryMsg "A valid hledger smart date is required. Eg: 2014/2/14, 14, yesterday." $
   parser (parseSmartDateAndCode esToday) $
   withCompletion (dateCompleter def) $
   defaultTo' def $ nonEmpty $
   maybeExit $
   -- maybeShowHelp $
   linePrewritten (green $ printf "Date%s: " (showDefault def)) (fromMaybe "" prevDateAndCode) ""
    where
      parseSmartDateAndCode refdate s = if s == "<" then return Nothing else either (const Nothing) (\(d,c) -> return $ Just (fixSmartDate refdate d, c)) edc
          where
            edc = runParser (dateandcodep <* eof) "" $ T.pack $ lowercase s
            dateandcodep :: SimpleTextParser (SmartDate, Text)
            dateandcodep = do
                d <- smartdate
                c <- optional codep
                skipNonNewlineSpaces
                eof
                return (d, fromMaybe "" c)
      -- defday = fixSmartDate today $ fromparse $ (parse smartdate "" . lowercase) defdate
      -- datestr = showDate $ fixSmartDate defday smtdate

descriptionAndCommentWizard PrevInput{..} EntryState{..} = do
  let def = headDef "" esArgs
  s <- withCompletion (descriptionCompleter esJournal def) $
       defaultTo' def $ nonEmpty $
       linePrewritten (green $ printf "Description%s: " (showDefault def)) (fromMaybe "" prevDescAndCmnt) ""
  if s == "<"
    then return Nothing
    else do
      let (desc,comment) = (T.pack $ strip a, T.pack $ strip $ dropWhile (==';') b) where (a,b) = break (==';') s
      return $ Just (desc, comment)

postingsBalanced :: [Posting] -> Bool
postingsBalanced ps = isRight $ balanceTransaction balancingOpts nulltransaction{tpostings=ps}

accountWizard PrevInput{..} EntryState{..} = do
  let pnum = length esPostings + 1
      historicalp = fmap ((!! (pnum - 1)) . (++ (repeat nullposting)) . tpostings) esSimilarTransaction
      historicalacct = case historicalp of Just p  -> showAccountName Nothing (ptype p) (paccount p)
                                           Nothing -> ""
      def = headDef (T.unpack historicalacct) esArgs
      endmsg | canfinish && null def = " (or . or enter to finish this transaction)"
             | canfinish             = " (or . to finish this transaction)"
             | otherwise             = ""
  retryMsg "A valid hledger account name is required. Eg: assets:cash, expenses:food:eating out." $
   parser (parseAccountOrDotOrNull def canfinish) $
   withCompletion (accountCompleter esJournal def) $
   defaultTo' def $ -- nonEmpty $
   linePrewritten (green $ printf "Account %d%s%s: " pnum (endmsg::String) (showDefault def)) (fromMaybe "" $ prevAccount `atMay` length esPostings) ""
    where
      canfinish = not (null esPostings) && postingsBalanced esPostings
      parseAccountOrDotOrNull :: String -> Bool -> String -> Maybe (Maybe String)
      parseAccountOrDotOrNull _  _ "<"       = dbg1 $ Just Nothing
      parseAccountOrDotOrNull _  _ "."       = dbg1 $ Just $ Just "." -- . always signals end of txn
      parseAccountOrDotOrNull "" True ""     = dbg1 $ Just $ Just ""  -- when there's no default and txn is balanced, "" also signals end of txn
      parseAccountOrDotOrNull def@(_:_) _ "" = dbg1 $ Just $ Just def -- when there's a default, "" means use that
      parseAccountOrDotOrNull _ _ s          = dbg1 $ fmap (Just . T.unpack) $
        either (const Nothing) validateAccount $
          flip evalState esJournal $ runParserT (accountnamep <* eof) "" (T.pack s) -- otherwise, try to parse the input as an accountname
        where
          validateAccount :: Text -> Maybe Text
          validateAccount t | no_new_accounts_ esOpts && notElem t (journalAccountNamesDeclaredOrImplied esJournal) = Nothing
                            | otherwise = Just t
      dbg1 = id -- strace

amountAndCommentWizard PrevInput{..} EntryState{..} = do
  let pnum = length esPostings + 1
      (mhistoricalp,followedhistoricalsofar) =
          case esSimilarTransaction of
            Nothing                        -> (Nothing,False)
            Just Transaction{tpostings=ps} ->
              ( if length ps >= pnum then Just (ps !! (pnum-1)) else Nothing
              , all sameamount $ zip esPostings ps
              )
              where
                sameamount (p1,p2) = mixedAmountUnstyled (pamount p1) == mixedAmountUnstyled (pamount p2)
      def | (d:_) <- esArgs                                     = d
          | Just hp <- mhistoricalp, followedhistoricalsofar    = showamt $ pamount hp
          | pnum > 1 && not (mixedAmountLooksZero balancingamt) = showamt balancingamtfirstcommodity
          | otherwise                                           = ""
  retryMsg "A valid hledger amount is required. Eg: 1, $2, 3 EUR, \"4 red apples\"." $
   parser parseAmountAndComment $
   withCompletion (amountCompleter def) $
   defaultTo' def $
   nonEmpty $
   linePrewritten (green $ printf "Amount  %d%s: " pnum (showDefault def)) (fromMaybe "" $ prevAmountAndCmnt `atMay` length esPostings) ""
    where
      parseAmountAndComment s = if s == "<" then return Nothing else either (const Nothing) (return . Just) $
                                runParser
                                  (evalStateT (amountandcommentp <* eof) nodefcommodityj)
                                  ""
                                  (T.pack s)
      nodefcommodityj = esJournal{jparsedefaultcommodity=Nothing}
      amountandcommentp :: JournalParser Identity (Amount, Text)
      amountandcommentp = do
        a <- amountp
        lift skipNonNewlineSpaces
        c <- T.pack <$> fromMaybe "" `fmap` optional (char ';' >> many anySingle)
        -- eof
        return (a,c)
      balancingamt = maNegate . sumPostings $ filter isReal esPostings
      balancingamtfirstcommodity = mixed . take 1 $ amounts balancingamt
      showamt = wbUnpack . showMixedAmountB noColour . mixedAmountSetPrecision
                  -- what should this be ?
                  -- 1 maxprecision (show all decimal places or none) ?
                  -- 2 maxprecisionwithpoint (show all decimal places or .0 - avoids some but not all confusion with thousands separators) ?
                  -- 3 canonical precision for this commodity in the journal ?
                  -- 4 maximum precision entered so far in this transaction ?
                  -- 5 3 or 4, whichever would show the most decimal places ?
                  -- I think 1 or 4, whichever would show the most decimal places
                  NaturalPrecision
  --
  -- let -- (amt,comment) = (strip a, strip $ dropWhile (==';') b) where (a,b) = break (==';') amtcmt
      -- a           = fromparse $ runParser (amountp <|> return missingamt) (jparsestate esJournal) "" amt
  --     awithoutjps = fromparse $ runParser (amountp <|> return missingamt) mempty              "" amt
  --     defamtaccepted = Just (showAmount a) == mdefamt
  --     es2 = if defamtaccepted then es1 else es1{esHistoricalPostings=Nothing}
  --     mdefaultcommodityapplied = if acommodity a == acommodity awithoutjps then Nothing else Just $ acommodity a
  -- when (isJust mdefaultcommodityapplied) $
  --      liftIO $ hPutStrLn stderr $ printf "using default commodity (%s)" (fromJust mdefaultcommodityapplied)

maybeExit = parser (\s -> if s=="." then throw UnexpectedEOF else Just s)

-- maybeShowHelp :: Wizard Haskeline String -> Wizard Haskeline String
-- maybeShowHelp wizard = maybe (liftIO showHelp >> wizard) return $
--                        parser (\s -> if s=="?" then Nothing else Just s) wizard

-- Completion helpers

dateCompleter :: String -> CompletionFunc IO
dateCompleter = completer ["today","tomorrow","yesterday"]

descriptionCompleter :: Journal -> String -> CompletionFunc IO
descriptionCompleter j = completer (map T.unpack $ journalDescriptions j)

accountCompleter :: Journal -> String -> CompletionFunc IO
accountCompleter j = completer (map T.unpack $ journalAccountNamesDeclaredOrImplied j)

amountCompleter :: String -> CompletionFunc IO
amountCompleter = completer []

-- | Generate a haskeline completion function from the given
-- completions and default, that case insensitively completes with
-- prefix matches, or infix matches above a minimum length, or
-- completes the null string with the default.
completer :: [String] -> String -> CompletionFunc IO
completer completions def = completeWord Nothing "" completionsFor
    where
      simpleCompletion' s = (simpleCompletion s){isFinished=False}
      completionsFor "" = return [simpleCompletion' def]
      completionsFor i  = return (map simpleCompletion' ciprefixmatches)
          where
            ciprefixmatches = [c | c <- completions, i `isPrefixOf` c]
            -- mixed-case completions require haskeline > 0.7.1.2
            -- ciprefixmatches = [c | c <- completions, lowercase i `isPrefixOf` lowercase c]

--------------------------------------------------------------------------------

-- utilities

defaultTo' = flip defaultTo

withCompletion f = withSettings (setComplete f defaultSettings)

green s = "\ESC[1;32m\STX"++s++"\ESC[0m\STX"

showDefault "" = ""
showDefault s = " [" ++ s ++ "]"

-- | Append this transaction to the journal's file and transaction list.
journalAddTransaction :: Journal -> CliOpts -> Transaction -> IO Journal
journalAddTransaction j@Journal{jtxns=ts} opts t = do
  let f = journalFilePath j
  appendToJournalFileOrStdout f $ showTransaction t
    -- unelided shows all amounts explicitly, in case there's a price, cf #283
  when (debug_ opts > 0) $ do
    putStrLn $ printf "\nAdded transaction to %s:" f
    TL.putStrLn =<< registerFromString (showTransaction t)
  return j{jtxns=ts++[t]}

-- | Append a string, typically one or more transactions, to a journal
-- file, or if the file is "-", dump it to stdout.  Tries to avoid
-- excess whitespace.
--
-- XXX This writes unix line endings (\n), some at least,
-- even if the file uses dos line endings (\r\n), which could leave
-- mixed line endings in the file. See also writeFileWithBackupIfChanged.
--
appendToJournalFileOrStdout :: FilePath -> Text -> IO ()
appendToJournalFileOrStdout f s
  | f == "-"  = T.putStr s'
  | otherwise = appendFile f $ T.unpack s'
  where s' = "\n" <> ensureOneNewlineTerminated s

-- | Replace a string's 0 or more terminating newlines with exactly one.
ensureOneNewlineTerminated :: Text -> Text
ensureOneNewlineTerminated = (<>"\n") . T.dropWhileEnd (=='\n')

-- | Convert a string of journal data into a register report.
registerFromString :: T.Text -> IO TL.Text
registerFromString s = do
  j <- readJournal' s
  return . postingsReportAsText opts $ postingsReport rspec j
      where
        ropts = defreportopts{empty_=True}
        rspec = defreportspec{rsOpts=ropts}
        opts = defcliopts{reportspec_=rspec}

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
