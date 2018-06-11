--- * doc
-- Lines beginning "--- *" are collapsible orgstruct nodes. Emacs users,
-- (add-hook 'haskell-mode-hook
--   (lambda () (set-variable 'orgstruct-heading-prefix-regexp "--- " t))
--   'orgstruct-mode)
-- and press TAB on nodes to expand/collapse.

{-|

A reader for hledger's journal file format
(<http://hledger.org/MANUAL.html#the-journal-file>).  hledger's journal
format is a compatible subset of c++ ledger's
(<http://ledger-cli.org/3.0/doc/ledger3.html#Journal-Format>), so this
reader should handle many ledger files as well. Example:

@
2012\/3\/24 gift
    expenses:gifts  $10
    assets:cash
@

Journal format supports the include directive which can read files in
other formats, so the other file format readers need to be importable
here.  Some low-level journal syntax parsers which those readers also
use are therefore defined separately in Hledger.Read.Common, avoiding
import cycles.

-}

--- * module

{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, NoMonoLocalBinds, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedStrings, PackageImports #-}

module Hledger.Read.JournalReader (
--- * exports

  -- * Reader
  reader,

  -- * Parsing utils
  genericSourcePos,
  parseAndFinaliseJournal,
  runJournalParser,
  rjp,

  -- * Parsers used elsewhere
  getParentAccount,
  journalp,
  directivep,
  defaultyeardirectivep,
  marketpricedirectivep,
  datetimep,
  datep,
  -- codep,
  -- accountnamep,
  modifiedaccountnamep,
  postingp,
  -- amountp,
  -- amountp',
  -- mamountp',
  -- numberp,
  statusp,
  emptyorcommentlinep,
  followingcommentp

  -- * Tests
  ,tests_Hledger_Read_JournalReader

)
where
--- * imports
import Prelude ()
import "base-compat-batteries" Prelude.Compat hiding (readFile)
import qualified Control.Exception as C
import Control.Monad
import Control.Monad.Except (ExceptT(..))
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.String
import Data.List
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Safe
import Test.HUnit
#ifdef TESTS
import Test.Framework
import Text.Megaparsec.Error
#endif
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Custom
import Text.Printf
import System.FilePath

import Hledger.Data
import Hledger.Read.Common
import Hledger.Read.TimeclockReader (timeclockfilep)
import Hledger.Read.TimedotReader (timedotfilep)
import Hledger.Utils

-- $setup
-- >>> :set -XOverloadedStrings

--- * reader

reader :: Reader
reader = Reader
  {rFormat     = "journal"
  ,rExtensions = ["journal", "j", "hledger", "ledger"]
  ,rParser     = parse
  ,rExperimental = False
  }

-- | Parse and post-process a "Journal" from hledger's journal file
-- format, or give an error.
parse :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal
parse iopts = parseAndFinaliseJournal journalp' iopts
  where
    journalp' = do 
      -- reverse parsed aliases to ensure that they are applied in order given on commandline
      mapM_ addAccountAlias (reverse $ aliasesFromOpts iopts) 
      journalp

-- | Get the account name aliases from options, if any.
aliasesFromOpts :: InputOpts -> [AccountAlias]
aliasesFromOpts = map (\a -> fromparse $ runParser accountaliasp ("--alias "++quoteIfNeeded a) $ T.pack a)
                  . aliases_

--- * parsers
--- ** journal

-- | A journal parser. Accumulates and returns a "ParsedJournal",
-- which should be finalised/validated before use.
--
-- >>> rjp (journalp <* eof) "2015/1/1\n a  0\n"
-- Right Journal  with 1 transactions, 1 accounts
--
journalp :: MonadIO m => JournalParser m ParsedJournal
journalp = do
  many addJournalItemP
  eof
  get

-- | A side-effecting parser; parses any kind of journal item
-- and updates the parse state accordingly.
addJournalItemP :: MonadIO m => JournalParser m ()
addJournalItemP =
  -- all journal line types can be distinguished by the first
  -- character, can use choice without backtracking
  choice [
      directivep
    , transactionp          >>= modify' . addTransaction
    , modifiertransactionp  >>= modify' . addModifierTransaction
    , periodictransactionp  >>= modify' . addPeriodicTransaction
    , marketpricedirectivep >>= modify' . addMarketPrice
    , void (lift emptyorcommentlinep)
    , void (lift multilinecommentp)
    ] <?> "transaction or directive"

--- ** directives

-- | Parse any journal directive and update the parse state accordingly.
-- Cf http://hledger.org/manual.html#directives,
-- http://ledger-cli.org/3.0/doc/ledger3.html#Command-Directives
directivep :: MonadIO m => JournalParser m ()
directivep = (do
  optional $ char '!'
  choice [
    includedirectivep
   ,aliasdirectivep
   ,endaliasesdirectivep
   ,accountdirectivep
   ,applyaccountdirectivep
   ,commoditydirectivep
   ,endapplyaccountdirectivep
   ,tagdirectivep
   ,endtagdirectivep
   ,defaultyeardirectivep
   ,defaultcommoditydirectivep
   ,commodityconversiondirectivep
   ,ignoredpricecommoditydirectivep
   ]
  ) <?> "directive"

includedirectivep :: MonadIO m => JournalParser m ()
includedirectivep = do
  string "include"
  lift (skipSome spacenonewline)
  filename <- T.unpack <$> takeWhileP Nothing (/= '\n') -- don't consume newline yet

  -- save parent state
  parentParserState <- getParserState
  parentj <- get

  let childj = newJournalWithParseStateFrom parentj
  parentpos <- getPosition

  -- read child input
  let curdir = takeDirectory (sourceName parentpos)
  filepath <- lift $ expandPath curdir filename `orRethrowIOError` (show parentpos ++ " locating " ++ filename)
  childInput <- lift $ readFilePortably filepath `orRethrowIOError` (show parentpos ++ " reading " ++ filepath)

  -- set child state
  setInput childInput
  pushPosition $ initialPos filepath
  put childj

  -- parse include file
  let parsers = [ journalp
                , timeclockfilep
                , timedotfilep
                ] -- can't include a csv file yet, that reader is special
  updatedChildj <- journalAddFile (filepath, childInput) <$>
                   region (withSource childInput) (choiceInState parsers)

  -- restore parent state, prepending the child's parse info
  setParserState parentParserState
  put $ updatedChildj <> parentj
  -- discard child's parse info, prepend its (reversed) list data, combine other fields

  void newline


newJournalWithParseStateFrom :: Journal -> Journal
newJournalWithParseStateFrom j = mempty{
   jparsedefaultyear      = jparsedefaultyear j
  ,jparsedefaultcommodity = jparsedefaultcommodity j
  ,jparseparentaccounts   = jparseparentaccounts j
  ,jparsealiases          = jparsealiases j
  ,jcommodities           = jcommodities j
  -- ,jparsetransactioncount = jparsetransactioncount j
  ,jparsetimeclockentries = jparsetimeclockentries j
  }

-- | Lift an IO action into the exception monad, rethrowing any IO
-- error with the given message prepended.
orRethrowIOError :: MonadIO m => IO a -> String -> TextParser m a
orRethrowIOError io msg = do
  eResult <- liftIO $ (Right <$> io) `C.catch` \(e::C.IOException) -> pure $ Left $ printf "%s:\n%s" msg (show e)
  case eResult of
    Right res -> pure res
    Left errMsg -> fail errMsg

accountdirectivep :: JournalParser m ()
accountdirectivep = do
  string "account"
  lift (skipSome spacenonewline)
  acct <- lift accountnamep  -- eats single spaces
  macode' :: Maybe String <- (optional $ lift $ skipSome spacenonewline >> some digitChar)
  let macode :: Maybe AccountCode = read <$> macode'
  newline
  skipMany indentedlinep
    
  modify' (\j -> j{jaccounts = (acct, macode) : jaccounts j})

indentedlinep :: JournalParser m String
indentedlinep = lift (skipSome spacenonewline) >> (rstrip <$> lift restofline)

-- | Parse a one-line or multi-line commodity directive.
--
-- >>> Right _ <- rjp commoditydirectivep "commodity $1.00"
-- >>> Right _ <- rjp commoditydirectivep "commodity $\n  format $1.00"
-- >>> Right _ <- rjp commoditydirectivep "commodity $\n\n" -- a commodity with no format
-- >>> Right _ <- rjp commoditydirectivep "commodity $1.00\n  format $1.00" -- both, what happens ?
commoditydirectivep :: JournalParser m ()
commoditydirectivep = commoditydirectiveonelinep <|> commoditydirectivemultilinep

-- | Parse a one-line commodity directive.
--
-- >>> Right _ <- rjp commoditydirectiveonelinep "commodity $1.00"
-- >>> Right _ <- rjp commoditydirectiveonelinep "commodity $1.00 ; blah\n"
commoditydirectiveonelinep :: JournalParser m ()
commoditydirectiveonelinep = do
  (pos, Amount{acommodity,astyle}) <- try $ do
    string "commodity"
    lift (skipSome spacenonewline)
    pos <- getPosition
    amount <- amountp
    pure $ (pos, amount)
  lift (skipMany spacenonewline)
  _ <- lift followingcommentp
  let comm = Commodity{csymbol=acommodity, cformat=Just $ dbg2 "style from commodity directive" astyle}
  if asdecimalpoint astyle == Nothing
  then parseErrorAt pos pleaseincludedecimalpoint
  else modify' (\j -> j{jcommodities=M.insert acommodity comm $ jcommodities j})

pleaseincludedecimalpoint :: String
pleaseincludedecimalpoint = "to avoid ambiguity, please include a decimal point in commodity directives"

-- | Parse a multi-line commodity directive, containing 0 or more format subdirectives.
--
-- >>> Right _ <- rjp commoditydirectivemultilinep "commodity $ ; blah \n  format $1.00 ; blah"
commoditydirectivemultilinep :: JournalParser m ()
commoditydirectivemultilinep = do
  string "commodity"
  lift (skipSome spacenonewline)
  sym <- lift commoditysymbolp
  _ <- lift followingcommentp
  mformat <- lastMay <$> many (indented $ formatdirectivep sym)
  let comm = Commodity{csymbol=sym, cformat=mformat}
  modify' (\j -> j{jcommodities=M.insert sym comm $ jcommodities j})
  where
    indented = (lift (skipSome spacenonewline) >>)

-- | Parse a format (sub)directive, throwing a parse error if its
-- symbol does not match the one given.
formatdirectivep :: CommoditySymbol -> JournalParser m AmountStyle
formatdirectivep expectedsym = do
  string "format"
  lift (skipSome spacenonewline)
  pos <- getPosition
  Amount{acommodity,astyle} <- amountp
  _ <- lift followingcommentp
  if acommodity==expectedsym
    then 
      if asdecimalpoint astyle == Nothing
      then parseErrorAt pos pleaseincludedecimalpoint
      else return $ dbg2 "style from format subdirective" astyle
    else parseErrorAt pos $
         printf "commodity directive symbol \"%s\" and format directive symbol \"%s\" should be the same" expectedsym acommodity

keywordp :: String -> JournalParser m ()
keywordp = (() <$) . string . fromString

spacesp :: JournalParser m ()
spacesp = () <$ lift (skipSome spacenonewline)

-- | Backtracking parser similar to string, but allows varying amount of space between words
keywordsp :: String -> JournalParser m ()
keywordsp = try . sequence_ . intersperse spacesp . map keywordp . words

applyaccountdirectivep :: JournalParser m ()
applyaccountdirectivep = do
  keywordsp "apply account" <?> "apply account directive"
  lift (skipSome spacenonewline)
  parent <- lift accountnamep
  newline
  pushParentAccount parent

endapplyaccountdirectivep :: JournalParser m ()
endapplyaccountdirectivep = do
  keywordsp "end apply account" <?> "end apply account directive"
  popParentAccount

aliasdirectivep :: JournalParser m ()
aliasdirectivep = do
  string "alias"
  lift (skipSome spacenonewline)
  alias <- lift accountaliasp
  addAccountAlias alias

accountaliasp :: TextParser m AccountAlias
accountaliasp = regexaliasp <|> basicaliasp

basicaliasp :: TextParser m AccountAlias
basicaliasp = do
  -- pdbg 0 "basicaliasp"
  old <- rstrip <$> (some $ noneOf ("=" :: [Char]))
  char '='
  skipMany spacenonewline
  new <- rstrip <$> anyChar `manyTill` eolof  -- eol in journal, eof in command lines, normally
  return $ BasicAlias (T.pack old) (T.pack new)

regexaliasp :: TextParser m AccountAlias
regexaliasp = do
  -- pdbg 0 "regexaliasp"
  char '/'
  re <- some $ noneOf ("/\n\r" :: [Char]) -- paranoid: don't try to read past line end
  char '/'
  skipMany spacenonewline
  char '='
  skipMany spacenonewline
  repl <- anyChar `manyTill` eolof
  return $ RegexAlias re repl

endaliasesdirectivep :: JournalParser m ()
endaliasesdirectivep = do
  keywordsp "end aliases" <?> "end aliases directive"
  clearAccountAliases

tagdirectivep :: JournalParser m ()
tagdirectivep = do
  string "tag" <?> "tag directive"
  lift (skipSome spacenonewline)
  _ <- lift $ some nonspace
  lift restofline
  return ()

endtagdirectivep :: JournalParser m ()
endtagdirectivep = do
  (keywordsp "end tag" <|> keywordp "pop") <?> "end tag or pop directive"
  lift restofline
  return ()

defaultyeardirectivep :: JournalParser m ()
defaultyeardirectivep = do
  char 'Y' <?> "default year"
  lift (skipMany spacenonewline)
  y <- some digitChar
  let y' = read y
  failIfInvalidYear y
  setYear y'

defaultcommoditydirectivep :: JournalParser m ()
defaultcommoditydirectivep = do
  char 'D' <?> "default commodity"
  lift (skipSome spacenonewline)
  pos <- getPosition
  Amount{acommodity,astyle} <- amountp
  lift restofline
  if asdecimalpoint astyle == Nothing
  then parseErrorAt pos pleaseincludedecimalpoint
  else setDefaultCommodityAndStyle (acommodity, astyle)

marketpricedirectivep :: JournalParser m MarketPrice
marketpricedirectivep = do
  char 'P' <?> "market price"
  lift (skipMany spacenonewline)
  date <- try (do {LocalTime d _ <- datetimep; return d}) <|> datep -- a time is ignored
  lift (skipSome spacenonewline)
  symbol <- lift commoditysymbolp
  lift (skipMany spacenonewline)
  price <- amountp
  lift restofline
  return $ MarketPrice date symbol price

ignoredpricecommoditydirectivep :: JournalParser m ()
ignoredpricecommoditydirectivep = do
  char 'N' <?> "ignored-price commodity"
  lift (skipSome spacenonewline)
  lift commoditysymbolp
  lift restofline
  return ()

commodityconversiondirectivep :: JournalParser m ()
commodityconversiondirectivep = do
  char 'C' <?> "commodity conversion"
  lift (skipSome spacenonewline)
  amountp
  lift (skipMany spacenonewline)
  char '='
  lift (skipMany spacenonewline)
  amountp
  lift restofline
  return ()

--- ** transactions

modifiertransactionp :: JournalParser m ModifierTransaction
modifiertransactionp = do
  char '=' <?> "modifier transaction"
  lift (skipMany spacenonewline)
  valueexpr <- T.pack <$> lift restofline
  postings <- postingsp Nothing
  return $ ModifierTransaction valueexpr postings

-- | Parse a periodic transaction
periodictransactionp :: MonadIO m => JournalParser m PeriodicTransaction
periodictransactionp = do
  char '~' <?> "periodic transaction"
  lift $ skipMany spacenonewline
  -- XXX periodexprp in Hledger.Data.Dates is a SimpleTextParser, which we can't call directly here.
  -- Instead, read until two or more spaces and reparse that. More use of two spaces is not ideal.
  pos <- getPosition
  periodtxt <- lift singlespacedtextp
  d <- liftIO getCurrentDay
  (interval, span) <-
    case parsePeriodExpr d periodtxt of
        Right (i,s) -> return (i,s)
        Left e -> 
          -- Show an informative error. XXX a bit unidiomatic, check for megaparsec helpers  
          fail $ -- XXX
            showGenericSourcePos (genericSourcePos pos) ++ ":\n" ++
            (unlines $ drop 1 $ lines $ parseErrorPretty e) ++
            "while parsing a period expression in: "++T.unpack periodtxt++"\n" ++
            "2+ spaces are needed between period expression and any description/comment." 
  -- In periodic transactions, the period expression has an additional constraint:
  case checkPeriodicTransactionStartDate interval span periodtxt of
    Just e -> fail e -- XXX
    Nothing -> do
      status <- lift statusp
      code <- lift codep
      description <- lift $ T.strip <$> descriptionp
      (comment, tags) <- lift transactioncommentp
      postings <- postingsp (Just $ first3 $ toGregorian d)
      return $ nullperiodictransaction{
         ptperiodexpr=periodtxt
        ,ptinterval=interval
        ,ptspan=span
        ,ptstatus=status
        ,ptcode=code
        ,ptdescription=description
        ,ptcomment=comment
        ,pttags=tags
        ,ptpostings=postings
        }

-- | Parse a (possibly unbalanced) transaction.
transactionp :: JournalParser m Transaction
transactionp = do
  -- ptrace "transactionp"
  startpos <- getPosition
  date <- datep <?> "transaction"
  edate <- optional (lift $ secondarydatep date) <?> "secondary date"
  lookAhead (lift spacenonewline <|> newline) <?> "whitespace or newline"
  status <- lift statusp <?> "cleared status"
  code <- lift codep <?> "transaction code"
  description <- lift $ T.strip <$> descriptionp
  (comment, tags) <- lift transactioncommentp
  let year = first3 $ toGregorian date
  postings <- postingsp (Just year)
  endpos <- getPosition
  let sourcepos = journalSourcePos startpos endpos
  return $ txnTieKnot $ Transaction 0 sourcepos date edate status code description comment tags postings ""

#ifdef TESTS
test_transactionp = do
    let s `gives` t = do
                        let p = parseWithState mempty transactionp s
                        assertBool $ isRight p
                        let Right t2 = p
                            -- same f = assertEqual (f t) (f t2)
                        assertEqual (tdate t) (tdate t2)
                        assertEqual (tdate2 t) (tdate2 t2)
                        assertEqual (tstatus t) (tstatus t2)
                        assertEqual (tcode t) (tcode t2)
                        assertEqual (tdescription t) (tdescription t2)
                        assertEqual (tcomment t) (tcomment t2)
                        assertEqual (ttags t) (ttags t2)
                        assertEqual (tpreceding_comment_lines t) (tpreceding_comment_lines t2)
                        assertEqual (show $ tpostings t) (show $ tpostings t2)
    -- "0000/01/01\n\n" `gives` nulltransaction
    unlines [
      "2012/05/14=2012/05/15 (code) desc  ; tcomment1",
      "    ; tcomment2",
      "    ; ttag1: val1",
      "    * a         $1.00  ; pcomment1",
      "    ; pcomment2",
      "    ; ptag1: val1",
      "    ; ptag2: val2"
      ]
     `gives`
     nulltransaction{
      tdate=parsedate "2012/05/14",
      tdate2=Just $ parsedate "2012/05/15",
      tstatus=Unmarked,
      tcode="code",
      tdescription="desc",
      tcomment=" tcomment1\n tcomment2\n ttag1: val1\n",
      ttags=[("ttag1","val1")],
      tpostings=[
        nullposting{
          pstatus=Cleared,
          paccount="a",
          pamount=Mixed [usd 1],
          pcomment=" pcomment1\n pcomment2\n ptag1: val1\n  ptag2: val2\n",
          ptype=RegularPosting,
          ptags=[("ptag1","val1"),("ptag2","val2")],
          ptransaction=Nothing
          }
        ],
      tpreceding_comment_lines=""
      }
    unlines [
      "2015/1/1",
      ]
     `gives`
     nulltransaction{
      tdate=parsedate "2015/01/01",
      }

    assertRight $ parseWithState mempty transactionp $ unlines
      ["2007/01/28 coopportunity"
      ,"    expenses:food:groceries                   $47.18"
      ,"    assets:checking                          $-47.18"
      ,""
      ]

    -- transactionp should not parse just a date
    assertLeft $ parseWithState mempty transactionp "2009/1/1\n"

    -- transactionp should not parse just a date and description
    assertLeft $ parseWithState mempty transactionp "2009/1/1 a\n"

    -- transactionp should not parse a following comment as part of the description
    let p = parseWithState mempty transactionp "2009/1/1 a ;comment\n b 1\n"
    assertRight p
    assertEqual "a" (let Right p' = p in tdescription p')

    -- parse transaction with following whitespace line
    assertRight $ parseWithState mempty transactionp $ unlines
        ["2012/1/1"
        ,"  a  1"
        ,"  b"
        ," "
        ]

    let p = parseWithState mempty transactionp $ unlines
             ["2009/1/1 x  ; transaction comment"
             ," a  1  ; posting 1 comment"
             ," ; posting 1 comment 2"
             ," b"
             ," ; posting 2 comment"
             ]
    assertRight p
    assertEqual 2 (let Right t = p in length $ tpostings t)
#endif

--- ** postings

-- Parse the following whitespace-beginning lines as postings, posting
-- tags, and/or comments (inferring year, if needed, from the given date).
postingsp :: Maybe Year -> JournalParser m [Posting]
postingsp mTransactionYear = many (postingp mTransactionYear) <?> "postings"

-- linebeginningwithspaces :: JournalParser m String
-- linebeginningwithspaces = do
--   sp <- lift (skipSome spacenonewline)
--   c <- nonspace
--   cs <- lift restofline
--   return $ sp ++ (c:cs) ++ "\n"

postingp :: Maybe Year -> JournalParser m Posting
postingp mTransactionYear = do
  -- pdbg 0 "postingp"
  (status, account) <- try $ do
    lift (skipSome spacenonewline)
    status <- lift statusp
    lift (skipMany spacenonewline)
    account <- modifiedaccountnamep
    return (status, account)
  let (ptype, account') = (accountNamePostingType account, textUnbracket account)
  amount <- spaceandamountormissingp
  massertion <- partialbalanceassertionp
  _ <- fixedlotpricep
  lift (skipMany spacenonewline)
  (comment,tags,mdate,mdate2) <- lift $ postingcommentp mTransactionYear
  return posting
   { pdate=mdate
   , pdate2=mdate2
   , pstatus=status
   , paccount=account'
   , pamount=amount
   , pcomment=comment
   , ptype=ptype
   , ptags=tags
   , pbalanceassertion=massertion
   }

#ifdef TESTS
test_postingp = do
    let s `gives` ep = do
                         let parse = parseWithState mempty (postingp Nothing) s
                         assertBool -- "postingp parser"
                           $ isRight parse
                         let Right ap = parse
                             same f = assertEqual (f ep) (f ap)
                         same pdate
                         same pstatus
                         same paccount
                         same pamount
                         same pcomment
                         same ptype
                         same ptags
                         same ptransaction
    "  expenses:food:dining  $10.00   ; a: a a \n   ; b: b b \n" `gives`
      posting{paccount="expenses:food:dining", pamount=Mixed [usd 10], pcomment=" a: a a \n b: b b \n", ptags=[("a","a a"), ("b","b b")]}

    " a  1 ; [2012/11/28]\n" `gives`
      ("a" `post` num 1){pcomment=" [2012/11/28]\n"
                        ,ptags=[("date","2012/11/28")]
                        ,pdate=parsedateM "2012/11/28"}

    " a  1 ; a:a, [=2012/11/28]\n" `gives`
      ("a" `post` num 1){pcomment=" a:a, [=2012/11/28]\n"
                        ,ptags=[("a","a"), ("date2","2012/11/28")]
                        ,pdate=Nothing}

    " a  1 ; a:a\n  ; [2012/11/28=2012/11/29],b:b\n" `gives`
      ("a" `post` num 1){pcomment=" a:a\n [2012/11/28=2012/11/29],b:b\n"
                        ,ptags=[("a","a"), ("date","2012/11/28"), ("date2","2012/11/29"), ("b","b")]
                        ,pdate=parsedateM "2012/11/28"}

    assertBool -- "postingp parses a quoted commodity with numbers"
      (isRight $ parseWithState mempty (postingp Nothing) "  a  1 \"DE123\"\n")

  -- ,"postingp parses balance assertions and fixed lot prices" ~: do
    assertBool (isRight $ parseWithState mempty (postingp Nothing) "  a  1 \"DE123\" =$1 { =2.2 EUR} \n")

    -- let parse = parseWithState mempty postingp " a\n ;next-line comment\n"
    -- assertRight parse
    -- let Right p = parse
    -- assertEqual "next-line comment\n" (pcomment p)
    -- assertEqual (Just nullmixedamt) (pbalanceassertion p)
#endif

--- * more tests

tests_Hledger_Read_JournalReader = TestList $ concat [
    -- test_numberp
    [
    "showParsedMarketPrice" ~: do
      let mp = parseWithState mempty marketpricedirectivep "P 2017/01/30 BTC $922.83\n"
          mpString = (fmap . fmap) showMarketPrice mp
      mpString `is` (Just (Right "P 2017/01/30 BTC $922.83"))
    ]
 ]

{- old hunit tests

tests_Hledger_Read_JournalReader = TestList $ concat [
    test_numberp,
    test_amountp,
    test_spaceandamountormissingp,
    test_tagcomment,
    test_inlinecomment,
    test_comments,
    test_ledgerDateSyntaxToTags,
    test_postingp,
    test_transactionp,
    [
   "modifiertransactionp" ~: do
     assertParse (parseWithState mempty modifiertransactionp "= (some value expr)\n some:postings  1\n")

  ,"periodictransactionp" ~: do
     assertParse (parseWithState mempty periodictransactionp "~ (some period expr)\n some:postings  1\n")

  ,"directivep" ~: do
     assertParse (parseWithState mempty directivep "!include /some/file.x\n")
     assertParse (parseWithState mempty directivep "account some:account\n")
     assertParse (parseWithState mempty (directivep >> directivep) "!account a\nend\n")

  ,"comment" ~: do
     assertParse (parseWithState mempty comment "; some comment \n")
     assertParse (parseWithState mempty comment " \t; x\n")
     assertParse (parseWithState mempty comment "#x")

  ,"datep" ~: do
     assertParse (parseWithState mempty datep "2011/1/1")
     assertParseFailure (parseWithState mempty datep "1/1")
     assertParse (parseWithState mempty{jpsYear=Just 2011} datep "1/1")

  ,"datetimep" ~: do
      let p = do {t <- datetimep; eof; return t}
          bad = assertParseFailure . parseWithState mempty p
          good = assertParse . parseWithState mempty p
      bad "2011/1/1"
      bad "2011/1/1 24:00:00"
      bad "2011/1/1 00:60:00"
      bad "2011/1/1 00:00:60"
      good "2011/1/1 00:00"
      good "2011/1/1 23:59:59"
      good "2011/1/1 3:5:7"
      -- timezone is parsed but ignored
      let startofday = LocalTime (fromGregorian 2011 1 1) (TimeOfDay 0 0 (fromIntegral 0))
      assertParseEqual (parseWithState mempty p "2011/1/1 00:00-0800") startofday
      assertParseEqual (parseWithState mempty p "2011/1/1 00:00+1234") startofday

  ,"defaultyeardirectivep" ~: do
     assertParse (parseWithState mempty defaultyeardirectivep "Y 2010\n")
     assertParse (parseWithState mempty defaultyeardirectivep "Y 10001\n")

  ,"marketpricedirectivep" ~:
    assertParseEqual (parseWithState mempty marketpricedirectivep "P 2004/05/01 XYZ $55.00\n") (MarketPrice (parsedate "2004/05/01") "XYZ" $ usd 55)

  ,"ignoredpricecommoditydirectivep" ~: do
     assertParse (parseWithState mempty ignoredpricecommoditydirectivep "N $\n")

  ,"defaultcommoditydirectivep" ~: do
     assertParse (parseWithState mempty defaultcommoditydirectivep "D $1,000.0\n")

  ,"commodityconversiondirectivep" ~: do
     assertParse (parseWithState mempty commodityconversiondirectivep "C 1h = $50.00\n")

  ,"tagdirectivep" ~: do
     assertParse (parseWithState mempty tagdirectivep "tag foo \n")

  ,"endtagdirectivep" ~: do
     assertParse (parseWithState mempty endtagdirectivep "end tag \n")
     assertParse (parseWithState mempty endtagdirectivep "pop \n")

  ,"accountnamep" ~: do
    assertBool "accountnamep parses a normal account name" (isRight $ parsewith accountnamep "a:b:c")
    assertBool "accountnamep rejects an empty inner component" (isLeft $ parsewith accountnamep "a::c")
    assertBool "accountnamep rejects an empty leading component" (isLeft $ parsewith accountnamep ":b:c")
    assertBool "accountnamep rejects an empty trailing component" (isLeft $ parsewith accountnamep "a:b:")

  ,"leftsymbolamountp" ~: do
    assertParseEqual (parseWithState mempty leftsymbolamountp "$1")  (usd 1 `withPrecision` 0)
    assertParseEqual (parseWithState mempty leftsymbolamountp "$-1") (usd (-1) `withPrecision` 0)
    assertParseEqual (parseWithState mempty leftsymbolamountp "-$1") (usd (-1) `withPrecision` 0)

  ,"amount" ~: do
     let -- | compare a parse result with an expected amount, showing the debug representation for clarity
         assertAmountParse parseresult amount =
             (either (const "parse error") showAmountDebug parseresult) ~?= (showAmountDebug amount)
     assertAmountParse (parseWithState mempty amountp "1 @ $2")
       (num 1 `withPrecision` 0 `at` (usd 2 `withPrecision` 0))

 ]]
-}
