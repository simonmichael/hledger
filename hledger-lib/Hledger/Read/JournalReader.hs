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

{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, NoMonoLocalBinds, ScopedTypeVariables, FlexibleContexts, TupleSections #-}

module Hledger.Read.JournalReader (

--- * exports

  -- * Reader
  reader,

  -- * Parsing utils
  genericSourcePos,
  parseAndFinaliseJournal,
  runStringParser,
  rsp,
  runJournalParser,
  rjp,
  runErroringJournalParser,
  rejp,

  -- * Parsers used elsewhere
  getParentAccount,
  journalp,
  directivep,
  defaultyeardirectivep,
  marketpricedirectivep,
  datetimep,
  datep,
  codep,
  accountnamep,
  modifiedaccountnamep,
  postingp,
  amountp,
  amountp',
  mamountp',
  numberp,
  statusp,
  emptyorcommentlinep,
  followingcommentp,
  accountaliasp

  -- * Tests
  ,tests_Hledger_Read_JournalReader

)
where
--- * imports
import Prelude ()
import Prelude.Compat hiding (readFile)
import qualified Control.Exception as C
import Control.Monad.Except (ExceptT(..), liftIO, runExceptT, throwError, catchError)
import qualified Data.Map.Strict as M
import Data.Time.Calendar
import Data.Time.LocalTime
import Safe
import Test.HUnit
#ifdef TESTS
import Test.Framework
import Text.Parsec.Error
#endif
import Text.Parsec hiding (parse)
import Text.Printf
import System.FilePath

import Hledger.Data
import Hledger.Read.Common
import Hledger.Read.TimeclockReader (timeclockfilep)
import Hledger.Read.TimedotReader (timedotfilep)
import Hledger.Utils


--- * reader

reader :: Reader
reader = Reader format detect parse

format :: String
format = "journal"

-- | Does the given file path and data look like it might be hledger's journal format ?
detect :: FilePath -> String -> Bool
detect f s
  | f /= "-"  = takeExtension f `elem` ['.':format, ".j"] -- from a known file name: yes if the extension is this format's name or .j
  | otherwise = regexMatches "(^|\n)[0-9]+.*\n[ \t]+" s   -- from stdin: yes if we can see something that looks like a journal entry (digits in column 0 with the next line indented)

-- | Parse and post-process a "Journal" from hledger's journal file
-- format, or give an error.
parse :: Maybe FilePath -> Bool -> FilePath -> String -> ExceptT String IO Journal
parse _ = parseAndFinaliseJournal journalp

--- * parsers
--- ** journal

-- | Top-level journal parser. Returns a single composite, I/O performing,
-- error-raising "JournalUpdate" (and final "JournalContext") which can be
-- applied to an empty journal to get the final result.
journalp :: ErroringJournalParser (JournalUpdate,JournalContext)
journalp = do
  journalupdates <- many journalItem
  eof
  finalctx <- getState
  return (combineJournalUpdates journalupdates, finalctx)
    where
      -- As all journal line types can be distinguished by the first
      -- character, excepting transactions versus empty (blank or
      -- comment-only) lines, can use choice w/o try
      journalItem = choice [ directivep
                           , fmap (return . addTransaction) transactionp
                           , fmap (return . addModifierTransaction) modifiertransactionp
                           , fmap (return . addPeriodicTransaction) periodictransactionp
                           , fmap (return . addMarketPrice) marketpricedirectivep
                           , emptyorcommentlinep >> return (return id)
                           , multilinecommentp >> return (return id)
                           ] <?> "transaction or directive"

--- ** directives

-- cf http://ledger-cli.org/3.0/doc/ledger3.html#Command-Directives
directivep :: ErroringJournalParser JournalUpdate
directivep = do
  optional $ char '!'
  choice' [
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
  <?> "directive"

includedirectivep :: ErroringJournalParser JournalUpdate
includedirectivep = do
  string "include"
  many1 spacenonewline
  filename <- restofline
  outerState <- getState
  outerPos <- getPosition
  let curdir = takeDirectory (sourceName outerPos)
  -- XXX clean this up, probably after getting rid of JournalUpdate
  let (u::ExceptT String IO (Journal -> Journal, JournalContext)) = do
       filepath <- expandPath curdir filename
       txt <- readFileOrError outerPos filepath
       let inIncluded = show outerPos ++ " in included file " ++ show filename ++ ":\n"
       r <- runParserT
            (choice' [journalp
                     ,timeclockfilep
                     ,timedotfilep
                     -- can't include a csv file yet, that reader is special
                     ])
            outerState filepath txt

       case r of
         Right (ju, ctx) -> do
                            u <- combineJournalUpdates [ return $ journalAddFile (filepath,txt)
                                                       , ju
                                                       ] `catchError` (throwError . (inIncluded ++))
                            return (u, ctx)
         Left err -> throwError $ inIncluded ++ show err
       where readFileOrError pos fp =
                ExceptT $ fmap Right (readFile' fp) `C.catch`
                  \e -> return $ Left $ printf "%s reading %s:\n%s" (show pos) fp (show (e::C.IOException))
  r <- liftIO $ runExceptT u
  case r of
    Left err -> return $ throwError err
    Right (ju, _finalparsectx) -> return $ ExceptT $ return $ Right ju

accountdirectivep :: ErroringJournalParser JournalUpdate
accountdirectivep = do
  string "account"
  many1 spacenonewline
  acct <- accountnamep
  newline
  _ <- many indentedlinep
  pushAccount acct
  return $ ExceptT $ return $ Right id

indentedlinep = many1 spacenonewline >> (rstrip <$> restofline)

-- | Parse a one-line or multi-line commodity directive.
--
-- >>> Right _ <- rejp commoditydirectivep "commodity $1.00"
-- >>> Right _ <- rejp commoditydirectivep "commodity $\n  format $1.00"
-- >>> Right _ <- rejp commoditydirectivep "commodity $\n\n" -- a commodity with no format
-- >>> Right _ <- rejp commoditydirectivep "commodity $1.00\n  format $1.00" -- both, what happens ?
commoditydirectivep :: ErroringJournalParser JournalUpdate
commoditydirectivep = try commoditydirectiveonelinep <|> commoditydirectivemultilinep

-- | Parse a one-line commodity directive.
--
-- >>> Right _ <- rejp commoditydirectiveonelinep "commodity $1.00"
-- >>> Right _ <- rejp commoditydirectiveonelinep "commodity $1.00 ; blah\n"
commoditydirectiveonelinep :: ErroringJournalParser JournalUpdate
commoditydirectiveonelinep = do
  string "commodity"
  many1 spacenonewline
  Amount{acommodity,astyle} <- amountp
  many spacenonewline
  _ <- followingcommentp <|> (eolof >> return "")
  let comm = Commodity{csymbol=acommodity, cformat=Just astyle}
  return $ ExceptT $ return $ Right $ \j -> j{jcommodities=M.insert acommodity comm $ jcommodities j}

-- | Parse a multi-line commodity directive, containing 0 or more format subdirectives.
--
-- >>> Right _ <- rejp commoditydirectivemultilinep "commodity $ ; blah \n  format $1.00 ; blah"
commoditydirectivemultilinep :: ErroringJournalParser JournalUpdate
commoditydirectivemultilinep = do
  string "commodity"
  many1 spacenonewline
  sym <- commoditysymbolp
  _ <- followingcommentp <|> (eolof >> return "")
  mformat <- lastMay <$> many (indented $ formatdirectivep sym)
  let comm = Commodity{csymbol=sym, cformat=mformat}
  return $ ExceptT $ return $ Right $ \j -> j{jcommodities=M.insert sym comm $ jcommodities j}

indented = (many1 spacenonewline >>)

-- | Parse a format (sub)directive, throwing a parse error if its
-- symbol does not match the one given.
formatdirectivep :: CommoditySymbol -> ErroringJournalParser AmountStyle
formatdirectivep expectedsym = do
  string "format"
  many1 spacenonewline
  pos <- getPosition
  Amount{acommodity,astyle} <- amountp
  _ <- followingcommentp <|> (eolof >> return "")
  if acommodity==expectedsym
    then return astyle
    else parserErrorAt pos $
         printf "commodity directive symbol \"%s\" and format directive symbol \"%s\" should be the same" expectedsym acommodity

applyaccountdirectivep :: ErroringJournalParser JournalUpdate
applyaccountdirectivep = do
  string "apply" >> many1 spacenonewline >> string "account"
  many1 spacenonewline
  parent <- accountnamep
  newline
  pushParentAccount parent
  return $ ExceptT $ return $ Right id

endapplyaccountdirectivep :: ErroringJournalParser JournalUpdate
endapplyaccountdirectivep = do
  string "end" >> many1 spacenonewline >> string "apply" >> many1 spacenonewline >> string "account"
  popParentAccount
  return $ ExceptT $ return $ Right id

aliasdirectivep :: ErroringJournalParser JournalUpdate
aliasdirectivep = do
  string "alias"
  many1 spacenonewline
  alias <- accountaliasp
  addAccountAlias alias
  return $ return id

accountaliasp :: Monad m => StringParser u m AccountAlias
accountaliasp = regexaliasp <|> basicaliasp

basicaliasp :: Monad m => StringParser u m AccountAlias
basicaliasp = do
  -- pdbg 0 "basicaliasp"
  old <- rstrip <$> many1 (noneOf "=")
  char '='
  many spacenonewline
  new <- rstrip <$> anyChar `manyTill` eolof  -- don't require a final newline, good for cli options
  return $ BasicAlias old new

regexaliasp :: Monad m => StringParser u m AccountAlias
regexaliasp = do
  -- pdbg 0 "regexaliasp"
  char '/'
  re <- many1 $ noneOf "/\n\r" -- paranoid: don't try to read past line end
  char '/'
  many spacenonewline
  char '='
  many spacenonewline
  repl <- rstrip <$> anyChar `manyTill` eolof
  return $ RegexAlias re repl

endaliasesdirectivep :: ErroringJournalParser JournalUpdate
endaliasesdirectivep = do
  string "end aliases"
  clearAccountAliases
  return (return id)

tagdirectivep :: ErroringJournalParser JournalUpdate
tagdirectivep = do
  string "tag" <?> "tag directive"
  many1 spacenonewline
  _ <- many1 nonspace
  restofline
  return $ return id

endtagdirectivep :: ErroringJournalParser JournalUpdate
endtagdirectivep = do
  (string "end tag" <|> string "pop") <?> "end tag or pop directive"
  restofline
  return $ return id

defaultyeardirectivep :: ErroringJournalParser JournalUpdate
defaultyeardirectivep = do
  char 'Y' <?> "default year"
  many spacenonewline
  y <- many1 digit
  let y' = read y
  failIfInvalidYear y
  setYear y'
  return $ return id

defaultcommoditydirectivep :: ErroringJournalParser JournalUpdate
defaultcommoditydirectivep = do
  char 'D' <?> "default commodity"
  many1 spacenonewline
  Amount{..} <- amountp
  setDefaultCommodityAndStyle (acommodity, astyle)
  restofline
  return $ return id

marketpricedirectivep :: ErroringJournalParser MarketPrice
marketpricedirectivep = do
  char 'P' <?> "market price"
  many spacenonewline
  date <- try (do {LocalTime d _ <- datetimep; return d}) <|> datep -- a time is ignored
  many1 spacenonewline
  symbol <- commoditysymbolp
  many spacenonewline
  price <- amountp
  restofline
  return $ MarketPrice date symbol price

ignoredpricecommoditydirectivep :: ErroringJournalParser JournalUpdate
ignoredpricecommoditydirectivep = do
  char 'N' <?> "ignored-price commodity"
  many1 spacenonewline
  commoditysymbolp
  restofline
  return $ return id

commodityconversiondirectivep :: ErroringJournalParser JournalUpdate
commodityconversiondirectivep = do
  char 'C' <?> "commodity conversion"
  many1 spacenonewline
  amountp
  many spacenonewline
  char '='
  many spacenonewline
  amountp
  restofline
  return $ return id

--- ** transactions

modifiertransactionp :: ErroringJournalParser ModifierTransaction
modifiertransactionp = do
  char '=' <?> "modifier transaction"
  many spacenonewline
  valueexpr <- restofline
  postings <- postingsp Nothing
  return $ ModifierTransaction valueexpr postings

periodictransactionp :: ErroringJournalParser PeriodicTransaction
periodictransactionp = do
  char '~' <?> "periodic transaction"
  many spacenonewline
  periodexpr <- restofline
  postings <- postingsp Nothing
  return $ PeriodicTransaction periodexpr postings

-- | Parse a (possibly unbalanced) transaction.
transactionp :: ErroringJournalParser Transaction
transactionp = do
  -- ptrace "transactionp"
  sourcepos <- genericSourcePos <$> getPosition
  date <- datep <?> "transaction"
  edate <- optionMaybe (secondarydatep date) <?> "secondary date"
  lookAhead (spacenonewline <|> newline) <?> "whitespace or newline"
  status <- statusp <?> "cleared status"
  code <- codep <?> "transaction code"
  description <- strip <$> descriptionp
  comment <- try followingcommentp <|> (newline >> return "")
  let tags = commentTags comment
  postings <- postingsp (Just date)
  idx <- incrementTransactionIndex
  return $ txnTieKnot $ Transaction idx sourcepos date edate status code description comment tags postings ""

#ifdef TESTS
test_transactionp = do
    let s `gives` t = do
                        let p = parseWithCtx nullctx transactionp s
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
      tstatus=Uncleared,
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

    assertRight $ parseWithCtx nullctx transactionp $ unlines
      ["2007/01/28 coopportunity"
      ,"    expenses:food:groceries                   $47.18"
      ,"    assets:checking                          $-47.18"
      ,""
      ]

    -- transactionp should not parse just a date
    assertLeft $ parseWithCtx nullctx transactionp "2009/1/1\n"

    -- transactionp should not parse just a date and description
    assertLeft $ parseWithCtx nullctx transactionp "2009/1/1 a\n"

    -- transactionp should not parse a following comment as part of the description
    let p = parseWithCtx nullctx transactionp "2009/1/1 a ;comment\n b 1\n"
    assertRight p
    assertEqual "a" (let Right p' = p in tdescription p')

    -- parse transaction with following whitespace line
    assertRight $ parseWithCtx nullctx transactionp $ unlines
        ["2012/1/1"
        ,"  a  1"
        ,"  b"
        ," "
        ]

    let p = parseWithCtx nullctx transactionp $ unlines
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
postingsp :: Maybe Day -> ErroringJournalParser [Posting]
postingsp mdate = many (try $ postingp mdate) <?> "postings"

-- linebeginningwithspaces :: Monad m => JournalParser m String
-- linebeginningwithspaces = do
--   sp <- many1 spacenonewline
--   c <- nonspace
--   cs <- restofline
--   return $ sp ++ (c:cs) ++ "\n"

postingp :: Maybe Day -> ErroringJournalParser Posting
postingp mtdate = do
  -- pdbg 0 "postingp"
  many1 spacenonewline
  status <- statusp
  many spacenonewline
  account <- modifiedaccountnamep
  let (ptype, account') = (accountNamePostingType account, unbracket account)
  amount <- spaceandamountormissingp
  massertion <- partialbalanceassertionp
  _ <- fixedlotpricep
  many spacenonewline
  (comment,tags,mdate,mdate2) <-
    try (followingcommentandtagsp mtdate) <|> (newline >> return ("",[],Nothing,Nothing))
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
                         let parse = parseWithCtx nullctx (postingp Nothing) s
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
      (isRight $ parseWithCtx nullctx (postingp Nothing) "  a  1 \"DE123\"\n")

  -- ,"postingp parses balance assertions and fixed lot prices" ~: do
    assertBool (isRight $ parseWithCtx nullctx (postingp Nothing) "  a  1 \"DE123\" =$1 { =2.2 EUR} \n")

    -- let parse = parseWithCtx nullctx postingp " a\n ;next-line comment\n"
    -- assertRight parse
    -- let Right p = parse
    -- assertEqual "next-line comment\n" (pcomment p)
    -- assertEqual (Just nullmixedamt) (pbalanceassertion p)
#endif

--- * more tests

tests_Hledger_Read_JournalReader = TestList $ concat [
    -- test_numberp
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
     assertParse (parseWithCtx nullctx modifiertransactionp "= (some value expr)\n some:postings  1\n")

  ,"periodictransactionp" ~: do
     assertParse (parseWithCtx nullctx periodictransactionp "~ (some period expr)\n some:postings  1\n")

  ,"directivep" ~: do
     assertParse (parseWithCtx nullctx directivep "!include /some/file.x\n")
     assertParse (parseWithCtx nullctx directivep "account some:account\n")
     assertParse (parseWithCtx nullctx (directivep >> directivep) "!account a\nend\n")

  ,"comment" ~: do
     assertParse (parseWithCtx nullctx comment "; some comment \n")
     assertParse (parseWithCtx nullctx comment " \t; x\n")
     assertParse (parseWithCtx nullctx comment "#x")

  ,"datep" ~: do
     assertParse (parseWithCtx nullctx datep "2011/1/1")
     assertParseFailure (parseWithCtx nullctx datep "1/1")
     assertParse (parseWithCtx nullctx{ctxYear=Just 2011} datep "1/1")

  ,"datetimep" ~: do
      let p = do {t <- datetimep; eof; return t}
          bad = assertParseFailure . parseWithCtx nullctx p
          good = assertParse . parseWithCtx nullctx p
      bad "2011/1/1"
      bad "2011/1/1 24:00:00"
      bad "2011/1/1 00:60:00"
      bad "2011/1/1 00:00:60"
      good "2011/1/1 00:00"
      good "2011/1/1 23:59:59"
      good "2011/1/1 3:5:7"
      -- timezone is parsed but ignored
      let startofday = LocalTime (fromGregorian 2011 1 1) (TimeOfDay 0 0 (fromIntegral 0))
      assertParseEqual (parseWithCtx nullctx p "2011/1/1 00:00-0800") startofday
      assertParseEqual (parseWithCtx nullctx p "2011/1/1 00:00+1234") startofday

  ,"defaultyeardirectivep" ~: do
     assertParse (parseWithCtx nullctx defaultyeardirectivep "Y 2010\n")
     assertParse (parseWithCtx nullctx defaultyeardirectivep "Y 10001\n")

  ,"marketpricedirectivep" ~:
    assertParseEqual (parseWithCtx nullctx marketpricedirectivep "P 2004/05/01 XYZ $55.00\n") (MarketPrice (parsedate "2004/05/01") "XYZ" $ usd 55)

  ,"ignoredpricecommoditydirectivep" ~: do
     assertParse (parseWithCtx nullctx ignoredpricecommoditydirectivep "N $\n")

  ,"defaultcommoditydirectivep" ~: do
     assertParse (parseWithCtx nullctx defaultcommoditydirectivep "D $1,000.0\n")

  ,"commodityconversiondirectivep" ~: do
     assertParse (parseWithCtx nullctx commodityconversiondirectivep "C 1h = $50.00\n")

  ,"tagdirectivep" ~: do
     assertParse (parseWithCtx nullctx tagdirectivep "tag foo \n")

  ,"endtagdirectivep" ~: do
     assertParse (parseWithCtx nullctx endtagdirectivep "end tag \n")
     assertParse (parseWithCtx nullctx endtagdirectivep "pop \n")

  ,"accountnamep" ~: do
    assertBool "accountnamep parses a normal account name" (isRight $ parsewith accountnamep "a:b:c")
    assertBool "accountnamep rejects an empty inner component" (isLeft $ parsewith accountnamep "a::c")
    assertBool "accountnamep rejects an empty leading component" (isLeft $ parsewith accountnamep ":b:c")
    assertBool "accountnamep rejects an empty trailing component" (isLeft $ parsewith accountnamep "a:b:")

  ,"leftsymbolamountp" ~: do
    assertParseEqual (parseWithCtx nullctx leftsymbolamountp "$1")  (usd 1 `withPrecision` 0)
    assertParseEqual (parseWithCtx nullctx leftsymbolamountp "$-1") (usd (-1) `withPrecision` 0)
    assertParseEqual (parseWithCtx nullctx leftsymbolamountp "-$1") (usd (-1) `withPrecision` 0)

  ,"amount" ~: do
     let -- | compare a parse result with an expected amount, showing the debug representation for clarity
         assertAmountParse parseresult amount =
             (either (const "parse error") showAmountDebug parseresult) ~?= (showAmountDebug amount)
     assertAmountParse (parseWithCtx nullctx amountp "1 @ $2")
       (num 1 `withPrecision` 0 `at` (usd 2 `withPrecision` 0))

 ]]
-}
