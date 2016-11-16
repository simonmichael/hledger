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

{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, NoMonoLocalBinds, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedStrings #-}

module Hledger.Read.JournalReader (
--- * exports

  -- * Reader
  reader,

  -- * Parsing utils
  genericSourcePos,
  parseAndFinaliseJournal,
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
import Control.Monad
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text (Text)
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
reader = Reader format detect parse

format :: String
format = "journal"

-- | Does the given file path and data look like something this reader can handle ?
detect :: FilePath -> Text -> Bool
detect f _
  -- file name known: try this reader if it has any of these extensions
  | f /= "-"  = takeExtension f `elem` ['.':format, ".j", ".hledger", ".ledger", ".l"]
  -- file name unknown: always try this reader
  | otherwise = True
  -- file name unknown: try this reader if we can see something like a journal entry
  -- (digits in column 0 with the next line indented)
  -- otherwise = regexMatches "(^|\n)[0-9]+.*\n[ \t]+" $ T.unpack excerpt

-- | Parse and post-process a "Journal" from hledger's journal file
-- format, or give an error.
parse :: Maybe FilePath -> Bool -> FilePath -> Text -> ExceptT String IO Journal
parse _ = parseAndFinaliseJournal journalp

--- * parsers
--- ** journal

-- | A journal parser. Accumulates and returns a "ParsedJournal",
-- which should be finalised/validated before use.
--
-- >>> rejp (journalp <* eof) "2015/1/1\n a  0\n"
-- Right Journal  with 1 transactions, 1 accounts
--
journalp :: ErroringJournalParser ParsedJournal
journalp = do
  many addJournalItemP
  eof
  get

-- | A side-effecting parser; parses any kind of journal item
-- and updates the parse state accordingly.
addJournalItemP :: ErroringJournalParser ()
addJournalItemP =
  -- all journal line types can be distinguished by the first
  -- character, can use choice without backtracking
  choice [
      directivep
    , transactionp          >>= modify' . addTransaction
    , modifiertransactionp  >>= modify' . addModifierTransaction
    , periodictransactionp  >>= modify' . addPeriodicTransaction
    , marketpricedirectivep >>= modify' . addMarketPrice
    , void emptyorcommentlinep
    , void multilinecommentp
    ] <?> "transaction or directive"

--- ** directives

-- | Parse any journal directive and update the parse state accordingly.
-- Cf http://hledger.org/manual.html#directives,
-- http://ledger-cli.org/3.0/doc/ledger3.html#Command-Directives
directivep :: ErroringJournalParser ()
directivep = (do
  optional $ char '!'
  choiceInState [
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

includedirectivep :: ErroringJournalParser ()
includedirectivep = do
  string "include"
  lift (some spacenonewline)
  filename  <- lift restofline
  parentpos <- getPosition
  parentj   <- get
  let childj = newJournalWithParseStateFrom parentj
  (ej :: Either String ParsedJournal) <-
    liftIO $ runExceptT $ do
      let curdir = takeDirectory (sourceName parentpos)
      filepath <- expandPath curdir filename `orRethrowIOError` (show parentpos ++ " locating " ++ filename)
      txt      <- readFileAnyLineEnding filepath `orRethrowIOError` (show parentpos ++ " reading " ++ filepath)
      (ej1::Either (ParseError Char Dec) ParsedJournal) <-
        runParserT
           (evalStateT
              (choiceInState
                 [journalp
                 ,timeclockfilep
                 ,timedotfilep
                 -- can't include a csv file yet, that reader is special
                 ])
              childj)
           filepath txt
      either
        (throwError
          . ((show parentpos ++ " in included file " ++ show filename ++ ":\n") ++)
          . show)
        (return . journalAddFile (filepath, txt))
        ej1
  case ej of
    Left e       -> throwError e
    Right childj -> modify' (\parentj -> childj <> parentj)
    -- discard child's parse info, prepend its (reversed) list data, combine other fields

newJournalWithParseStateFrom :: Journal -> Journal
newJournalWithParseStateFrom j = mempty{
   jparsedefaultyear      = jparsedefaultyear j
  ,jparsedefaultcommodity = jparsedefaultcommodity j
  ,jparseparentaccounts   = jparseparentaccounts j
  ,jparsealiases          = jparsealiases j
  -- ,jparsetransactioncount = jparsetransactioncount j
  ,jparsetimeclockentries = jparsetimeclockentries j
  }

-- | Lift an IO action into the exception monad, rethrowing any IO
-- error with the given message prepended.
orRethrowIOError :: IO a -> String -> ExceptT String IO a
orRethrowIOError io msg =
  ExceptT $
    (Right <$> io)
    `C.catch` \(e::C.IOException) -> return $ Left $ printf "%s:\n%s" msg (show e)

accountdirectivep :: ErroringJournalParser ()
accountdirectivep = do
  string "account"
  lift (some spacenonewline)
  acct <- lift accountnamep
  newline
  _ <- many indentedlinep
  modify' (\j -> j{jaccounts = acct : jaccounts j})

indentedlinep = lift (some spacenonewline) >> (rstrip <$> lift restofline)

-- | Parse a one-line or multi-line commodity directive.
--
-- >>> Right _ <- rejp commoditydirectivep "commodity $1.00"
-- >>> Right _ <- rejp commoditydirectivep "commodity $\n  format $1.00"
-- >>> Right _ <- rejp commoditydirectivep "commodity $\n\n" -- a commodity with no format
-- >>> Right _ <- rejp commoditydirectivep "commodity $1.00\n  format $1.00" -- both, what happens ?
commoditydirectivep :: ErroringJournalParser ()
commoditydirectivep = try commoditydirectiveonelinep <|> commoditydirectivemultilinep

-- | Parse a one-line commodity directive.
--
-- >>> Right _ <- rejp commoditydirectiveonelinep "commodity $1.00"
-- >>> Right _ <- rejp commoditydirectiveonelinep "commodity $1.00 ; blah\n"
commoditydirectiveonelinep :: ErroringJournalParser ()
commoditydirectiveonelinep = do
  string "commodity"
  lift (some spacenonewline)
  Amount{acommodity,astyle} <- amountp
  lift (many spacenonewline)
  _ <- followingcommentp <|> (lift eolof >> return "")
  let comm = Commodity{csymbol=acommodity, cformat=Just astyle}
  modify' (\j -> j{jcommodities=M.insert acommodity comm $ jcommodities j})

-- | Parse a multi-line commodity directive, containing 0 or more format subdirectives.
--
-- >>> Right _ <- rejp commoditydirectivemultilinep "commodity $ ; blah \n  format $1.00 ; blah"
commoditydirectivemultilinep :: ErroringJournalParser ()
commoditydirectivemultilinep = do
  string "commodity"
  lift (some spacenonewline)
  sym <- lift commoditysymbolp
  _ <- followingcommentp <|> (lift eolof >> return "")
  mformat <- lastMay <$> many (indented $ formatdirectivep sym)
  let comm = Commodity{csymbol=sym, cformat=mformat}
  modify' (\j -> j{jcommodities=M.insert sym comm $ jcommodities j})
  where
    indented = (lift (some spacenonewline) >>)

-- | Parse a format (sub)directive, throwing a parse error if its
-- symbol does not match the one given.
formatdirectivep :: CommoditySymbol -> ErroringJournalParser AmountStyle
formatdirectivep expectedsym = do
  string "format"
  lift (some spacenonewline)
  pos <- getPosition
  Amount{acommodity,astyle} <- amountp
  _ <- followingcommentp <|> (lift eolof >> return "")
  if acommodity==expectedsym
    then return astyle
    else parserErrorAt pos $
         printf "commodity directive symbol \"%s\" and format directive symbol \"%s\" should be the same" expectedsym acommodity

applyaccountdirectivep :: ErroringJournalParser ()
applyaccountdirectivep = do
  string "apply" >> lift (some spacenonewline) >> string "account"
  lift (some spacenonewline)
  parent <- lift accountnamep
  newline
  pushParentAccount parent

endapplyaccountdirectivep :: ErroringJournalParser ()
endapplyaccountdirectivep = do
  string "end" >> lift (some spacenonewline) >> string "apply" >> lift (some spacenonewline) >> string "account"
  popParentAccount

aliasdirectivep :: ErroringJournalParser ()
aliasdirectivep = do
  string "alias"
  lift (some spacenonewline)
  alias <- lift accountaliasp
  addAccountAlias alias

accountaliasp :: TextParser m AccountAlias
accountaliasp = regexaliasp <|> basicaliasp

basicaliasp :: TextParser m AccountAlias
basicaliasp = do
  -- pdbg 0 "basicaliasp"
  old <- rstrip <$> (some $ noneOf ("=" :: [Char]))
  char '='
  many spacenonewline
  new <- rstrip <$> anyChar `manyTill` eolof  -- don't require a final newline, good for cli options
  return $ BasicAlias (T.pack old) (T.pack new)

regexaliasp :: TextParser m AccountAlias
regexaliasp = do
  -- pdbg 0 "regexaliasp"
  char '/'
  re <- some $ noneOf ("/\n\r" :: [Char]) -- paranoid: don't try to read past line end
  char '/'
  many spacenonewline
  char '='
  many spacenonewline
  repl <- rstrip <$> anyChar `manyTill` eolof
  return $ RegexAlias re repl

endaliasesdirectivep :: ErroringJournalParser ()
endaliasesdirectivep = do
  string "end aliases"
  clearAccountAliases

tagdirectivep :: ErroringJournalParser ()
tagdirectivep = do
  string "tag" <?> "tag directive"
  lift (some spacenonewline)
  _ <- lift $ some nonspace
  lift restofline
  return ()

endtagdirectivep :: ErroringJournalParser ()
endtagdirectivep = do
  (string "end tag" <|> string "pop") <?> "end tag or pop directive"
  lift restofline
  return ()

defaultyeardirectivep :: ErroringJournalParser ()
defaultyeardirectivep = do
  char 'Y' <?> "default year"
  lift (many spacenonewline)
  y <- some digitChar
  let y' = read y
  failIfInvalidYear y
  setYear y'

defaultcommoditydirectivep :: ErroringJournalParser ()
defaultcommoditydirectivep = do
  char 'D' <?> "default commodity"
  lift (some spacenonewline)
  Amount{..} <- amountp
  lift restofline
  setDefaultCommodityAndStyle (acommodity, astyle)

marketpricedirectivep :: ErroringJournalParser MarketPrice
marketpricedirectivep = do
  char 'P' <?> "market price"
  lift (many spacenonewline)
  date <- try (do {LocalTime d _ <- datetimep; return d}) <|> datep -- a time is ignored
  lift (some spacenonewline)
  symbol <- lift commoditysymbolp
  lift (many spacenonewline)
  price <- amountp
  lift restofline
  return $ MarketPrice date symbol price

ignoredpricecommoditydirectivep :: ErroringJournalParser ()
ignoredpricecommoditydirectivep = do
  char 'N' <?> "ignored-price commodity"
  lift (some spacenonewline)
  lift commoditysymbolp
  lift restofline
  return ()

commodityconversiondirectivep :: ErroringJournalParser ()
commodityconversiondirectivep = do
  char 'C' <?> "commodity conversion"
  lift (some spacenonewline)
  amountp
  lift (many spacenonewline)
  char '='
  lift (many spacenonewline)
  amountp
  lift restofline
  return ()

--- ** transactions

modifiertransactionp :: ErroringJournalParser ModifierTransaction
modifiertransactionp = do
  char '=' <?> "modifier transaction"
  lift (many spacenonewline)
  valueexpr <- T.pack <$> lift restofline
  postings <- postingsp Nothing
  return $ ModifierTransaction valueexpr postings

periodictransactionp :: ErroringJournalParser PeriodicTransaction
periodictransactionp = do
  char '~' <?> "periodic transaction"
  lift (many spacenonewline)
  periodexpr <- T.pack <$> lift restofline
  postings <- postingsp Nothing
  return $ PeriodicTransaction periodexpr postings

-- | Parse a (possibly unbalanced) transaction.
transactionp :: ErroringJournalParser Transaction
transactionp = do
  -- ptrace "transactionp"
  sourcepos <- genericSourcePos <$> getPosition
  date <- datep <?> "transaction"
  edate <- optional (secondarydatep date) <?> "secondary date"
  lookAhead (lift spacenonewline <|> newline) <?> "whitespace or newline"
  status <- lift statusp <?> "cleared status"
  code <- T.pack <$> lift codep <?> "transaction code"
  description <- T.pack . strip <$> descriptionp
  comment <- try followingcommentp <|> (newline >> return "")
  let tags = commentTags comment
  postings <- postingsp (Just date)
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
postingsp :: Maybe Day -> ErroringJournalParser [Posting]
postingsp mdate = many (try $ postingp mdate) <?> "postings"

-- linebeginningwithspaces :: Monad m => JournalParser m String
-- linebeginningwithspaces = do
--   sp <- lift (some spacenonewline)
--   c <- nonspace
--   cs <- lift restofline
--   return $ sp ++ (c:cs) ++ "\n"

postingp :: Maybe Day -> ErroringJournalParser Posting
postingp mtdate = do
  -- pdbg 0 "postingp"
  lift (some spacenonewline)
  status <- lift statusp
  lift (many spacenonewline)
  account <- modifiedaccountnamep
  let (ptype, account') = (accountNamePostingType account, textUnbracket account)
  amount <- spaceandamountormissingp
  massertion <- partialbalanceassertionp
  _ <- fixedlotpricep
  lift (many spacenonewline)
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
