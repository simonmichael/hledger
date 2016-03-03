#!/usr/bin/env runhaskell
-- {-# LANGUAGE CPP #-}
{-|
hledger-rewrite [PATTERNS] --add-posting "ACCT  AMTEXPR" ...

A start at a generic rewriter of journal entries.
Reads the default journal and prints the entries, like print,
but adds the specified postings to any entries matching PATTERNS.

Examples:

hledger-rewrite.hs ^income --add-posting '(liabilities:tax)  *.33' --add-posting '(reserve:gifts)  $100'
hledger-rewrite.hs expenses:gifts --add-posting '(reserve:gifts)  *-1"'

Note the single quotes to protect the dollar sign from bash, and the two spaces between account and amount.
See the command-line help for more details.
Currently does not work when invoked via hledger, run hledger-rewrite[.hs] directly.

Needs to work on unbalanced entries, eg while editing one.

Tested-with: hledger HEAD ~ 2016/3/2

|-}

-- hledger lib, cli and cmdargs utils
import Hledger.Cli
-- more utils for parsing
-- #if !MIN_VERSION_base(4,8,0)
-- import Control.Applicative.Compat ((<*))
-- #endif
import Text.Parsec


cmdmode :: Mode RawOpts
cmdmode = (defCommandMode ["hledger-rewrite"]) {
   modeArgs = ([], Just $ argsFlag "[PATTERNS] --add-posting \"ACCT  AMTEXPR\" ...")
  ,modeHelp = "print all journal entries, with custom postings added to the matched ones"
  ,modeGroupFlags = Group {
     groupNamed = [("Input",     inputflags)
                  ,("Reporting", reportflags)
                  ,("Misc",      helpflags)
                 ]
    ,groupUnnamed = [flagReq ["add-posting"] (\s opts -> Right $ setopt "add-posting" s opts) "'ACCT  AMTEXPR'"
                     "add a posting to ACCT, which may be parenthesised. AMTEXPR is either a literal amount, or *N which means the transaction's first matched amount multiplied by N (a decimal number). Two spaces separate ACCT and AMTEXPR."]
    ,groupHidden = []
    }
  }

type PostingExpr = (AccountName, AmountExpr)

data AmountExpr = AmountLiteral String | AmountMultiplier Quantity deriving (Show)

addPostingExprsFromOpts :: RawOpts -> [PostingExpr]
addPostingExprsFromOpts = map (either parseerror id . runParser (postingexprp <* eof) nullctx "") . map stripquotes . listofstringopt "add-posting"

postingexprp = do
  a <- accountnamep
  spacenonewline >> many1 spacenonewline
  aex <- amountexprp
  many spacenonewline
  return (a,aex)

amountexprp =
  choice [
     AmountMultiplier <$> (do char '*'
                              many spacenonewline
                              (q,_,_,_) <- numberp
                              return q)
    ,AmountLiteral <$> many anyChar
    ]

amountExprRenderer :: Query -> AmountExpr -> (Transaction -> MixedAmount)
amountExprRenderer q aex =
  case aex of
    AmountLiteral s    -> const (mamountp' s)
    AmountMultiplier n -> (`divideMixedAmount` (1 / n)) . (`firstAmountMatching` q)
  where
    firstAmountMatching :: Transaction -> Query -> MixedAmount
    firstAmountMatching t q = pamount $ head $ filter (q `matchesPosting`) $ tpostings t

rewriteTransaction :: Transaction -> [(AccountName, Transaction -> MixedAmount)] -> Transaction
rewriteTransaction t addps = t{tpostings=tpostings t ++ map (uncurry (generatePosting t)) addps}
  where
    generatePosting :: Transaction -> AccountName -> (Transaction -> MixedAmount) -> Posting
    generatePosting t acct amtfn = nullposting{paccount     = accountNameWithoutPostingType acct
                                              ,ptype        = accountNamePostingType acct
                                              ,pamount      = amtfn t
                                              ,ptransaction = Just t
                                              }

main = do
  opts@CliOpts{rawopts_=rawopts,reportopts_=ropts} <- getCliOpts cmdmode
  d <- getCurrentDay
  let q = queryFromOpts d ropts
      addps = [(a, amountExprRenderer q aex) | (a, aex) <- addPostingExprsFromOpts rawopts]
  withJournalDo opts $ \opts j@Journal{jtxns=ts} -> do
    -- rewrite matched transactions
    let j' = j{jtxns=map (\t -> if q `matchesTransaction` t then rewriteTransaction t addps else t) ts}
    -- run the print command, showing all transactions
    print' opts{reportopts_=ropts{query_=""}} j'

