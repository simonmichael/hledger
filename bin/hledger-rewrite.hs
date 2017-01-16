#!/usr/bin/env stack
{- stack runghc --verbosity info
  --package hledger-lib
  --package hledger
  --package megaparsec
  --package text
-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-

hledger-rewrite [PATTERNS] --add-posting "ACCT  AMTEXPR" ...

A start at a generic rewriter of journal entries.
Reads the default journal and prints the entries, like print,
but adds the specified postings to any entries matching PATTERNS.

Examples:

hledger-rewrite.hs ^income --add-posting '(liabilities:tax)  *.33  ; income tax' --add-posting '(reserve:gifts)  $100'
hledger-rewrite.hs expenses:gifts --add-posting '(reserve:gifts)  *-1"'
hledger-rewrite.hs -f rewrites.hledger

rewrites.hledger may consist of entries like:
= ^income amt:<0 date:2017
  (liabilities:tax)  *0.33  ; tax on income
  (reserve:grocery)  *0.25  ; reserve 25% for grocery
  (reserve:)  *0.25  ; reserve 25% for grocery


Note the single quotes to protect the dollar sign from bash, and the two spaces between account and amount.
See the command-line help for more details.
Currently does not work when invoked via hledger, run it directly instead.

Related: https://github.com/simonmichael/hledger/issues/99

TODO:
- should allow regex matching and interpolating matched name in replacement
- should apply all matching rules to a transaction, not just one
- should be possible to use this on unbalanced entries, eg while editing one

-}

import Data.Monoid
import qualified Data.Text as T
-- hledger lib, cli and cmdargs utils
import Hledger.Cli
-- more utils for parsing
-- #if !MIN_VERSION_base(4,8,0)
-- import Control.Applicative.Compat ((<*))
-- #endif
import Text.Megaparsec
import Hledger.Data.AutoTransaction (runModifierTransaction)

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

postingp' :: T.Text -> IO Posting
postingp' t = runErroringJournalParser (postingp Nothing <* eof) t' >>= \case
        Left err -> fail err
        Right p -> return p
    where t' = " " <> t <> "\n" -- inject space and newline for proper parsing

modifierTransactionFromOpts :: RawOpts -> IO ModifierTransaction
modifierTransactionFromOpts opts = do
    postings <- mapM (postingp' . stripquotes . T.pack) $ listofstringopt "add-posting" opts
    return
        ModifierTransaction { mtvalueexpr = T.empty, mtpostings = postings }

main :: IO ()
main = do
  opts@CliOpts{rawopts_=rawopts,reportopts_=ropts} <- getCliOpts cmdmode
  d <- getCurrentDay
  let q = queryFromOpts d ropts
  modifier <- modifierTransactionFromOpts rawopts
  withJournalDo opts $ \opts' j@Journal{jtxns=ts} -> do
    -- create re-writer
    let modifiers = modifier : jmodifiertxns j
        -- Note that some query matches require transaction. Thus modifiers
        -- pipeline should include txnTieKnot on every step.
        modifier' = foldr (flip (.) . fmap txnTieKnot . runModifierTransaction q) id modifiers
    -- rewrite matched transactions
    let j' = j{jtxns=map modifier' ts}
    -- run the print command, showing all transactions
    print' opts'{reportopts_=ropts{query_=""}} j'
