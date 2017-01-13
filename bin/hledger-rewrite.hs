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

hledger-rewrite.hs ^income --add-posting '(liabilities:tax)  *.33' --add-posting '(reserve:gifts)  $100'
hledger-rewrite.hs expenses:gifts --add-posting '(reserve:gifts)  *-1"'

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
import Text.Megaparsec.Text

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

post' :: AccountName -> Amount -> Posting
post' acct amt = (accountNameWithoutPostingType acct `post` amt) { ptype = accountNamePostingType acct }


postingScale :: Posting -> Maybe Quantity
postingScale p =
    case amounts $ pamount p of
        [a] | acommodity a == "*" -> Just $ aquantity a
        _ -> Nothing

runModifierPosting :: Posting -> (Posting -> Posting)
runModifierPosting p' =
    case postingScale p' of
        Nothing -> \p -> p' { ptransaction = ptransaction p }
        Just n -> \p -> p' { pamount = pamount p `divideMixedAmount` (1/n), ptransaction = ptransaction p }

runModifierTransaction :: Query -> ModifierTransaction -> (Transaction -> Transaction)
runModifierTransaction q mod = modifier where
    mods = map runModifierPosting $ mtpostings mod
    generatePostings ps = [mod p | p <- ps, q `matchesPosting` p, mod <- mods]
    modifier t@Transaction{ tpostings = ps } = t { tpostings = ps ++ generatePostings ps }

main = do
  opts@CliOpts{rawopts_=rawopts,reportopts_=ropts} <- getCliOpts cmdmode
  d <- getCurrentDay
  let q = queryFromOpts d ropts
  mod <- modifierTransactionFromOpts rawopts
  withJournalDo opts $ \opts j@Journal{jtxns=ts} -> do
    -- rewrite matched transactions
    let j' = j{jtxns=map (runModifierTransaction q mod) ts}
    -- run the print command, showing all transactions
    print' opts{reportopts_=ropts{query_=""}} j'
