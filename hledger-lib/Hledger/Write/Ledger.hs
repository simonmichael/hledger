{-|
Helpers for Ledger-compatible output.
-}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Write.Ledger (
  showTransactionLedger,
)
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB

import Hledger.Data.Amount (defaultFmt, AmountFormat(..))
import Hledger.Data.Posting (postingsAsLines, renderCommentLines)
import Hledger.Data.Transaction (showTransactionLineFirstPart)
import Hledger.Data.Types (Transaction(..), tdescription)

ledgerFmt :: AmountFormat
ledgerFmt = defaultFmt{displayLedgerLotSyntax = True}

-- | Like showTransaction, but renders cost basis using Ledger-style lot syntax
-- ({COST} [DATE] (LABEL)) instead of hledger consolidated syntax.
showTransactionLedger :: Transaction -> Text
showTransactionLedger t =
  TL.toStrict . TB.toLazyText $
      TB.fromText descriptionline <> newline
    <> foldMap ((<> newline) . TB.fromText) newlinecomments
    <> foldMap ((<> newline) . TB.fromText) (postingsAsLines ledgerFmt False $ tpostings t)
    <> newline
  where
    descriptionline = T.stripEnd $ showTransactionLineFirstPart t <> T.concat [desc, samelinecomment]
    desc = if T.null d then "" else " " <> d where d = tdescription t
    (samelinecomment, newlinecomments) =
      case renderCommentLines (tcomment t) of []   -> ("",[])
                                              c:cs -> (c,cs)
    newline = TB.singleton '\n'
