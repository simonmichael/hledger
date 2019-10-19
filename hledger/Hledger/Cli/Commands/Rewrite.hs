{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveTraversable, ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Rewrite (
  rewritemode
 ,rewrite
)
where

#if !(MIN_VERSION_base(4,11,0))
import Control.Monad.Writer
#endif
import Data.Functor.Identity
import Data.List (sortOn, foldl')
import qualified Data.Text as T
import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Print
import System.Console.CmdArgs.Explicit
import Text.Printf
import Text.Megaparsec
import qualified Data.Algorithm.Diff as D

rewritemode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Rewrite.txt")
  [flagReq ["add-posting"] (\s opts -> Right $ setopt "add-posting" s opts) "'ACCT  AMTEXPR'"
           "add a posting to ACCT, which may be parenthesised. AMTEXPR is either a literal amount, or *N which means the transaction's first matched amount multiplied by N (a decimal number). Two spaces separate ACCT and AMTEXPR."
  ,flagNone ["diff"] (setboolopt "diff") "generate diff suitable as an input for patch tool"
  ]
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY] --add-posting \"ACCT  AMTEXPR\" ...")

-- TODO regex matching and interpolating matched name in replacement
-- TODO interpolating match groups in replacement
-- TODO allow using this on unbalanced entries, eg to rewrite while editing

rewrite opts@CliOpts{rawopts_=rawopts,reportopts_=ropts} j@Journal{jtxns=ts} = do
  -- rewrite matched transactions
  let modifiers = transactionModifierFromOpts opts : jtxnmodifiers j
  let j' = j{jtxns=modifyTransactions modifiers ts}
  -- run the print command, showing all transactions, or show diffs
  printOrDiff rawopts opts{reportopts_=ropts{query_=""}} j j'

-- | Build a 'TransactionModifier' from any query arguments and --add-posting flags
-- provided on the command line, or throw a parse error.
transactionModifierFromOpts :: CliOpts -> TransactionModifier
transactionModifierFromOpts CliOpts{rawopts_=rawopts,reportopts_=ropts} =
  TransactionModifier{tmquerytxt=q, tmpostingrules=ps}
  where
    q = T.pack $ query_ ropts
    ps = map (parseposting . T.pack) $ listofstringopt "add-posting" rawopts
    parseposting t = either (error' . errorBundlePretty) id ep
      where
        ep = runIdentity (runJournalParser (postingp Nothing <* eof) t')
        t' = " " <> t <> "\n" -- inject space and newline for proper parsing

printOrDiff :: RawOpts -> (CliOpts -> Journal -> Journal -> IO ())
printOrDiff opts
    | boolopt "diff" opts = const diffOutput
    | otherwise = flip (const print')

diffOutput :: Journal -> Journal -> IO ()
diffOutput j j' = do
    let changed = [(originalTransaction t, originalTransaction t') | (t, t') <- zip (jtxns j) (jtxns j'), t /= t']
    putStr $ renderPatch $ map (uncurry $ diffTxn j) changed

type Chunk = (GenericSourcePos, [DiffLine String])

-- XXX doctests, update needed:
-- >>> putStr $ renderPatch [(GenericSourcePos "a" 1 1, [D.First "x", D.Second "y"])]
-- --- a
-- +++ a
-- @@ -1,1 +1,1 @@
-- -x
-- +y
-- >>> putStr $ renderPatch [(GenericSourcePos "a" 1 1, [D.Both "x" "x", D.Second "y"]), (GenericSourcePos "a" 5 1, [D.Second "z"])]
-- --- a
-- +++ a
-- @@ -1,1 +1,2 @@
--  x
-- +y
-- @@ -5,0 +6,1 @@
-- +z
-- >>> putStr $ renderPatch [(GenericSourcePos "a" 1 1, [D.Both "x" "x", D.Second "y"]), (GenericSourcePos "b" 5 1, [D.Second "z"])]
-- --- a
-- +++ a
-- @@ -1,1 +1,2 @@
--  x
-- +y
-- --- b
-- +++ b
-- @@ -5,0 +5,1 @@
-- +z
-- | Render list of changed lines as a unified diff
renderPatch :: [Chunk] -> String
renderPatch = go Nothing . sortOn fst where
    go _ [] = ""
    go Nothing cs@((sourceFilePath -> fp, _):_) = fileHeader fp ++ go (Just (fp, 0)) cs
    go (Just (fp, _)) cs@((sourceFilePath -> fp', _):_) | fp /= fp' = go Nothing cs
    go (Just (fp, offs)) ((sourceFirstLine -> lineno, diffs):cs) = chunkHeader ++ chunk ++ go (Just (fp, offs + adds - dels)) cs
        where
            chunkHeader = printf "@@ -%d,%d +%d,%d @@\n" lineno dels (lineno+offs) adds where
            (dels, adds) = foldl' countDiff (0, 0) diffs
            chunk = concatMap renderLine diffs
    fileHeader fp = printf "--- %s\n+++ %s\n" fp fp

    countDiff (dels, adds) = \case
        Del _  -> (dels + 1, adds)
        Add _ -> (dels    , adds + 1)
        Ctx _ -> (dels + 1, adds + 1)

    renderLine = \case
        Del s -> '-' : s ++ "\n"
        Add s -> '+' : s ++ "\n"
        Ctx s -> ' ' : s ++ "\n"

diffTxn :: Journal -> Transaction -> Transaction -> Chunk
diffTxn j t t' =
        case tsourcepos t of
            GenericSourcePos fp lineno _ -> (GenericSourcePos fp (lineno+1) 1, diffs) where
                -- TODO: use range and produce two chunks: one removes part of
                --       original file, other adds transaction to new file with
                --       suffix .ledger (generated). I.e. move transaction from one file to another.
                diffs :: [DiffLine String]
                diffs = concat . map (traverse showPostingLines . mapDiff) $ D.getDiff (tpostings t) (tpostings t')
            pos@(JournalSourcePos fp (line, line')) -> (pos, diffs) where
                -- We do diff for original lines vs generated ones. Often leads
                -- to big diff because of re-format effect.
                diffs :: [DiffLine String]
                diffs = map mapDiff $ D.getDiff source changed'
                source | Just contents <- lookup fp $ jfiles j = map T.unpack . drop (line-1) . take line' $ T.lines contents
                       | otherwise = []
                changed = lines $ showTransactionUnelided t'
                changed' | null changed = changed
                         | null $ last changed = init changed
                         | otherwise = changed

data DiffLine a = Del a | Add a | Ctx a
    deriving (Show, Functor, Foldable, Traversable)

mapDiff :: D.Diff a -> DiffLine a
mapDiff = \case
    D.First x -> Del x
    D.Second x -> Add x
    D.Both x _ -> Ctx x
