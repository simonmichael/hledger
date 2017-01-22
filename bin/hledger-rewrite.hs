#!/usr/bin/env stack
{- stack runghc --verbosity info
  --package hledger-lib
  --package hledger
  --package megaparsec
  --package text
  --package Diff
-}
{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveTraversable, ViewPatterns #-}
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

import Control.Monad.Writer
import Data.List (sortOn, foldl')
import qualified Data.Text as T
-- hledger lib, cli and cmdargs utils
import Hledger.Cli hiding (outputflags)
-- more utils for parsing
-- #if !MIN_VERSION_base(4,8,0)
-- import Control.Applicative.Compat ((<*))
-- #endif
import Text.Printf
import Text.Megaparsec
import qualified Data.Algorithm.Diff as D
import Hledger.Data.AutoTransaction (runModifierTransaction)

cmdmode :: Mode RawOpts
cmdmode = (defCommandMode ["hledger-rewrite"]) {
   modeArgs = ([], Just $ argsFlag "[PATTERNS] --add-posting \"ACCT  AMTEXPR\" ...")
  ,modeHelp = "print all journal entries, with custom postings added to the matched ones"
  ,modeGroupFlags = Group {
     groupNamed = [("Input",     inputflags)
                  ,("Output",    outputflags)
                  ,("Reporting", reportflags)
                  ,("Misc",      helpflags)
                 ]
    ,groupUnnamed = [flagReq ["add-posting"] (\s opts -> Right $ setopt "add-posting" s opts) "'ACCT  AMTEXPR'"
                     "add a posting to ACCT, which may be parenthesised. AMTEXPR is either a literal amount, or *N which means the transaction's first matched amount multiplied by N (a decimal number). Two spaces separate ACCT and AMTEXPR."]
    ,groupHidden = []
    }
  }

outputflags :: [Flag RawOpts]
outputflags = [flagNone ["diff"] (setboolopt "diff") "generate diff suitable as an input for patch tool"]

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

outputFromOpts :: RawOpts -> (CliOpts -> Journal -> Journal -> IO ())
outputFromOpts opts
    | boolopt "diff" opts = const diffOutput
    | otherwise = flip (const print')

diffOutput :: Journal -> Journal -> IO ()
diffOutput j j' = do
    let changed = [(originalTransaction t, originalTransaction t') | (t, t') <- zip (jtxns j) (jtxns j'), t /= t']
    putStr $ renderPatch $ map (uncurry $ diffTxn j) changed

type Chunk = (GenericSourcePos, [DiffLine String])

-- | Render list of changed lines as a unified diff
--
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
    outputFromOpts rawopts opts'{reportopts_=ropts{query_=""}} j j'
