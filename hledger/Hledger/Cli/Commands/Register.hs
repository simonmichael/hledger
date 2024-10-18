{-|

A ledger-compatible @register@ command.

-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hledger.Cli.Commands.Register (
  registermode
 ,register
 ,postingsReportAsText
 ,postingsReportItemAsText
 -- ,showPostingWithBalanceForVty
 ,tests_Register
) where

import Data.Default (def)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import System.Console.CmdArgs.Explicit (flagNone, flagReq)

import Hledger hiding (per)
import Hledger.Write.Csv (CSV, printCSV, printTSV)
import Hledger.Write.Ods (printFods)
import Hledger.Write.Html.Lucid (printHtml)
import qualified Hledger.Write.Spreadsheet as Spr
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils
import Hledger.Cli.Anchor (setAccountAnchor, dateCell)
import Text.Tabular.AsciiWide (Cell(..), Align(..), Properties(..), Header(Header, Group), renderRowB, textCell, tableBorders, borderSpaces)
import qualified Lucid
import Data.List (sortBy)
import Data.Char (toUpper)
import Data.List.Extra (intersect)
import System.Exit (exitFailure)
import qualified System.IO as IO

registermode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Register.txt")
  ([flagNone ["cumulative"] (setboolopt "cumulative")
     "show running total from report start date (default)"
  ,flagNone ["historical","H"] (setboolopt "historical")
     "show historical running total/balance (includes postings before report start date)"
  ,flagNone ["average","A"] (setboolopt "average")
     "show running average of posting amounts instead of total (implies --empty)"
  ,let arg = "DESC" in
   flagReq  ["match","m"] (\s opts -> Right $ setopt "match" s opts) arg
    ("fuzzy search for one recent posting with description closest to "++arg)
  ,flagNone ["related","r"] (setboolopt "related") "show postings' siblings instead"
  ,flagNone ["invert"] (setboolopt "invert") "display all amounts with reversed sign"
  ,flagReq  ["sort"] (\s opts -> Right $ setopt "sort" s opts) "FIELDS" 
    ("sort by: " <> sortKeysDescription
    <> ", or a comma-separated combination of these. For a descending sort, prefix with -. (Default: date)")
  ,flagReq  ["width","w"] (\s opts -> Right $ setopt "width" s opts) "N"
     ("set output width (default: " ++
#ifdef mingw32_HOST_OS
      show defaultWidth
#else
      "terminal width"
#endif
      ++ " or $COLUMNS). -wN,M sets description width as well."
     )
  ,flagNone ["align-all"] (setboolopt "align-all") "guarantee alignment across all lines (slower)"
  ,flagReq  ["base-url"] (\s opts -> Right $ setopt "base-url" s opts) "URLPREFIX" "in html output, generate links to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)"
  ,outputFormatFlag ["txt","csv","tsv","json"]
  ,outputFileFlag
  ])
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | Print a (posting) register report.
register :: CliOpts -> Journal -> IO ()
register opts@CliOpts{rawopts_=rawopts, reportspec_=rspec} j
  -- match mode, print one recent posting most similar to given description, if any
  -- XXX should match similarly to print --match
  | Just desc <- maybestringopt "match" rawopts = do
      let ps = [p | (_,_,_,p,_) <- rpt]
      case similarPosting ps desc of
        Nothing -> putStrLn "no matches found." >> exitFailure
        Just p  -> TL.putStr $ postingsReportAsText opts [pri]
                  where pri = (Just (postingDate p)
                              ,Nothing
                              ,tdescription <$> ptransaction p
                              ,styleAmounts styles p
                              ,styleAmounts styles nullmixedamt)
  -- normal register report, list postings
  | otherwise = writeOutputLazyText opts $ render $ styleAmounts styles rpt
  where
    styles = journalCommodityStylesWith HardRounding j
    rpt = postingsReport rspec j
    render | fmt=="txt"  = postingsReportAsText opts
           | fmt=="json" = toJsonText
           | fmt=="csv"  = printCSV . postingsReportAsCsv
           | fmt=="tsv"  = printTSV . postingsReportAsCsv
           | fmt=="html" =
                (<>"\n") . Lucid.renderText . printHtml .
                map (map (fmap Lucid.toHtml)) .
                postingsReportAsSpreadsheet oneLineNoCostFmt baseUrl query
           | fmt=="fods" =
                printFods IO.localeEncoding . Map.singleton "Register" .
                (,) (Just 1, Nothing) .
                postingsReportAsSpreadsheet oneLineNoCostFmt baseUrl query
           | otherwise   = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
      where fmt = outputFormatFromOpts opts
            baseUrl = balance_base_url_ $ _rsReportOpts rspec
            query = querystring_ $ _rsReportOpts rspec

postingsReportAsCsv :: PostingsReport -> CSV
postingsReportAsCsv =
  Spr.rawTableContent . postingsReportAsSpreadsheet machineFmt Nothing []

postingsReportAsSpreadsheet ::
  AmountFormat -> Maybe Text -> [Text] ->
  PostingsReport -> [[Spr.Cell Spr.NumLines T.Text]]
postingsReportAsSpreadsheet fmt base query is =
  Spr.addHeaderBorders
    (map Spr.headerCell
      ["txnidx","date","code","description","account","amount","total"])
  :
  map (postingsReportItemAsRecord fmt base query) is

{- ToDo:
link txnidx to journal URL,
   however, requires Web.Widget.Common.transactionFragment
-}
postingsReportItemAsRecord ::
    (Spr.Lines border) =>
    AmountFormat -> Maybe Text -> [Text] ->
    PostingsReportItem -> [Spr.Cell border T.Text]
postingsReportItemAsRecord fmt base query (_, _, _, p, b) =
    [(cell idx) {Spr.cellType = Spr.TypeInteger},
     (dateCell base query (paccount p) date) {Spr.cellType = Spr.TypeDate},
     cell code, cell desc,
     setAccountAnchor base query (paccount p) $ cell acct,
     amountCell (pamount p),
     amountCell b]
  where
    cell = Spr.defaultCell
    idx  = T.pack . show . maybe 0 tindex $ ptransaction p
    date = postingDate p -- XXX csv should show date2 with --date2
    code = maybe "" tcode $ ptransaction p
    desc = maybe "" tdescription $ ptransaction p
    acct = bracket $ paccount p
      where
        bracket = case ptype p of
                             BalancedVirtualPosting -> wrap "[" "]"
                             VirtualPosting -> wrap "(" ")"
                             _ -> id
    -- Since postingsReport strips prices from all Amounts when not used, we can display prices.
    amountCell amt =
      wbToText <$> Spr.cellFromMixedAmount fmt (Spr.Class "amount", amt)

-- | Render a register report as plain text suitable for console output.
postingsReportAsText :: CliOpts -> PostingsReport -> TL.Text
postingsReportAsText opts = TB.toLazyText .
    postingsOrTransactionsReportAsText alignAll opts (postingsReportItemAsText opts) itemamt itembal
  where
    alignAll = boolopt "align-all" $ rawopts_ opts
    itemamt (_,_,_,Posting{pamount=a},_) = a
    itembal (_,_,_,_,a) = a

-- | Render one register report line item as plain text. Layout is like so:
-- @
-- <---------------- width (specified, terminal width, or 80) -------------------->
-- date (10)  description           account              amount (12)   balance (12)
-- DDDDDDDDDD dddddddddddddddddddd  aaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
-- @
-- If description's width is specified, account will use the remaining space.
-- Otherwise, description and account divide up the space equally.
--
-- With a report interval, the layout is like so:
-- @
-- <---------------- width (specified, terminal width, or 80) -------------------->
-- date (21)              account                        amount (12)   balance (12)
-- DDDDDDDDDDDDDDDDDDDDD  aaaaaaaaaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
-- @
--
-- date and description are shown for the first posting of a transaction only.
--
-- Returns a string which can be multi-line, eg if the running balance
-- has multiple commodities. Does not yet support formatting control
-- like balance reports.
--
-- Also returns the natural width (without padding) of the amount and balance
-- fields.
postingsReportItemAsText :: CliOpts -> Int -> Int
                         -> (PostingsReportItem, [WideBuilder], [WideBuilder])
                         -> TB.Builder
postingsReportItemAsText opts preferredamtwidth preferredbalwidth ((mdate, mperiod, mdesc, p, _), amt, bal) =
    table <> TB.singleton '\n'
  where
    table = renderRowB def{tableBorders=False, borderSpaces=False} . Group NoLine $ map Header
      [ textCell TopLeft $ fitText (Just datewidth) (Just datewidth) True True date
      , spacerCell
      , textCell TopLeft $ fitText (Just descwidth) (Just descwidth) True True desc
      , spacerCell2
      , textCell TopLeft $ fitText (Just acctwidth) (Just acctwidth) True True acct
      , spacerCell2
      , Cell TopRight $ map (pad amtwidth) amt
      , spacerCell2
      , Cell BottomRight $ map (pad balwidth) bal
      ]
    spacerCell  = Cell BottomLeft [WideBuilder (TB.singleton ' ') 1]
    spacerCell2 = Cell BottomLeft [WideBuilder (TB.fromString "  ") 2]
    pad fullwidth amt' = WideBuilder (TB.fromText $ T.replicate w " ") w <> amt'
      where w = fullwidth - wbWidth amt'
    -- calculate widths
    (totalwidth,mdescwidth) = registerWidthsFromOpts opts
    datewidth = maybe 10 periodTextWidth mperiod
    date = case mperiod of
             Just per -> if isJust mdate then showPeriod per else ""
             Nothing  -> maybe "" showDate mdate
    (amtwidth, balwidth)
      | shortfall <= 0 = (preferredamtwidth, preferredbalwidth)
      | otherwise      = (adjustedamtwidth, adjustedbalwidth)
      where
        mincolwidth = 2 -- columns always show at least an ellipsis
        maxamtswidth = max 0 (totalwidth - (datewidth + 1 + mincolwidth + 2 + mincolwidth + 2 + 2))
        shortfall = (preferredamtwidth + preferredbalwidth) - maxamtswidth
        amtwidthproportion = fromIntegral preferredamtwidth / fromIntegral (preferredamtwidth + preferredbalwidth)
        adjustedamtwidth = round $ amtwidthproportion * fromIntegral maxamtswidth
        adjustedbalwidth = maxamtswidth - adjustedamtwidth

    remaining = totalwidth - (datewidth + 1 + 2 + amtwidth + 2 + balwidth)
    (descwidth, acctwidth)
      | isJust mperiod = (0, remaining - 2)
      | otherwise      = (w, remaining - 2 - w)
      where
        w = fromMaybe ((remaining - 2) `div` 2) mdescwidth

    -- gather content
    desc = fromMaybe "" mdesc
    acct = parenthesise . elideAccountName awidth $ paccount p
      where
        (parenthesise, awidth) = case ptype p of
            BalancedVirtualPosting -> (wrap "[" "]", acctwidth-2)
            VirtualPosting         -> (wrap "(" ")", acctwidth-2)
            _                      -> (id,acctwidth)

-- for register --match:

-- Identify the closest recent match for this description in the given date-sorted postings.
similarPosting :: [Posting] -> String -> Maybe Posting
similarPosting ps desc =
  let matches =
          sortBy compareRelevanceAndRecency
                     $ filter ((> threshold).fst)
                     [(maybe 0 (\t -> compareDescriptions desc (T.unpack $ tdescription t)) (ptransaction p), p) | p <- ps]
              where
                compareRelevanceAndRecency (n1,p1) (n2,p2) = compare (n2,postingDate p2) (n1,postingDate p1)
                threshold = 0
  in case matches of []  -> Nothing
                     m:_ -> Just $ snd m

-- -- Identify the closest recent match for this description in past transactions.
-- similarTransaction :: Journal -> Query -> String -> Maybe Transaction
-- similarTransaction j q desc =
--   case historymatches = transactionsSimilarTo j q desc of
--     ((,t):_) = Just t
--     []       = Nothing

compareDescriptions :: String -> String -> Double
compareDescriptions s t = compareStrings s' t'
    where s' = simplify s
          t' = simplify t
          simplify = filter (not . (`elem` ("0123456789"::String)))

-- | Return a similarity measure, from 0 to 1, for two strings.
-- This is Simon White's letter pairs algorithm from
-- http://www.catalysoft.com/articles/StrikeAMatch.html
-- with a modification for short strings.
compareStrings :: String -> String -> Double
compareStrings "" "" = 1
compareStrings [_] "" = 0
compareStrings "" [_] = 0
compareStrings [a] [b] = if toUpper a == toUpper b then 1 else 0
compareStrings s1 s2 = 2.0 * fromIntegral i / fromIntegral u
    where
      i = length $ intersect pairs1 pairs2
      u = length pairs1 + length pairs2
      pairs1 = wordLetterPairs $ uppercase s1
      pairs2 = wordLetterPairs $ uppercase s2

wordLetterPairs = concatMap letterPairs . words

letterPairs (a:b:rest) = [a,b] : letterPairs (b:rest)
letterPairs _ = []

-- tests

tests_Register = testGroup "Register" [

   testGroup "postingsReportAsText" [
    testCase "unicode in register layout" $ do
      j <- readJournal' "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
      let rspec = defreportspec
      (TL.unpack . postingsReportAsText defcliopts $ postingsReport rspec j)
        @?=
        unlines
        ["2009-01-01 медвежья шкура       расходы:покупки                100           100"
        ,"                                актив:наличные                -100             0"]
   ]

 ]
