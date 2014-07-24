#!/usr/bin/env runhaskell
{-|
hledger-print-csv [-f JOURNALFILE]

Print matched journal entries as CSV
Reads the default or specified journal.
|-}

import Hledger.Cli
import Text.CSV
import Data.Char (isSpace)
import Data.List (mapAccumL)

argsmode :: Mode RawOpts
argsmode = (defCommandMode ["print-csv"])
	{ modeHelp = "print matched journal entries as CSV"
	, modeGroupFlags = Group
		{ groupNamed =
			[ ("Input",inputflags)
			, ("Reporting",reportflags)
			, ("Misc",helpflags)
			]
		, groupUnnamed = []
		, groupHidden = []
		}
	}

chomp :: String -> String
chomp = reverse . dropWhile isSpace . reverse . dropWhile isSpace

postingToCSV :: Posting -> CSV
postingToCSV p =
	map (\(a@(Amount {aquantity=q,acommodity=c})) ->
		let a_ = a{acommodity=""} in
		let amount = showAmount a_ in
		let commodity = c in
		let credit = if q < 0 then showAmount $ negate a_ else "" in
		let debit  = if q > 0 then showAmount a_ else "" in
		account:amount:commodity:credit:debit:status:comment:[])
	 amounts
	where
		Mixed amounts = pamount p
		status = if pstatus p then "*" else ""
		account = showAccountName Nothing (ptype p) (paccount p)
		comment = chomp $ pcomment p

postingsToCSV :: [Posting] -> CSV
postingsToCSV ps =
	concatMap postingToCSV ps

transactionToCSV :: Integer -> Transaction -> CSV
transactionToCSV n t =
	map (\p -> show n:date:date2:status:code:description:comment:p)
	 (postingsToCSV (tpostings t))
	where
		description = tdescription t
		date = showDate (tdate t)
		date2 = maybe "" showDate (tdate2 t)
		status = if tstatus t then "*" else ""
		code = tcode t
		comment = chomp $ tcomment t

main :: IO ()
main = do
	opts <- getCliOpts argsmode
	withJournalDo opts $
	 \CliOpts{reportopts_=ropts} j -> do
		d <- getCurrentDay
		let ropts_ = ropts{flat_=True}
		let q = queryFromOpts d ropts_
		putStrLn $ printCSV $ concat $
			([["nth","date","date2","status","code","description","comment","account","amount","commodity","credit","debit","status","posting-comment"]]:).snd $
				mapAccumL (\n e -> (n + 1, transactionToCSV n e)) 0 $
					entriesReport ropts_ q j
