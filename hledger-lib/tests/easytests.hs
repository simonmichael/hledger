#!/usr/bin/env stack exec -- ghcid -Tmain
-- Run tests using project's resolver, whenever ghcid is happy.
--
-- Experimental tests using easytest, an alternative to hunit (eg).
-- https://hackage.haskell.org/package/easytest

{-# LANGUAGE OverloadedStrings #-}

import Hledger

main :: IO ()
main = do
  run
  -- rerun "journal.standard account types.queries.assets"
  -- rerunOnly 2686786430487349354 "journal.standard account types.queries.assets"
    $ tests [

      test "journal.standard account types.queries" $
        let
          j = samplejournal
          journalAccountNamesMatching :: Query -> Journal -> [AccountName]
          journalAccountNamesMatching q = filter (q `matchesAccount`) . journalAccountNames
          namesfrom qfunc = journalAccountNamesMatching (qfunc j) j
        in
          tests
            [ test "assets" $
              expectEq (namesfrom journalAssetAccountQuery)     ["assets","assets:bank","assets:bank:checking","assets:bank:saving","assets:cash"]
            , test "liabilities" $
              expectEq (namesfrom journalLiabilityAccountQuery) ["liabilities","liabilities:debts"]
            , test "equity" $
              expectEq (namesfrom journalEquityAccountQuery)    []
            , test "income" $
              expectEq (namesfrom journalIncomeAccountQuery)    ["income","income:gifts","income:salary"]
            , test "expenses" $
              expectEq (namesfrom journalExpenseAccountQuery)   ["expenses","expenses:food","expenses:supplies"]
            ]

    ]
