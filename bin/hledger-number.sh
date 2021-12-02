#!/usr/bin/env bash
# hledger-number - try to extract just one single machine-readable number from hledger:
# the grand total of `hledger balance` run with any provided arguments.
# Requires hledger 1.24 or newer (the December 2021 release, see https://hledger.org/download.html )
# https://gist.github.com/simonmichael/feee7fc1567c5f10355f952b87bd3090

VALUATION_COMMODITY="$"
hledger bal -0 -N -X "$VALUATION_COMMODITY" --infer-market-prices -c "$VALUATION_COMMODITY 1000" --layout=bare "$@" | awk '{print $1}'

# Tired of complex financial reports ? This is a silly but fun and
# occasionally useful script showing how to get "one number" semi-robustly
# without writing a haskell script. With this installed in PATH, then eg:
#
# $ hledger number lodging
# 200
#
# Explanation:
# -0 (--depth 0) hides all but top-level accounts
# -N (--no-total) hides the totals line
# -X COMM (--value=end,COMM) converts to a single commodity if possible (needs at least one suitable P market price declaration) 
# --infer-market-prices guesses P price from conversion transactions if necessary
# -c (--commodity-style) sets a predictable number format free of thousands separators
# --layout=bare moves the commodity symbol away from the number
# awk discards all but the number
#
# A note: If you're new to hledger/PTA and thinking it shouldn't be this hard..
# well of course normally when a hledger user wants to see a single balance,
# they run a much simpler command like `hledger bal lodging`. This script is
# exploring how to anticipate and neutralise all variations from account
# structure, number formatting, multiple commodities, etc. so as to *reliably*
# produce *one machine readable number* from highly diverse data. As we figure
# out good approaches we'll build them in so that producing data for charting
# (eg) is as easy as it can be, while still being general.
