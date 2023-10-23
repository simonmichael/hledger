#!/usr/bin/env bash
# A custom compound report - like incomestatement but with different
# subheadings/subreports. A bit hacky but quick and short.
# See also hledger-report1.hs.

echo "Report1 Statement $(date +%Y-%m-%d)"
printf "\nRevenues\n"
hledger bal expr:"type:r and $@" | tail +2
printf "\nOperating Expenses\n"
hledger bal expr:"type:x and operating and $@" | tail +2
printf "\nOther expenses\n"
hledger bal expr:"type:x and not:operating and $@" | tail +2
printf "\nGrand Total\n"
hledger bal expr:"type:rx and $@" | tail -1
