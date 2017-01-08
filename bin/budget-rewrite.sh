#!/bin/sh
#    budget.sh < $LEDGER_FILE 
# or hledger print ... | budget.sh
# https://github.com/simonmichael/hledger/issues/99#issuecomment-270796337

  hledger-rewrite -f- expenses:food      --add-posting '(budget:food)       *-1' \
| hledger-rewrite -f- expenses:health    --add-posting '(budget:health)     *-1' \
| hledger-rewrite -f- expenses:home      --add-posting '(budget:home)       *-1' \
| hledger-rewrite -f- expenses:transport --add-posting '(budget:transport)  *-1' \
| hledger-rewrite -f- 'expenses not:(food|health|home|transport)' --add-posting '(budget:misc)  *-1'
