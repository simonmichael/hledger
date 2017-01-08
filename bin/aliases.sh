# some hledger-related bash aliases

# time

# all in one big journal:
#export TIMELOG=~/personal/time.journal

alias hours="hledger -f $TIMELOG"
alias today='hours -p today'
alias yesterday='hours -p yesterday'
alias thisweek='hours -p thisweek'
alias lastweek='hours -p lastweek'
alias thismonth='hours -p thismonth'
alias lastmonth='hours -p lastmonth'
alias thisyear='hours -p thisyear'
alias lastyear='hours -p lastyear'
alias janhours="hours -p jan"
alias febhours="hours -p feb"
alias marhours="hours -p mar"
alias aprhours="hours -p apr"
alias mayhours="hours -p may"
alias junhours="hours -p jun"
alias julhours="hours -p jul"
alias aughours="hours -p aug"
alias sephours="hours -p sep"
alias octhours="hours -p oct"
alias novhours="hours -p nov"
alias dechours="hours -p dec"
alias 2008hours="hours -p 2008"
alias 2009hours="hours -p 2009"
alias 2010hours="hours -p 2010"
alias 2011hours="hours -p 2011"
alias 2012hours="hours -p 2012"
alias 2013hours="hours -p 2013"
alias 2014hours="hours -p 2014"
alias 2015hours="hours -p 2015"
alias weeklyhours="hours -p 'weekly this year' register --empty"
alias monthlyhours="hours -p 'monthly this year' register --empty"
alias weeklybillablehours="weeklyhours jobs not:unbilled --depth 3"
alias monthlybillablehours="monthlyhours jobs not:unbilled --depth 3"

# money

# one journal per year, included by current and all journals:
#export LEDGER_FILE=~/personal/current.journal
#export LEDGER_FILE=~/personal/all.journal

alias jan="hledger -p jan"
alias feb="hledger -p feb"
alias mar="hledger -p mar"
alias apr="hledger -p apr"
alias may="hledger -p may"
alias jun="hledger -p jun"
alias jul="hledger -p jul"
alias aug="hledger -p aug"
alias sep="hledger -p sep"
alias oct="hledger -p oct"
alias nov="hledger -p nov"
alias dec="hledger -p dec"
alias 2006='hledger -f ~/personal/2006.journal'
alias 2007='hledger -f ~/personal/2007.journal'
alias 2008='hledger -f ~/personal/2008.journal'
alias 2009='hledger -f ~/personal/2009.journal'
alias 2010='hledger -f ~/personal/2010.journal'
alias 2011='hledger -f ~/personal/2011.journal'
alias 2012='hledger -f ~/personal/2012.journal'
alias 2013='hledger -f ~/personal/2013.journal'
alias 2014='hledger -f ~/personal/2014.journal'
alias 2015='hledger -f ~/personal/2015.journal'
alias all='hledger -f ~/personal/all.journal'
alias household='hledger -f ~/personal/household.journal'
alias add='hledger add'

alias bankbalances='hledger bal assets:bank liabilities:credit -E'
alias cashflow="hledger balance '^assets:(bank|cash)'"
alias incexp="hledger balance '(^income|^expenses|^equity:draw)'"
# show activity in these accounts this and last month
alias cash="hledger -d 'd>=[last month]' reg 'assets:cash' -B"
alias checking="hledger -d 'd>=[last month]' reg 'assets:bank:wells fargo:checking' -B"
# show daily cleared checking balance - for reconciling with online bank statement
alias checkingcleared="checking --cleared --period 'daily to tomorrow'"
# show checking balance from today forward
alias checkingfuture="checking -d 'd>=[yesterday]'"

# generate a chart and view it in emacs
function chart () {
  hledger chart $* && emacsclient -n hledger.png
}

# old ledger 2.6 scripts

function BalanceSheet() {
    echo "Balance sheet as of `date`"
    echo "totals include sub-accounts"
    echo
    ledger -n --balance-format '%10T  %2_%-a\n' --display "l<=3" --basis --subtotal $* balance assets
    echo
    ledger -n --balance-format '%10T  %2_%-a\n' --display "l<=3" --basis --subtotal $* balance liabilities
    echo
    ledger -nE --balance-format '%10T  %2_%-a\n' --display "l<=4" --basis --subtotal $* balance equity
    echo
    echo "`ledger --balance-format '%10T  %2_%-a\n' --basis $* balance liabilities equity | tail -1`liabilities + equity"
    echo
    ledger --balance-format '%10T  %2_%-a\n' --basis $* balance assets liabilities | tail -2
}

function IncomeStatement() {
    echo "Income statement for `date +%Y` as of `date`"
    echo "totals include sub-accounts"
    echo
    ledger -n --balance-format '%10(-T)  %2_%-a\n' --display "l<=3" --basis --subtotal $* balance income
    echo
    ledger -n --balance-format '%10(-T)  %2_%-a\n' --display "l<=2" --basis --subtotal $* balance expenses -equity
    echo
    ledger --balance-format '%10(-T)  %2_%-a\n' --basis $* balance income expenses -equity | tail -2
}

# function CashflowStatement () {
#     echo "Cashflow statement for `date +%Y`"
#     #echo "(totals include sub-accounts)"
#     echo
#     cat <<EOF
# cash flows from operating activities
#   net income as on income statement
#   add: depreciation
#   add: allowance for doubtful accounts
#   deduct: increase in inventory
#   deduct: increase in prepaid expenses
#   deduct: decrease in accounts payable
# cash flows from investing activities
#   cash received from investments sold
#   less: cash paid for store equipment
# cash flows from financing activities
#   cash paid for dividends
#
# Increase in cash: 
# Cash at the beginning of the year:
# Cash at the end of the year:
# EOF
# }
