# hledger fish completions

set -l has_input_flags 'add check close commodities diff files help import prices rewrite test'
set -l has_reporting_flags 'accounts activity areg aregister bal balance balancesheet balancesheetequity bs bse cashflow cf codes descriptions help incomestatement is notes payees prices print print-unique reg register register-match roi stats tags txns'

# Commands
complete -c hledger -f -n __fish_use_subcommand -a accounts                -d 'Show account names'
complete -c hledger -f -n __fish_use_subcommand -a activity                -d 'Show postings-per-interval bar charts'
complete -c hledger -f -n __fish_use_subcommand -a add                     -d 'Add transactions using guided prompts'
complete -c hledger -f -n __fish_use_subcommand -a areg                    -d 'Show register for a single account'
complete -c hledger -f -n __fish_use_subcommand -a aregister               -d 'Show register for a single account'
complete -c hledger -f -n __fish_use_subcommand -a bal                     -d 'Show balance changes/end balances/budgets'
complete -c hledger -f -n __fish_use_subcommand -a balance                 -d 'Show balance changes/end balances/budgets'
complete -c hledger -f -n __fish_use_subcommand -a balancesheet            -d 'Show assets, liabilities, and net worth'
complete -c hledger -f -n __fish_use_subcommand -a balancesheetequity      -d 'Show assets, liabilities, and equity'
complete -c hledger -f -n __fish_use_subcommand -a bs                      -d 'Show assets, liabilities, and net worth'
complete -c hledger -f -n __fish_use_subcommand -a bse                     -d 'Show assets, liabilities, and equity'
complete -c hledger -f -n __fish_use_subcommand -a cashflow                -d 'Show changes in liquid assets'
complete -c hledger -f -n __fish_use_subcommand -a cf                      -d 'Show changes in liquid assets'
complete -c hledger -f -n __fish_use_subcommand -a check                   -d 'Check journal for errors'
complete -c hledger -f -n __fish_use_subcommand -a close                   -d 'Generate balance-resetting transactions'
complete -c hledger -f -n __fish_use_subcommand -a codes                   -d 'Show transaction codes'
complete -c hledger -f -n __fish_use_subcommand -a commodities             -d 'Show commodity/currency symbols'
complete -c hledger -f -n __fish_use_subcommand -a demo                    -d 'Demo hledger features'
complete -c hledger -f -n __fish_use_subcommand -a descriptions            -d 'Show transaction descriptions'
complete -c hledger -f -n __fish_use_subcommand -a diff                    -d 'Compare two journal files'
complete -c hledger -f -n __fish_use_subcommand -a files                   -d 'Show input file paths'
complete -c hledger -f -n __fish_use_subcommand -a help                    -d 'Show hledger manual list'
complete -c hledger -f -n __fish_use_subcommand -a import                  -d 'Add new transactions from other files'
complete -c hledger -f -n __fish_use_subcommand -a incomestatement         -d 'Show revenues and expenses'
complete -c hledger -f -n __fish_use_subcommand -a is                      -d 'Show revenues and expenses'
complete -c hledger -f -n __fish_use_subcommand -a notes                   -d 'Show posting notes'
complete -c hledger -f -n __fish_use_subcommand -a payees                  -d 'Show payee names'
complete -c hledger -f -n __fish_use_subcommand -a prices                  -d 'Show market-price records'
complete -c hledger -f -n __fish_use_subcommand -a print                   -d 'Show transactions'
complete -c hledger -f -n __fish_use_subcommand -a print-unique            -d 'Show transactions with unique descriptions'
complete -c hledger -f -n __fish_use_subcommand -a reg                     -d 'Show postings and running total'
complete -c hledger -f -n __fish_use_subcommand -a register                -d 'Show postings and running total'
complete -c hledger -f -n __fish_use_subcommand -a register-match          -d 'Show a recent posting matching a description'
complete -c hledger -f -n __fish_use_subcommand -a rewrite                 -d 'Generate automated postings'
complete -c hledger -f -n __fish_use_subcommand -a roi                     -d 'Show return on investments'
complete -c hledger -f -n __fish_use_subcommand -a stats                   -d 'Show journal statistics'
complete -c hledger -f -n __fish_use_subcommand -a tags                    -d 'Show tag names'
complete -c hledger -f -n __fish_use_subcommand -a test                    -d 'Run self-tests'
complete -c hledger -f -n __fish_use_subcommand -a txns                    -d 'Show transactions (same as print)'

# Global input flags (work with most commands that read journal)
complete -c hledger -r -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags" -s f -l file                  -d 'Input file (.journal/.csv etc)'
complete -c hledger -r -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l rules-file            -d 'CSV conversion rules file'
complete -c hledger -r -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l rules                -d 'CSV conversion rules file (short)'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l alias                -d 'Rename account (eg /old/=new)'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l anon                 -d 'Anonymize accounts and payees'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l pivot                -d 'Use a field/tag for account names'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags" -s I -l ignore-assertions      -d 'Ignore balance assertions'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l strict               -d 'Do strict checking (all error checks)'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l infer-costs          -d 'Infer costs from transaction prices'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l infer-equity         -d 'Infer equity postings'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l infer-market-prices  -d 'Infer market prices from transactions'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l commodity-style      -d 'Set commodity output style'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l verbose-tags         -d 'Show tags in the tag area explicitly'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l no-conf              -d 'Don\'t read any config file'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_input_flags $has_reporting_flags"      -l conf                 -d 'Read config file from path'

# Global reporting flags (work with reporting commands)
complete -c hledger -x -n "__fish_seen_subcommand_from $has_reporting_flags" -s b -l begin   -d 'Include postings/txns on or after DATE'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_reporting_flags" -s e -l end     -d 'Include postings/txns before DATE'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s D -l daily   -d 'Multiperiod report by day'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s W -l weekly  -d 'Multiperiod report by week'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s M -l monthly -d 'Multiperiod report by month'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s Q -l quarterly -d 'Multiperiod report by quarter'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s Y -l yearly  -d 'Multiperiod report by year'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_reporting_flags" -s p -l period  -d 'Set period expression (eg "jan", "2024", "weekly")'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags"      -l date2    -d 'Match secondary date instead of primary'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s U -l unmarked -d 'Include only unmarked postings/txns'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s P -l pending -d 'Include only pending (!) postings/txns'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s C -l cleared -d 'Include only cleared (*) postings/txns'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s R -l real    -d 'Include only non-virtual postings'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_reporting_flags"      -l depth    -d 'Hide accounts/postings deeper than N'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s E -l empty   -d 'Show items with zero amount'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s B -l cost    -d 'Convert amounts to cost at transaction time'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s V -l value   -d 'Convert amounts to market value at report end'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags"      -l auto     -d 'Apply automated posting rules'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags"      -l forecast  -d 'Generate future transactions from periodic rules'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags" -s X -l exchange -d 'Convert to commodity using market prices'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags"      -l market   -d 'Same as --value'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_reporting_flags"      -l today    -d 'Set today\'s date (for relative dates)'
complete -c hledger -f -n "__fish_seen_subcommand_from $has_reporting_flags"      -l pretty   -d 'Use prettier output'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_reporting_flags"      -l color    -d 'Use color in output'
complete -c hledger -x -n "__fish_seen_subcommand_from $has_reporting_flags"      -l colour   -d 'Use color in output (UK spelling)'

# Global help/debug flags
complete -c hledger -f -s h -l help    -d 'Show help'
complete -c hledger -f      -l tldr    -d 'Show help (tldr style)'
complete -c hledger -f      -l info    -d 'Show help (info style)'
complete -c hledger -f      -l man     -d 'Show help (man style)'
complete -c hledger -f      -l version -d 'Show version'
complete -c hledger -x      -l debug   -d 'Show debug output (1-9)'

# Command-specific completions

# add
complete -c hledger -f -n '__fish_seen_subcommand_from add' -l no-new-accounts -d 'Don\'t allow creating new accounts'

# accounts
complete -c hledger -f -n '__fish_seen_subcommand_from accounts' -l declared    -d 'Show account names declared with account directives'
complete -c hledger -f -n '__fish_seen_subcommand_from accounts' -l used        -d 'Show account names referenced by transactions'
complete -c hledger -f -n '__fish_seen_subcommand_from accounts' -l undeclared  -d 'Show only undeclared accounts'
complete -c hledger -f -n '__fish_seen_subcommand_from accounts' -l unuse     -d 'Show only unused accounts'
complete -c hledger -f -n '__fish_seen_subcommand_from accounts' -l directives  -d 'Show account directives only'
complete -c hledger -f -n '__fish_seen_subcommand_from accounts' -l tree        -d 'Show accounts as a tree'
complete -c hledger -f -n '__fish_seen_subcommand_from accounts' -l flat        -d 'Show accounts as a flat list'
complete -c hledger -f -n '__fish_seen_subcommand_from accounts' -l types       -d 'Show account types'
complete -c hledger -f -n '__fish_seen_subcommand_from accounts' -l find        -d 'Show matching account names'
complete -c hledger -f -n '__fish_seen_subcommand_from accounts' -l locations   -d 'Show file locations of account declarations'
complete -c hledger -x -n '__fish_seen_subcommand_from accounts' -l drop        -d 'Omit N leading account name parts'

# areg / aregister
for cmd in areg aregister
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l align-all   -d 'Pad numbers to align in output'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-elide    -d 'Don\'t squash boring parent accounts'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l txn-dates   -d 'Show transaction dates instead of posting dates'
    complete -c hledger -r -n "__fish_seen_subcommand_from $cmd" -s o -l output-file   -d 'Write output to FILE'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s O -l output-format -d 'Output format' -a 'txt csv html'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s w -l width         -d 'Set output width'
end

# balance / bal
for cmd in bal balance
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l change        -d 'Show balance change in each period'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l cumulative    -d 'Show balance change accumulated across periods'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s H -l historical -d 'Show historical ending balance in each period'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l tree          -d 'Show accounts as a tree'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l flat          -d 'Show accounts as a flat list'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s A -l average  -d 'Show average column in multicolumn reports'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s T -l row-total -d 'Show row total column'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s N -l no-total -d 'Omit the final total row'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l budget        -d 'Show budget performance vs goals'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l invert        -d 'Reverse sign of all amounts'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l transpose     -d 'Transpose rows and columns'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l gain          -d 'Show unrealized capital gains'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l count         -d 'Show count of postings'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l percent       -d 'Show percentages'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l sum           -d 'Show sum of posting amounts'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l valuechange   -d 'Show change in period of market value'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l related       -d 'Show related account postings'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l declared      -d 'Show declared accounts only'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-elide      -d 'Don\'t squash boring accounts in tree mode'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l summary-only  -d 'Show parent accounts only'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l sort-amount   -d 'Sort by amount instead of name'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l drop          -d 'Omit N leading account name parts'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l format        -d 'Custom line format'
    complete -c hledger -r -n "__fish_seen_subcommand_from $cmd" -s o -l output-file   -d 'Write output to FILE'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s O -l output-format -d 'Output format' -a 'txt csv html'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l layout        -d 'Set layout'
end

# balancesheet / bs
for cmd in balancesheet bs
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l change        -d 'Show balance change in each period'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l cumulative    -d 'Show accumulated balance change'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s H -l historical -d 'Show historical ending balance'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l tree          -d 'Show accounts as a tree'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l flat          -d 'Show accounts as a flat list'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-elide      -d 'Don\'t squash boring accounts'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-total      -d 'Omit the final total row'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l average       -d 'Show average column'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l row-total     -d 'Show row total column'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l sort-amount   -d 'Sort by amount'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l gain          -d 'Show unrealized capital gains'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l percent       -d 'Show percentages'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l valuechange   -d 'Show change in market value'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l budget        -d 'Show budget vs performance'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l declared      -d 'Show declared accounts only'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l summary-only  -d 'Show parent accounts only'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l drop          -d 'Omit N leading account name parts'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l format        -d 'Custom line format'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l layout        -d 'Set layout'
    complete -c hledger -r -n "__fish_seen_subcommand_from $cmd" -s o -l output-file   -d 'Write output to FILE'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s O -l output-format -d 'Output format' -a 'txt csv html'
end

# balancesheetequity / bse
for cmd in balancesheetequity bse
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l change        -d 'Show balance change in each period'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l cumulative    -d 'Show accumulated balance change'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s H -l historical -d 'Show historical ending balance'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l tree          -d 'Show accounts as a tree'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l flat          -d 'Show accounts as a flat list'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-elide      -d 'Don\'t squash boring accounts'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-total      -d 'Omit the final total row'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l average       -d 'Show average column'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l row-total     -d 'Show row total column'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l sort-amount   -d 'Sort by amount'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l gain          -d 'Show unrealized capital gains'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l percent       -d 'Show percentages'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l valuechange   -d 'Show change in market value'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l budget        -d 'Show budget vs performance'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l declared      -d 'Show declared accounts only'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l summary-only  -d 'Show parent accounts only'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l drop          -d 'Omit N leading account name parts'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l format        -d 'Custom line format'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l layout        -d 'Set layout'
    complete -c hledger -r -n "__fish_seen_subcommand_from $cmd" -s o -l output-file   -d 'Write output to FILE'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s O -l output-format -d 'Output format' -a 'txt csv html'
end

# cashflow / cf
for cmd in cashflow cf
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l change        -d 'Show balance change in each period'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l cumulative    -d 'Show accumulated balance change'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s H -l historical -d 'Show historical ending balance'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l tree          -d 'Show accounts as a tree'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l flat          -d 'Show accounts as a flat list'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-elide      -d 'Don\'t squash boring accounts'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-total      -d 'Omit the final total row'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l average       -d 'Show average column'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l row-total     -d 'Show row total column'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l sort-amount   -d 'Sort by amount'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l gain          -d 'Show unrealized capital gains'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l percent       -d 'Show percentages'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l valuechange   -d 'Show change in market value'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l budget        -d 'Show budget vs performance'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l declared      -d 'Show declared accounts only'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l summary-only  -d 'Show parent accounts only'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l drop          -d 'Omit N leading account name parts'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l format        -d 'Custom line format'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l layout        -d 'Set layout'
    complete -c hledger -r -n "__fish_seen_subcommand_from $cmd" -s o -l output-file   -d 'Write output to FILE'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s O -l output-format -d 'Output format' -a 'txt csv html'
end

# incomestatement / is
for cmd in incomestatement is
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l change        -d 'Show balance change in each period'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l cumulative    -d 'Show accumulated balance change'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s H -l historical -d 'Show historical ending balance'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l tree          -d 'Show accounts as a tree'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l flat          -d 'Show accounts as a flat list'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-elide      -d 'Don\'t squash boring accounts'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-total      -d 'Omit the final total row'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l average       -d 'Show average column'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l row-total     -d 'Show row total column'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l sort-amount   -d 'Sort by amount'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l gain          -d 'Show unrealized capital gains'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l percent       -d 'Show percentages'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l valuechange   -d 'Show change in market value'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l budget        -d 'Show budget vs performance'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l declared      -d 'Show declared accounts only'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l summary-only  -d 'Show parent accounts only'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l drop          -d 'Omit N leading account name parts'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l format        -d 'Custom line format'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l layout        -d 'Set layout'
    complete -c hledger -r -n "__fish_seen_subcommand_from $cmd" -s o -l output-file   -d 'Write output to FILE'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s O -l output-format -d 'Output format' -a 'txt csv html'
end

# close
complete -c hledger -f -n '__fish_seen_subcommand_from close' -l close          -d 'Show just closing transaction'
complete -c hledger -f -n '__fish_seen_subcommand_from close' -l open           -d 'Show just opening transaction'
complete -c hledger -x -n '__fish_seen_subcommand_from close' -l close-acct     -d 'Set closing equity account name'
complete -c hledger -x -n '__fish_seen_subcommand_from close' -l close-desc     -d 'Set closing transaction description'
complete -c hledger -x -n '__fish_seen_subcommand_from close' -l open-acct      -d 'Set opening equity account name'
complete -c hledger -x -n '__fish_seen_subcommand_from close' -l open-desc      -d 'Set opening transaction description'
complete -c hledger -f -n '__fish_seen_subcommand_from close' -l explicit       -d 'Show all amounts explicitly'
complete -c hledger -f -n '__fish_seen_subcommand_from close' -l interleaved    -d 'Show interleaved close/open entries'
complete -c hledger -x -n '__fish_seen_subcommand_from close' -l migrate        -d 'Migrate close/open to a different account'
complete -c hledger -f -n '__fish_seen_subcommand_from close' -l retain         -d 'Retain earnings in the closing entry'
complete -c hledger -f -n '__fish_seen_subcommand_from close' -l assert         -d 'Add balance assertions'
complete -c hledger -x -n '__fish_seen_subcommand_from close' -l assertion-type -d 'Set assertion type'
complete -c hledger -x -n '__fish_seen_subcommand_from close' -l assign         -d 'Assign unassigned amounts to an account'
complete -c hledger -f -n '__fish_seen_subcommand_from close' -l show-costs     -d 'Show costs in output'
complete -c hledger -x -n '__fish_seen_subcommand_from close' -l round          -d 'Set rounding mode'

# import
complete -c hledger -f -n '__fish_seen_subcommand_from import' -l dry-run   -d 'Just show transactions to import'
complete -c hledger -f -n '__fish_seen_subcommand_from import' -l catchup   -d 'Mark all prior transactions as imported'

# prices
complete -c hledger -f -n '__fish_seen_subcommand_from prices' -l costs           -d 'Print transaction prices from postings'
complete -c hledger -f -n '__fish_seen_subcommand_from prices' -l show-reverse    -d 'Also show reverse prices'
complete -c hledger -f -n '__fish_seen_subcommand_from prices' -l inverted-costs  -d 'Print transaction inverted prices'

# print / txns
for cmd in print txns
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l explicit       -d 'Show all amounts explicitly'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l new            -d 'Show only newer transactions since last run'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s m -l match     -d 'Show transaction most similar to STR'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l show-costs     -d 'Show costs in output'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l round          -d 'Set rounding mode'
    complete -c hledger -r -n "__fish_seen_subcommand_from $cmd" -s o -l output-file   -d 'Write output to FILE'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s O -l output-format -d 'Output format' -a 'txt csv html json'
end

# print-unique
complete -c hledger -f -n '__fish_seen_subcommand_from print-unique' -l explicit       -d 'Show all amounts explicitly'
complete -c hledger -f -n '__fish_seen_subcommand_from print-unique' -l new            -d 'Show only newer transactions since last run'
complete -c hledger -x -n '__fish_seen_subcommand_from print-unique' -s m -l match     -d 'Show transaction most similar to STR'
complete -c hledger -f -n '__fish_seen_subcommand_from print-unique' -l show-costs     -d 'Show costs in output'
complete -c hledger -r -n '__fish_seen_subcommand_from print-unique' -s o -l output-file   -d 'Write output to FILE'
complete -c hledger -x -n '__fish_seen_subcommand_from print-unique' -s O -l output-format -d 'Output format' -a 'txt csv html json'

# reg / register
for cmd in reg register
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l cumulative      -d 'Show running total from report start'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s H -l historical  -d 'Show historical running balance'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s A -l average     -d 'Show running average'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -s r -l related     -d 'Show postings\' siblings instead'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l invert           -d 'Reverse sign of all amounts'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l align-all        -d 'Pad numbers to align in output'
    complete -c hledger -f -n "__fish_seen_subcommand_from $cmd" -l no-elide         -d 'Don\'t squash boring parent accounts'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s m -l match       -d 'Show transaction most similar to STR'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s w -l width       -d 'Set output width'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -l sort             -d 'Sort order'
    complete -c hledger -r -n "__fish_seen_subcommand_from $cmd" -s o -l output-file   -d 'Write output to FILE'
    complete -c hledger -x -n "__fish_seen_subcommand_from $cmd" -s O -l output-format -d 'Output format' -a 'txt csv html'
end

# rewrite
complete -c hledger -f -n '__fish_seen_subcommand_from rewrite' -l add-posting -d 'Add a posting to account'
complete -c hledger -f -n '__fish_seen_subcommand_from rewrite' -l diff         -d 'Generate diff suitable for patch(1)'

# roi
complete -c hledger -f -n '__fish_seen_subcommand_from roi' -l cashflow    -d 'Show all amounts used to compute returns'
complete -c hledger -x -n '__fish_seen_subcommand_from roi' -l investment  -d 'Query to select investment transactions'
complete -c hledger -x -n '__fish_seen_subcommand_from roi' -l profit-loss -d 'Query to select P&L transactions'
complete -c hledger -x -n '__fish_seen_subcommand_from roi' -l pnl         -d 'Query to select P&L transactions (short)'

# stats
complete -c hledger -f -n '__fish_seen_subcommand_from stats' -l verbose    -d 'Show verbose statistics'
complete -c hledger -r -n '__fish_seen_subcommand_from stats' -s o -l output-file -d 'Write output to FILE'

# tags
complete -c hledger -f -n '__fish_seen_subcommand_from tags' -l values     -d 'Show tag values'
complete -c hledger -f -n '__fish_seen_subcommand_from tags' -l parsed     -d 'Show tags as parsed'
complete -c hledger -f -n '__fish_seen_subcommand_from tags' -l declared   -d 'Show declared tags only'

# codes
complete -c hledger -f -n '__fish_seen_subcommand_from codes' -l declared  -d 'Show declared codes only'
complete -c hledger -f -n '__fish_seen_subcommand_from codes' -l used      -d 'Show used codes only'

# notes
complete -c hledger -f -n '__fish_seen_subcommand_from notes' -l declared  -d 'Show declared notes only'
complete -c hledger -f -n '__fish_seen_subcommand_from notes' -l used      -d 'Show used notes only'

# descriptions
complete -c hledger -f -n '__fish_seen_subcommand_from descriptions' -l declared -d 'Show declared descriptions only'
complete -c hledger -f -n '__fish_seen_subcommand_from descriptions' -l used     -d 'Show used descriptions only'

# payees
complete -c hledger -f -n '__fish_seen_subcommand_from payees' -l declared -d 'Show declared payees only'
complete -c hledger -f -n '__fish_seen_subcommand_from payees' -l used     -d 'Show used payees only'

# check (subcommands)
complete -c hledger -f -n '__fish_seen_subcommand_from check' -l strict     -d 'Perform all checks'
complete -c hledger -f -n '__fish_seen_subcommand_from check' -l quiet      -d 'Don\'t show checked items'
