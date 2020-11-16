#!/bin/bash
#
# This scripts expects stdin formatted like this:
# <multi-line csv file>
# RULES
# <multi-line rules>
#
awk -v CSV="t.$$.csv" -v RULES="t.$$.csv.rules" '
BEGIN{output=CSV}
/^RULES/{output=RULES}
!/^RULES/{print $0 >output}'

trap "rm -f t.$$.csv t.$$.csv.rules" EXIT ERR

# Remove variable file name from error messages
:; ( hledger -f csv:t.$$.csv --rules-file t.$$.csv.rules print "$@" ) \
       2> >( sed -Ee "s/t.*.csv/input/" >&2 )
