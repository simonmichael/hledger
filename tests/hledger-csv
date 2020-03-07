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

hledger -f csv:t.$$.csv --rules-file t.$$.csv.rules print "$@"
