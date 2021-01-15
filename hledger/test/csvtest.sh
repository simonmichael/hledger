#!/bin/sh
#
# sh version, ported from bash so freebsd users can run these tests.
# This scripts expects stdin formatted like this:
# <multi-line csv file>
# RULES
# <multi-line rules>
#
cat > t.$$.input
sed '1,/^RULES/d' t.$$.input > t.$$.csv.rules
sed '/^RULES/,$d' t.$$.input > t.$$.csv

trap 'rm -f t.$$.input t.$$.csv t.$$.csv.rules t.$$.stderr' EXIT

# Remove variable file name from error messages
mkfifo t.$$.stderr
sed -Ee "s/t\.$$\.csv/input/" t.$$.stderr >&2 &

hledger -f csv:t.$$.csv --rules-file t.$$.csv.rules print "$@" 2> t.$$.stderr
