#!/bin/sh
#
# sh version, ported from bash so freebsd users can run these tests.
# This scripts expects stdin formatted like this:
# <multi-line ssv file (at least one line required, even if blank)>
# RULES
# <multi-line rules>
#
# Here, unlike in csvtest.sh, the ssv extension is intentionally NOT set
# This allows us to verify that the prefix detection is working

cat > t.$$.input
sed '1,/^RULES/d' t.$$.input > t.$$.rules
sed '/^RULES/,$d' t.$$.input > t.$$

trap 'rm -f t.$$.input t.$$ t.$$.rules t.$$.stderr' EXIT

# Remove variable file name from error messages
mkfifo t.$$.stderr
sed -Ee "s/t\.$$/input/" t.$$.stderr >&2 &

hledger -f ssv:t.$$ print "$@" 2> t.$$.stderr
