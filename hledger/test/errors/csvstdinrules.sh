#!/usr/bin/env -S  sh
# Second space above is significant, prevents shelltest's "-w hledger" substitution.
# Try to read CSV from stdin without specifying a rules file.
echo | hledger -fcsv:- check
