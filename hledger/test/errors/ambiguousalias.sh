#!/usr/bin/env -S sh
# Convert to a commodity alias which two files declare differently.
cd ../commodity-aliases && hledger -f ./usage.journal -f ./mt-alias.journal bal -X Mt
