#!/usr/bin/env -S sh
# A config file with a syntax error.
hledger --conf ./confparse.conf -f /dev/null files
