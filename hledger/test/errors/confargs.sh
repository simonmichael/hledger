#!/usr/bin/env -S sh
# A config file line which is not valid command line arguments.
hledger --conf ./confargs.conf -f /dev/null files
