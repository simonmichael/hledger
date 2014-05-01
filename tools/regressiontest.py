#!/usr/bin/python
# test whether hledger output matches ledger's
# Simon Michael 2007

from os import *
from posix import *

files = [
    'data/test.dat',
    'data/test1.dat',
#    getenv('LEDGER'),
    ]

commands = [
    'balance petty',
    '-s balance',
    '-s balance cash',
    'register',
    'register cash',
    'print',
    ]

do = system
rule = lambda s: "="*30 + s + "="*30

def regtest(file, cmd):
    """Print a heading and the diff of ledger and hledger output. No diff
    output is good."""
    print rule('%s:%s' % (file,cmd))
    putenv('LEDGER',file)
    do('ledger %s >1; ./hledger.hs %s >2; diff 1 2' % (cmd,cmd))

for f in files:
    for c in commands:
        regtest(f,c)

