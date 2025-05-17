# This is another set of quickbench benchmarks, as in bench.sh.
# These are used to compare Ledger and hledger, so they should all
# be commands that Ledger can run.
# Example: quickbench -f bench-ledger.sh -w ledger,hledger-1.23

hledger -f examples/10000x1000x10.journal print
hledger -f examples/10000x1000x10.journal register
hledger -f examples/10000x1000x10.journal balance
hledger -f examples/100000x1000x10.journal balance
hledger -f examples/100000x1000x10.journal balance ff
