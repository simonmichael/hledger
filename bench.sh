# Some standard commands to benchmark. "quickbench" runs these by default.
# There is another set of benchmarks in the hledger package: hledger/bench/bench.hs
# Here is a quick benchmarking guide. Note these are quick measurements which
# can be affected by system activity. Usually this isn't a problem. The last
# (criterion) is more robust.
# 
# Generate the test journals: 
# just samplejournals 
#
# Get quickbench: 
# git clone https://github.com/simonmichael/quickbench
# cd quickbench
# stack install  # must be run in source dir
#
# Measure performance:
# time ./bench.sh      # show if these work, what they do, total time (GNU time also shows max memory)
# quickbench [OPTS]    # time each command, one or more times
# stack bench hledger  # time a different set of benchmarks (bench/bench.hs) 
# stack bench hledger --ba --criterion  # time more carefully, using criterion 

# commands to benchmark:

# hledger -f examples/100txns-100accts.journal print
# hledger -f examples/1ktxns-1kaccts.journal print
hledger -f examples/10ktxns-1kaccts.journal print
# hledger -f examples/10ktxns-1kaccts.journal print ff
#hledger -f examples/100ktxns-1kaccts.journal print
#hledger -f examples/100ktxns-1kaccts.journal print ff

# hledger -f examples/100txns-100accts.journal register
# hledger -f examples/1ktxns-1kaccts.journal register
hledger -f examples/10ktxns-1kaccts.journal register
# hledger -f examples/10ktxns-1kaccts.journal register ff
#hledger -f examples/100ktxns-1kaccts.journal register
#hledger -f examples/100ktxns-1kaccts.journal register ff

# hledger -f examples/100txns-100accts.journal balance
# hledger -f examples/1ktxns-1kaccts.journal balance
hledger -f examples/10ktxns-1kaccts.journal balance
# hledger -f examples/10ktxns-1kaccts.journal balance ff
#hledger -f examples/100ktxns-1kaccts.journal balance
#hledger -f examples/100ktxns-1kaccts.journal balance ff
#hledger -f examples/1ktxns-1kaccts.journal balance --weekly
#hledger -f examples/10ktxns-1kaccts.journal balance --weekly

