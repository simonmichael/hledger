# Some standard commands to benchmark. "quickbench" runs these by default.
# There is another set of benchmarks in the hledger package: hledger/bench/bench.hs
# Here is a quick benchmarking guide. Note these are quick measurements which
# can be affected by system activity. Usually this isn't a problem. The last
# (criterion) is more robust.
# 
# Generate the test journals: make samplejournals 
# Get quickbench: stack install quickbench
# Measure performance:
# time ./bench.sh      # show if these work, what they do, total time
# quickbench [OPTS]    # time each command, one or more times
# make bench           # time each command with several hledger versions (BENCHEXES in Makefile) 
# stack bench hledger  # time a different set of benchmarks (bench/bench.hs) 
# stack bench hledger --ba --criterion  # time more carefully, using criterion 

# hledger -f examples/100x100x10.journal print
# hledger -f examples/1000x1000x10.journal print
hledger -f examples/10000x1000x10.journal print
# hledger -f examples/10000x1000x10.journal print ff
#hledger -f examples/100000x1000x10.journal print
#hledger -f examples/100000x1000x10.journal print ff

# hledger -f examples/100x100x10.journal register
# hledger -f examples/1000x1000x10.journal register
hledger -f examples/10000x1000x10.journal register
# hledger -f examples/10000x1000x10.journal register ff
#hledger -f examples/100000x1000x10.journal register
#hledger -f examples/100000x1000x10.journal register ff

# hledger -f examples/100x100x10.journal balance
# hledger -f examples/1000x1000x10.journal balance
hledger -f examples/10000x1000x10.journal balance
# hledger -f examples/10000x1000x10.journal balance ff
#hledger -f examples/100000x1000x10.journal balance
#hledger -f examples/100000x1000x10.journal balance ff
hledger -f examples/1000x1000x10.journal balance --weekly
hledger -f examples/10000x1000x10.journal balance --weekly

