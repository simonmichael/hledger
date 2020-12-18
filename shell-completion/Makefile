# Setting the number of job runners like this in the makefile only works in
# GNU Make 4.3 or later. Older versions will require that either an env
# variable be set before running or command line flag be passed at runtime to
# get parallel jobs.
MAKEFLAGS += --jobs=$(shell nproc 2>/dev/null || printf 8)

all: hledger-completion.bash

COMMANDS := $(sort $(shell hledger | ./output-commands.sh | grep -v ^hledger | sort -u) ui web api)
CMDOPTFILES := $(foreach CMD,$(COMMANDS),options-$(CMD).txt)

define M4DEPS :=
hledger-completion.bash.m4 \
hledger-completion.bash.stub \
commands.txt \
commands-list.txt \
query-filters.txt \
generic-options.txt \
$(CMDOPTFILES)
endef

hledger-completion.bash: $(M4DEPS)
	m4 hledger-completion.bash.m4 > $@

generic-options.txt:
	hledger -h | ./output-options.sh | sort -u > $@

commands.txt:
	printf "%s\n" $(COMMANDS) > $@

commands-list.txt:
	printf "%s,"  $(COMMANDS) | sed 's/,$$//' > $@

#query-filters.txt:
	# The query filters are hard to extract!
	# hledger help --cat hledger | sed -n '/^QUERIES/,/^[A-Z]/p'

options-%.txt:
	hledger $* -h | ./output-options.sh $* | sort -u > $@

.PHONY: clean
clean:
	rm -f commands*.txt generic-options.txt options-*.txt
	rm -f hledger-completion.bash
	rm -rf _{commands,options}.tmp
