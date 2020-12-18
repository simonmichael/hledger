# Setting the number of job runners like this in the makefile only works in
# GNU Make 4.3 or later. Older versions will require that either an env
# variable be set before running or command line flag be passed at runtime to
# get parallel jobs.
MAKEFLAGS += --jobs=$(shell nproc 2>/dev/null || printf 8)

# Parse hledger's help and output all commands and command aliases in
# parenthesis. Do not output single letter command aliases, it's not useful.
COMMANDS_TMP := commands.tmp

define PARSE_COMMANDS :=
hledger > $(COMMANDS_TMP) ;
{ \
	sed -rn 's/^\s+([a-z][-a-z]+)\s+.*/\1/p' $(COMMANDS_TMP) ; \
	sed -rn 's/^\s+[a-z][-a-z]+\s+\(([a-z][ ,a-z]+)\).*/\1/p' $(COMMANDS_TMP) | \
	sed 's/\s*,\s*/\n/g' | \
	sed '/^.$$/d' ; \
} | sed '/^hledger/d' | sort -u
endef

# Parse hledger's help and output long options. Do not propose single letter
# completions. Options requiring an argument make that explicit by appending the
# equal sign (=)
define PARSE_OPTIONS :=
sed -rn '/^\s+-/p' | \
sed -rn 's/^\s{1,4}(-.)?\s{1,4}(--[a-zA-Z][-_a-zA-Z0-9]+=?).*/\2/p' | \
sort -u
endef

EXTENSIONS := ui web api

COMMANDS := $(sort $(shell $(PARSE_COMMANDS)) $(EXTENSIONS))
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

all: hledger-completion.bash

hledger-completion.bash: $(M4DEPS)
	m4 hledger-completion.bash.m4 > $@

commands.txt:
	printf "%s\n" $(COMMANDS) > $@

commands-list.txt:
	printf "%s,"  $(COMMANDS) | sed 's/,$$//' > $@

generic-options.txt:
	hledger -h | $(PARSE_OPTIONS) > $@

options-%.txt:
	hledger $* -h | $(PARSE_OPTIONS) > $@

.PHONY: clean
clean:
	rm -f commands*.{txt,tmp} generic-options.txt options-*.txt
	rm -f hledger-completion.bash


# Basic REGEX debugging targets
.PHONY: debug-commands debug-options

debug-commands:
	$(PARSE_COMMANDS)

debug-options: CMD :=
debug-options:
	hledger $(CMD) -h | $(PARSE_OPTIONS)
