# Setting the number of job runners like this in the makefile only works in
# GNU Make 4.3 or later. Older versions will require that either an env
# variable be set before running or command line flag be passed at runtime to
# get parallel jobs.
MAKEFLAGS += --jobs=$(shell nproc 2>/dev/null || printf 8)

EUID := $(shell id -u)

ifeq ($(EUID),0)
PREFIX := /usr/local
endif

ifdef PREFIX
BASHCOMPDIR := $(PREFIX)/share/bash-completion/completions
else
XDG_DATA_HOME ?= $(HOME)/.local/share
BASH_COMPLETION_USER_DIR ?= $(XDG_DATA_HOME)/bash-completion
BASHCOMPDIR := $(BASH_COMPLETION_USER_DIR)/completions
endif

DESTDIR ?=

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
INSTALLED_EXTENSIONS := $(foreach EXT,$(EXTENSIONS),$(shell type hledger-$(EXT) >/dev/null 2>&1 && echo $(EXT)))

COMMANDS := $(sort $(shell $(PARSE_COMMANDS)) $(INSTALLED_EXTENSIONS))
CMDOPTFILES := $(foreach CMD,$(COMMANDS),options-$(CMD).txt)

all: hledger-completion.bash

.PHONY: install
install:
	@install -v -d "$(DESTDIR)$(BASHCOMPDIR)"
	@install -v -m 0644 hledger-completion.bash "$(DESTDIR)$(BASHCOMPDIR)/hledger"
	@for ext in $(INSTALLED_EXTENSIONS); do \
		printf "symlink " ; \
		ln -sfv hledger "$(DESTDIR)$(BASHCOMPDIR)/hledger-$$ext" ; \
	done

.PHONY: uninstall
uninstall:
	@rm -vf "$(DESTDIR)$(BASHCOMPDIR)/hledger"
	@for ext in $(INSTALLED_EXTENSIONS); do \
		rm -vf "$(DESTDIR)$(BASHCOMPDIR)/hledger-$$ext" ; \
	done

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
	rm -f $(COMMANDS_TMP) commands*.txt generic-options.txt options-*.txt
	rm -f hledger-completion.bash


# Basic REGEX debugging targets. Example usage:
# diff <(make -s debug-options) <(make -s debug-options CMD=reg)

.PHONY: debug-commands
debug-commands:
	$(PARSE_COMMANDS)

.PHONY: debug-options
debug-options: CMD :=
debug-options:
	hledger $(CMD) -h | $(PARSE_OPTIONS)
