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

PARSE_COMMANDS := ./parse-commands.sh
PARSE_OPTIONS := ./parse-options.sh

EXTENSIONS := ui web
INSTALLED_EXTENSIONS := $(foreach EXT,$(EXTENSIONS),$(shell type hledger-$(EXT) >/dev/null 2>&1 && echo $(EXT)))

COMMANDS := $(sort $(shell $(PARSE_COMMANDS)) $(INSTALLED_EXTENSIONS))

ifneq ($(.SHELLSTATUS),0)
$(error Error running $(PARSE_COMMANDS))
endif

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

.PHONY: install
install:
	@install -v -d "$(DESTDIR)$(BASHCOMPDIR)"
	@install -v -m 0644 hledger-completion.bash "$(DESTDIR)$(BASHCOMPDIR)/hledger"
	@for ext in $(EXTENSIONS); do \
		printf "symlink " ; \
		ln -sfv hledger "$(DESTDIR)$(BASHCOMPDIR)/hledger-$$ext" ; \
	done

.PHONY: uninstall
uninstall:
	@rm -vf "$(DESTDIR)$(BASHCOMPDIR)/hledger"
	@for ext in $(EXTENSIONS); do \
		rm -vf "$(DESTDIR)$(BASHCOMPDIR)/hledger-$$ext" ; \
	done

hledger-completion.bash: $(M4DEPS)
	m4 -g hledger-completion.bash.m4 > $@

commands.txt:
	printf "%s\n" $(COMMANDS) > $@

commands-list.txt:
	printf "%s,"  $(COMMANDS) | sed 's/,$$//' > $@

generic-options.txt:
	$(PARSE_OPTIONS) > $@

options-%.txt:
	$(PARSE_OPTIONS) $* > $@

.PHONY: clean
clean:
	rm -f commands*.txt generic-options.txt options-*.txt

.PHONY: clean-all
clean-all: clean
	rm -f hledger-completion.bash
