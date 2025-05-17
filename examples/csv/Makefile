# * Finance makefile   -*- mode:makefile-gmake; eval:(outshine-mode 1); -*-
# ** ABOUT
# This makefile, based on SM's circa 2023, is a place to keep small finance-related scripts.
# In particular, it is responsible for collecting and importing downloaded CSV files
# (which is why it is in the examples/csv/ folder, even though not only useful for CSV).
# It handles the tedious file management and importing. Hopefully more of this
# will be done by hledger some day.
#
# 2023-05: More of this is done by hledger now (see 1.30's CSV `source` rule),
# making this makefile pretty much obsolete.
#
# To list available targets: make or make help-PAT
# To debug a target: make SHELL='sh -x' [-n] TARGET
# When logic or quoting or syntax gets hard: use shell scripts in bin/ instead.

# -*- -*- sets modes in Emacs. outshine mode treats "# *" as outline headings, cycleable with TAB.

# ensure failure in piped commands causes make to fail. Good for some but not others.
#SHELL=sh -o pipefail

YEAR:=$(shell date +%Y)
JOURNAL=$(YEAR).journal

RG=rg -IN --sort=path

help: # list make targets
	@printf "Targets:\n"
	@$(RG) '^# \*\* (.*)|^(\w[^:]*):($$| [^#]*(# [^#]*)?$$)' -or '$$1 $$2|$$4' $(MAKEFILE_LIST) | column -t -s'|' || true
# Shows second level outshine headings and targets beginning with a word character and not followed by a ## same-line comment.

help-%: # list make targets matching a pattern
	@make -s help | $(RG) "$*" || true

PHONY:  ## Depend on this to make phony targets (more easily than .PHONY: foo bar baz..)

###############################################################################
# ** DOWNLOAD

# CSVs should be downloaded/exported manually to this directory.
# You should remove any old downloads first, so browsers don't save the new ones with a wrong name.
DOWNLOADDIR=~/Downloads

init:  # ensure all importable CSVs exist. Run this once at the start to keep make happy.
	touch $(CSVS)

# Keep the following three lists in sync.

# 1. CSV files with standard names, from which we will import transactions.
CSVS=\
	wf-checking.csv \
	wf-savings.csv \
	paypal.csv \
	vanguard.csv \
#	stripe.csv \
#	stripeexpress.csv \

# 2. How to make these CSV files / where to move them from.
wf-checking.csv:      $(DOWNLOADDIR)/Checking1.csv; mv $< $@
wf-savings.csv:       $(DOWNLOADDIR)/Savings2.csv ; mv $< $@
paypal.csv:           $(DOWNLOADDIR)/Download.csv ; mv $< $@
# paypal.csv:         PHONY; paypaljson | paypaljson2csv > $@
vanguard.csv:         $(DOWNLOADDIR)/ofxdownload.csv; mv $< $@
#stripe.csv:           $(DOWNLOADDIR)/Itemized_balance_change_from_activity_USD_*.csv; mv $< $@
#stripeexpress.csv:    $(DOWNLOADDIR)/StripeExpressActivity_*.csv; mv $< $@

# 3. Marking them as intermediate files (https://www.gnu.org/software/make/manual/html_node/Chained-Rules.html).
.INTERMEDIATE: \
	$(DOWNLOADDIR)/Checking1.csv \
	$(DOWNLOADDIR)/Savings2.csv \
	$(DOWNLOADDIR)/Download.csv \
	$(DOWNLOADDIR)/ofxdownload.csv \
#	$(DOWNLOADDIR)/Itemized_balance_change_from_activity_USD_*.csv \
#	$(DOWNLOADDIR)/StripeExpressActivity_*.csv \

csv: $(CSVS)  # move any downloaded CSVs to the standard filenames above

###############################################################################
# ** IMPORT

# hledger .latest files used for remembering the csvs' import state
IMPORTLATESTFILES=$(CSVS:%=.latest.%)

# hledger .rules files used for parsing the csvs
IMPORTRULESFILES=\
	$(CSVS:=.rules) \
	wf.rules \
	common.rules \

Import: csv import  # move all downloaded CSVs here and import any new transactions to the journal

import:  # import any new transactions from CSV to the journal
	@date >>import.log
	hledger -f $(JOURNAL) import $(CSVS) 2>>import.log || echo "Failed, check import.log"
# Avoids showing hledger errors on stdout in case it is being called by cron and emailed.
# Logs to import.log.

import-dry:  # like import but just do a dry run, without changing the journal or import state
	hledger -f $(JOURNAL) import --dry-run $(CSVS)

import-dry-watch:  # rerun import-dry as files change
	watchexec -- make import-dry

import-dry-unknowns:  # like import-dry but show only the transactions that rules have not categorised
	@hledger -f $(JOURNAL) import --dry-run $(CSVS) | hledger -f- -I print unknown

import-dry-unknowns-watch:  # rerun import-dry-unknowns as files change
	watchexec -- make import-dry-unknowns

###############################################################################
# ** PRICES

PRICELOG=$(YEAR)prices.journal
MARKET_PRICES=~/src/PTA/hledger-scripts/market-prices/market-prices.py <market-prices.conf.json

prices: $(PRICELOG)  # download, show and save today's prices for the journal's commodities

$(PRICELOG): PHONY  # download today's market prices from coinbase/FT with market_prices.py, show them, and save them in YYYYprices.journal
	@(echo; $(MARKET_PRICES)) | tee -a $@

prices-dry: PHONY  # download and show today's prices, without saving
	@$(MARKET_PRICES)

###############################################################################
# ** REPORTS

bs:  # simplified balance sheet
	hledger bs -2 -V --infer-market-prices --layout=bare
