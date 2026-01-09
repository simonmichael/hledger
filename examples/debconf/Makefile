recent: \
 dc25 \
 dc24 \

all: \
 dc25 \
 dc24 \
 dc23 \
 dc22 \
 dc21 \
 dc20 \
 dc19 \
 dc18 \
 dc17 \

# make dc-YY - clone the budget directory for this year and make it hledger-readable
dc%:
	git clone --filter=blob:none --sparse http://salsa.debian.org/debconf-team/public/data/$@
	git -C $@ sparse-checkout set --no-cone budget '!budget/invoices'
	git -C $@ apply --allow-empty ../patches/$@.patch

# make check-ledger, check-hledger - check readability of all years
check-%:
	@for d in dc*; do printf "$$d: "; $* -f $$d/budget/journal.ledger stats >/dev/null && echo ok; done


# maintenance

# make rg-PAT - ripgrep for PAT in ledger,inc,db files in all years
rg-%:
	rg -g '*.{ledger,inc,db}' '$*'

# apply hledger readability fixes to all years
fixups:
	# ensure at least two whitespace chars before amounts:
	sed -i -E 's/(\w)\t(\w)/ \t/' dc*/budget/*.ledger
	# ensure a decimal mark in commodity declarations:
	sed -i -E 's/^( +format +\w+ +[0-9]+(,[0-9]+)+)$$/\1.00/' dc*/budget/commodities.inc
	# convert amount expressions to simple amounts (use ledgereval or ledger eval)

# show uncommitted changes in each year
diff:
	for d in dc*; do echo $$d:; git -C $$d diff; done

# save patch files for each year
save-patches:
	for d in dc*; do GIT_PAGER= git -C $$d diff >patches/$$d.patch; done

