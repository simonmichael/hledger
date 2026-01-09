recent: \
 dc26 \
 dc25 \
 dc24 \

all: \
 dc26 \
 dc25 \
 dc24 \
 dc23 \
 dc22 \
 dc21 \
 dc20 \
 dc19 \
 dc18 \
 dc17 \

dc%:
	git clone --filter=blob:none --sparse http://salsa.debian.org/debconf-team/public/data/$@
	git -C $@ sparse-checkout set --no-cone budget '!budget/invoices'

# make check-ledger, check-hledger - check readability
check-%:
	@for d in dc*; do printf "$$d: "; $* -f $$d/budget/journal.ledger stats >/dev/null && echo ok; done

