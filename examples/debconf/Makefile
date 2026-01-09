recent: \
 dc26 \
 dc25 \
 dc24 \
 dc23 \

dc%:
	git clone --filter=blob:none --sparse http://salsa.debian.org/debconf-team/public/data/$@
	git -C $@ sparse-checkout set --no-cone budget '!budget/invoices'
