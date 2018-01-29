## TAX REPORTS
#
# Tested with hledger 1.5.99, osx high sierra.
# "make" generates the following reports in three formats, 
# "make preview" prints them on stdout.
# (You'll need to customise these for your needs.)
#
# business revenue
# business expenses
# possibly business/tax-related expenses:
#  trips, travel
#  trips, other
#  personal development
#  health
#  tax
# non-business/non-tax-related:
#  other income
#  other expenses

Y=2017
J=../$(Y).journal
H=TERM=dumb hledger -f $(J) -V -Y --alias expenses:personal=expenses bal  # depth:1 -N

STDOUTREPORTS= \
	$(Y)-business-revenue.-     \
	$(Y)-business-expenses.-    \
	$(Y)-trips-travel.-         \
	$(Y)-trips-other.-          \
	$(Y)-personal-development.- \
	$(Y)-health.-               \
	$(Y)-tax.-                  \
	$(Y)-other-income.-         \
	$(Y)-other-expenses.-       \

CSVREPORTS=$(STDOUTREPORTS:.-=.csv)
HTMLREPORTS=$(STDOUTREPORTS:.-=.html)
TXTREPORTS=$(STDOUTREPORTS:.-=.txt)

REPORTS=\
	$(CSVREPORTS) $(Y)-all.csv \
	$(HTMLREPORTS) \
	$(TXTREPORTS) \

reports: csv html txt

preview:
	@make -s $(STDOUTREPORTS) | sed -e 's/\. Balance changes in ...../:/'

csv: $(CSVREPORTS) $(Y)-all.csv

$(Y)-all.csv: $(CSVREPORTS)
	@for f in $(CSVREPORTS); do \
		echo '"",""'      ;\
		echo '"'$${f/.csv/:}'",""' ;\
		cat $$f           ;\
	done >$@

html: $(HTMLREPORTS)

txt: $(TXTREPORTS)

open: $(REPORTS)
	open $^

clean:
	rm -f $(REPORTS)

# Save various subreports with given file extension (html, csv),
# or with - extension print on stdout; in appropriate format.

$(Y)-business-revenue.%: $(J)
	@[[ $* == - ]] && printf "\n$(subst -, ,$@)" || true # if on stdout, print a title
	$(H) $(if $(filter-out -,$*),-o $@)  cur:. revenues:business depth:3 --invert

$(Y)-business-expenses.%: $(J)
	@[[ $* == - ]] && printf "\n$(subst -, ,$@)" || true
	$(H) $(if $(filter-out -,$*),-o $@)  cur:. expenses:business depth:3

$(Y)-trips-travel.%: $(J)
	@[[ $* == - ]] && printf "\n$(subst -, ,$@)" || true
	$(H) $(if $(filter-out -,$*),-o $@)  not:business expenses.*travel

$(Y)-trips-other.%: $(J)
	@[[ $* == - ]] && printf "\n$(subst -, ,$@)" || true
	$(H) $(if $(filter-out -,$*),-o $@)  not:business tag:trip expenses not:travel not:health not:'personal development'

$(Y)-personal-development.%: $(J)
	@[[ $* == - ]] && printf "\n$(subst -, ,$@)" || true
	$(H) $(if $(filter-out -,$*),-o $@)  not:business cur:. 'expenses.*personal development'

$(Y)-health.%: $(J)
	@[[ $* == - ]] && printf "\n$(subst -, ,$@)" || true
	$(H) $(if $(filter-out -,$*),-o $@)  not:business cur:. expenses.*health

$(Y)-tax.%: $(J)
	@[[ $* == - ]] && printf "\n$(subst -, ,$@)" || true
	$(H) $(if $(filter-out -,$*),-o $@)  not:business cur:. 'expenses.*tax\b'

$(Y)-other-income.%: $(J)
	@[[ $* == - ]] && printf "\n$(subst -, ,$@)" || true
	$(H) $(if $(filter-out -,$*),-o $@)  not:business cur:. depth:3 revenues:personal --invert

$(Y)-other-expenses.%: $(J)
	@[[ $* == - ]] && printf "\n$(subst -, ,$@)" || true
	$(H) $(if $(filter-out -,$*),-o $@)  not:business cur:. depth:3 expenses not:business not:tag:trip not:expenses.*health not:'expenses.*personal development' not:'expenses.*tax\b'

