# Makefile
# Generates HTML/PDF invoices from a markdown template

help:
	@echo "make copy     # copy last invoice to YYYYMMab.md, open in emacs"
	@echo "(edit)"
	@echo "make invoice  # make YYYYMMab.pdf and YYYYMMab.html, git commit, add invoice transaction to journal"

CSS=invoice.css
LAST=`ls -t 2*md | head -1`
# must end with "ab":
NEW=$(shell date +%Y%mab)

copy:
	@echo "copying invoice $(LAST) to $(strip $(NEW)).md"
	@cp $(LAST) $(strip $(NEW)).md
	emacsclient -s2 -n $(strip $(NEW)).md

invoice:
	@echo "making invoice $(NEW)"
	@make $(NEW)
	@git add $(NEW).{md,html,pdf}
	@git commit -m "invoice $(NEW)" -- $(NEW).{md,html,pdf}
	@printf "`date +%Y-%m-%d` * (`date +%Y%m`ab) AB Inc. | invoice\n    (assets:receivable:ab)                 \$$1000  ; TODO:adjust\n\n" >>$$LEDGER_FILE

%ab:
	@make $@.pdf $@.html

%.pdf: %.md $(CSS) #logo.jpg
	pandoc $< -t html5 --css $(CSS) -o $@ --metadata title=" "
# --metadata title to silence warning, space to avoid unwanted display

%.pdf-watch:
	ls | entr make -s $*.pdf

%.html: %.md $(CSS) #logo.jpg
	pandoc $< -t html5 -s --css $(CSS) -o $@ --metadata title=" "

%.html-watch:
	ls | entr make -s $*.html
