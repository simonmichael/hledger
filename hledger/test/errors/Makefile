HLEDGER ?= hledger

# j, csv, timeclock, timedot files which are executable
ERRORSCRIPTS := $$(find . -perm +rwx \( -name '*.j' -or -name '*.csv' -or -name '*.timeclock' -or -name '*.timedot' -or -name '*.sh' \))

# Check error messages of $HLEDGER against current error tests.
# A few tests (csvstdinrules.sh) may use "hledger" in $PATH instead of $HLEDGER.
test:
	@printf "Running error message tests with hledger $$($(HLEDGER) --version | awk '{print $$2}'):\n"
	shelltest -w $(HLEDGER) *.test

# Update error message tests and readme based on the latest test journals
# and error output of hledger in $PATH.
update: tests readme

tests:
	@printf "(Re)generating *.test with the error messages of hledger $$($(HLEDGER) --version | awk '{print $$2}')\n"
	@read -p "ok ? Press enter: "
	@for f in $(ERRORSCRIPTS); do echo "HLEDGER=$(HLEDGER) ./hledger2shelltest $$f"; HLEDGER=$(HLEDGER) ./hledger2shelltest $$f; done

readme: $(ERRORSCRIPTS)
	@printf "Updating README.md with the error messages of hledger $$($(HLEDGER) --version)\n"
	@read -p "ok ? Press enter: "
	sed '/<!-- GENERATED: -->/q' <README.md >README.md.tmp
	echo "$$($(HLEDGER) --version | cut -d, -f1) error messages:" >>README.md.tmp
	for f in $(ERRORSCRIPTS); do \
		printf '\n### %s\n```\n%s\n```\n\n' "$$(echo "$$f" | sed -E 's/\.[^.]+$$//')" "$$(./"$$f" 2>&1)"; \
	done >>README.md.tmp
	mv README.md.tmp README.md

