# Check error messages of hledger in $PATH against current error tests.
test:
	@printf "Running error message tests with hledger $$(hledger --version | awk '{print $$2}'):\n"
	shelltest *.test

TESTJOURNALS=*.j

# Update error message tests and readme based on the latest test journals
# and error output of hledger in $PATH.
update: tests readme

tests:
	@printf "Updating *.test with the error messages of hledger $$(hledger --version | awk '{print $$2}')\n"
	@read -p "ok ? Press enter: "
	for f in $(TESTJOURNALS); do make -s $$(basename $$f .j).test; done
	make -s test

%.test: %.j
	head -1 $< | sed -E 's%#!/usr/bin/env -S (.*)%$$$$$$ \1 $<%' >$@
	echo ">>>2" >>$@
	shelltest $@ | awk '/Got stderr/{f=1;next}/Expected exit code/{f=0}f' >>$@
	echo ">>>=1" >>$@

readme: $(TESTJOURNALS)
	@printf "Updating README.md with the error messages of hledger $$(hledger --version | awk '{print $$2}')\n"
	@read -p "ok ? Press enter: "
	sed '/<!-- GENERATED: -->/q' <README.md >README.md.tmp
	echo "$$(hledger --version | cut -d, -f1) error messages:" >>README.md.tmp
	for f in $(TESTJOURNALS); do \
		printf '\n### %s\n```\n%s\n```\n\n' "$$(basename "$$f" .j)" "$$(./"$$f" 2>&1)"; \
	done >>README.md.tmp
	mv README.md.tmp README.md

