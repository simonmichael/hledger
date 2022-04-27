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

# Generate a shelltest. Run the test script/journal to generate the error message.
# Since the error will contain an absolute file path, we must:
# 1. remove most of the file path
# 2. test with a (multiline) regex rather than literal text
# 3. backslash-quote most forward slashes in error messages
# 4. neutralise any remaining problematic error text (eg in parseable-regexps.test)
%.test: %.j
	head -1 $< | sed -E 's%#!/usr/bin/env -S (.*)%$$$$$$ \1 $<%' >$@
	printf ">>>2 /" >>$@
	-./$< 2>&1 | sed -E \
		-e 's%(hledger: Error: ).*/\./(.*)%\1.*\2%' \
		-e 's%/%\\/%g' \
		-e 's%alias \\/\(\\/%alias \\/\\(\\/%' \
		-e 's%compiled: \(%compiled: \\(%' \
		>>$@
	printf "/\n>>>= 1" >>$@

readme: $(TESTJOURNALS)
	@printf "Updating README.md with the error messages of hledger $$(hledger --version | awk '{print $$2}')\n"
	@read -p "ok ? Press enter: "
	sed '/<!-- GENERATED: -->/q' <README.md >README.md.tmp
	echo "$$(hledger --version | cut -d, -f1) error messages:" >>README.md.tmp
	for f in $(TESTJOURNALS); do \
		printf '\n### %s\n```\n%s\n```\n\n' "$$(basename "$$f" .j)" "$$(./"$$f" 2>&1)"; \
	done >>README.md.tmp
	mv README.md.tmp README.md

