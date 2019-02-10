
.PHONY: command-options clean

all: command-options hledger-completion.bash

hledger-completion.bash: hledger-completion.bash.m4 commands-list.txt query-filters.txt generic-options.txt
	m4 hledger-completion.bash.m4 > $@

generic-options.txt:
	hledger -h | ./output-options.sh | sort -u > $@

commands.txt:
	hledger | ./output-commands.sh | grep -v ^hledger > $@
	echo ui  >> $@
	echo web >> $@
	echo api >> $@

commands-list.txt: commands.txt
	paste -sd, $^ | tr -d '\n' > $@

#query-filters.txt:
	# The query filters are hard to extract!
	# hledger help --cat hledger | sed -n '/^QUERIES/,/^[A-Z]/p'

command-options: commands.txt
	parallel -j8 'hledger {} -h | ./output-options.sh | sort -u > options-{}.txt' < commands.txt

clean:
	rm -f commands*.txt generic-options.txt options-*.txt
	rm -f hledger-completion.bash
