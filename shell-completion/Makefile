
.PHONY: command-options clean

all: command-options hledger-completion.bash

hledger-completion.bash: hledger-completion.bash.m4 commands-list.txt query-filters.txt generic-options.txt
	m4 hledger-completion.bash.m4 > $@

generic-options.txt:
	hledger -h | ./output-options.sh > $@

commands.txt:
	hledger | ./output-commands.sh | grep -v ^hledger > $@

commands-list.txt: commands.txt
	paste -sd, $^ | tr -d '\n' > $@

command-options: commands.txt
	parallel -j8 'hledger {} -h | ./output-options.sh > options-{}.txt' < commands.txt

clean:
	rm -f *.txt hledger-completion.bash
