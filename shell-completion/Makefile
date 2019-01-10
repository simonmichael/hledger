
.PHONY: commands clean

all: generic-options.txt commands hledger-completion.bash

generic-options.txt:
	hledger -h | ./output-options.sh > $@

commands.txt:
	hledger | ./output-commands.sh > $@

commands-list.txt: commands.txt
	paste -sd, $^ | tr -d '\n' > $@

commands: commands.txt
	parallel 'hledger {} -h | ./output-options.sh > options-{}.txt' < commands.txt

hledger-completion.bash: hledger-completion.bash.m4 commands-list.txt
	m4 hledger-completion.bash.m4 > $@

clean:
	rm -f *.txt hledger-completion.bash
