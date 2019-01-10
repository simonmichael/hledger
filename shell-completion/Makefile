
.PHONY: commands clean

all: generic-options.txt commands hledger-completion.bash

generic-options.txt:
	hledger -h | ./output-options.sh > $@

commands.txt:
	hledger | ./output-commands.sh > $@

commands-list.txt: commands.txt
	paste -sd, $^ | tr -d '\n' > $@

commands: commands.txt
	#parallel 'touch {}.command' < commands.txt
	parallel 'hledger {} -h | ./output-options.sh > options-{}.txt' < commands.txt

# It's possible to call this rule explicitly but it's not invoked automatically.
# Better generate *-options.txt with the 'commands' phony rule.
%-options.txt: %.command
	hledger $* -h | ./output-options.sh > $@

hledger-completion.bash: hledger-completion.bash.m4 commands-list.txt
	m4 hledger-completion.bash.m4 > $@

clean:
	rm -f *.commands *.txt hledger-completion.bash
