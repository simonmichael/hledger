SLUG=haskellerz

TYPES=\
	slidy \
	revealjs \
#	dzslides \
#	s5 \
#	slideous \

default: build open watch

open:
	open *.html

build:
	for T in $(TYPES); do pandoc --slide-level=2 -s -t $$T $(SLUG).md -o $(SLUG).$$T.html; done

watch:
	ls *.md | entr bash -c 'make -s build && date'

clean:
	rm -f $(SLUG).*.html
