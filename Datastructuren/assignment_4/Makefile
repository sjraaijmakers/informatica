CC=gcc
#CC=clang

# no-unused-parameter used here to supress warnings in the incomplete
# template code. Remove this flag when you have added your own code.
CFLAGS= -std=c99 -Wall -Wextra -pedantic -Wno-unused-parameter

# debug flags.
CFLAGS= -g3 -DDEBUG -std=c99 -Wall -Wextra -pedantic

all: autocomplete

trie.o: ll.h

autocomplete: autocomplete.o trie.o ll.o

autocomplete.o: trie.h ll.h

# Rule to unzip input when needed for running the checks
%.txt: %.txt.gz
	gzip -dk $<

check: all war-and-peace-ascii.txt
	# short test
	./autocomplete -P test1.txt
	@echo
	./autocomplete -p h -P -r remove_list.txt test1.txt
	@echo
	# longer test
	./autocomplete war-and-peace-ascii.txt
	@echo
	./autocomplete -p thea war-and-peace-ascii.txt

clean:
	rm -f *.o autocomplete war-and-peace-ascii.txt war-and-peace-mod-the.txt
