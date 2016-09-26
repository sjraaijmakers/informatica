# Enter you UVAID here
UVAID=12345
PROGNAME=wordfinder

# Commands for creating the submit archive
MKDIR=mkdir
PACK=tar -jcvf
CP=cp
RM=rm -rf

CC=gcc
# You can use clang as an alternative compiler
#CC=clang

#CFLAGS=  -std=c99 -Wall -Wextra -pedantic
# debug flags.
CFLAGS= -g3 -DDEBUG -std=c99 -Wall -Wextra -pedantic

all: wordfinder

wordfinder: wordfinder.o debug.h

check: wordfinder
	@echo "First five serial numbers are valid."
	@echo "The last two are invalid and should fail the tests."
	./wordfinder < serials.txt

clean:
	rm -f *.o wordfinder

tarball: all
	$(MAKE) clean
	$(MKDIR) $(PROGNAME)-$(UVAID)
	$(CP) * $(PROGNAME)-$(UVAID) || true
	$(PACK) $(PROGNAME)-$(UVAID).tbz $(PROGNAME)-$(UVAID)/*
	$(RM) $(PROGNAME)-$(UVAID)
