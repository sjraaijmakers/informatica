# Add additional .c files here if you added any yourself.
ADDITIONAL_SOURCES =

# Add additional .h files here if you added any yourself.
ADDITIONAL_HEADERS =

# -- Do not modify below this point - will get replaced during testing --
TARGETS = libsys.a libmm.a test test-dumb
libmm_SRC = mm.c
libmm_dumb_SRC = mm-dumb.c
libsys_SRC = sys.c
test_SRC = test.c
SRC = $(libmm_SRC) $(libmm_dumb_SRC) $(libsys_SRC) $(test_SRC) $(ADDITIONAL_SOURCES)
HEADERS = myalloc.h xcheck.h sys.h $(ADDITIONAL_HEADERS)
META = AUTHORS report.pdf $(wildcard LEADERBOARD_NAME)
TRACES = $(wildcard *.t)
CHECKFILES = check/parse_authors.py $(wildcard check/*.sh) $(wildcard check/*.t)

CC = gcc
CFLAGS += -std=c11 -W -Wall -g -D_POSIX_C_SOURCE=200809L
LDFLAGS = -L.
LIBS = -lm
RST2PDF = rst2pdf -s tenpoint --footer='\#\#\#Page\#\#\#/\#\#\#Total\#\#\#' --real-footnotes

AR = ar
RANLIB = ranlib

.PHONY: all localcheck check doc tarball clean moreclean

all: $(TARGETS)

tarball: myalloc.tar.gz

myalloc.tar.gz: $(SRC) $(HEADERS) $(META) Makefile $(TRACES) $(CHECKFILES)
	tar -czf $@ $^

localcheck: all
	bash check/check.sh

check: myalloc.tar.gz
	curl --insecure -F "file=@$<" https://csa.science.uva.nl/bs2015/upload2/

clean:
	rm -f $(TARGETS)
	rm -f *.o *~ *.out
	rm -f *.aux *.log *.ltx

moreclean: clean
	rm -f $(GENERATED)

libmm.a: $(libmm_SRC:.c=.o)
	$(AR) cru $@ $^
	$(RANLIB) $@

libmm-dumb.a: $(libmm_dumb_SRC:.c=.o)
	$(AR) cru $@ $^
	$(RANLIB) $@

libsys.a: $(libsys_SRC:.c=.o)
	$(AR) cru $@ $^
	$(RANLIB) $@

test: libmm.a libsys.a $(test_SRC:.c=.o)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(test_SRC:.c=.o) -lmm -lsys $(LIBS)

test-dumb: libmm-dumb.a libsys.a $(test_SRC:.c=.o)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(test_SRC:.c=.o) -lmm-dumb -lsys $(LIBS)

$(SRC:.c=.o): $(HEADERS)

report.pdf:
	touch $@

.SUFFIXES: .pdf .rst
doc: README.pdf

.rst.pdf:
	$(RST2PDF) $<
