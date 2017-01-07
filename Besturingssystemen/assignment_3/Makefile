TARGET = schedule
SOURCES = mt19937ar.c simul2015.c schedule.c linux-x86_64/mem_alloc.o
HEADERS = mt19937.h mem_alloc.h schedule.h

CFLAGS = -Wall -O2 -g -std=c99
LDFLAGS = -lm
CC = gcc
RST2PDF = rst2pdf -s twelvepoint --footer='\#\#\#Page\#\#\#/\#\#\#Total\#\#\#' --real-footnotes

.PHONY: all tarball clean moreclean

all: $(TARGET)

tarball: schedule.tar.gz

schedule.tar.gz: $(GENERATED) $(SOURCES) $(HEADERS) $(META) AUTHORS Makefile
	tar -czf $@ $^

$(TARGET): $(SOURCES:.c=.o)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) $(LDFLAGS)

$(SOURCES:.c=.o): $(HEADERS)

.SUFFIXES: .pdf .rst
doc: README.pdf

.rst.pdf:
	$(RST2PDF) $<
