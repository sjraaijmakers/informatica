%.s: %.cvc
	bin/civcc -o $@ $<

%.o: %.s
	bin/civas -o $@ $<

run: main.o core.o coreio.o array.o
	bin/civvm $^

clean:
	rm -f main *.o *.s
