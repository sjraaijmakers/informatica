COMPILER = civcc
ASSEMBLER = civas
VM = civvm

OUT_FILES = core.out coreio.out array.out main.out

all: build run

build: $(OUT_FILES)

run:
	$(VM) $(OUT_FILES)

%.cvcasm: %.cvc
	$(COMPILER) -o $@ $^

%.out: %.cvcasm
	$(ASSEMBLER) -o $@ $^

clean:
	rm $(OUT_FILES)
