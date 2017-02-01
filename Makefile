all: asm rom.bin tetris.bin

asm: asm.c
	cc -g -o asm asm.c

rom.bin: asm defs.asm rom.asm
	./asm -s 0x20000 -o rom.bin -l rom.lst defs.asm rom.asm

tetris.bin: asm tetris.asm defs.asm
	./asm -b 0x400 -o tetris.bin -l tetris.lst defs.asm tetris.asm

clean:
	rm -f asm *.lst *.bin

