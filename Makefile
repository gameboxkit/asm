all: asm rom.bin tetris.bin ps2.bin text.bin

asm: asm.c
	cc -g -o asm asm.c

rom.bin: asm defs.asm rom.asm
	./asm -s 0x20000 -b 0x60000 -o rom.bin -l rom.lst defs.asm rom.asm

tetris.bin: asm tetris.asm defs.asm
	./asm -b 0x60000 -o tetris.bin -l tetris.lst defs.asm tetris.asm

ps2.bin: asm ps2.asm defs.asm
	./asm -b 0x70000 -o ps2.bin -l ps2.lst defs.asm ps2.asm

text.bin: asm text.asm defs.asm
	./asm -b 0x70000 -o text.bin -l text.lst defs.asm text.asm

clean:
	rm -f asm *.lst *.bin

