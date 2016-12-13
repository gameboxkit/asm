all: asm rom.bin ps2.bin forth.bin

asm: asm.c
	cc -g -o asm asm.c

rom.bin: asm defs.asm rom.asm
	./asm -s 0x20000 -o rom.bin -l rom.lst defs.asm rom.asm

ps2.bin: asm defs.asm ps2.asm
	./asm -b 0x400 -o ps2.bin -l ps2.lst defs.asm ps2.asm

forth.bin: asm defs.asm forth.asm
	./asm -b 0x400 -o forth.bin -l forth.lst defs.asm forth.asm

clean:
	rm -f asm *.lst *.bin

