all: asm rom.bin

asm: asm.c
	cc -g -o asm asm.c

rom.bin: asm rom.asm
	./asm -s 0x20000 -b 0x60000 -o rom.bin -l rom.lst rom.asm

clean:
	rm -f asm *.lst *.bin

