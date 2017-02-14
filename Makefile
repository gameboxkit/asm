all: asm rom.bin romhi.bin

asm: asm.c
	cc -g -o asm asm.c

rom.bin: asm defs.asm rom.asm
	./asm -s 0x20000 -b 0x60000 -o rom.bin -l rom.lst defs.asm rom.asm

romhi.bin: rom.bin
	dd if=rom.bin of=romhi.bin skip=65536 bs=1 count=4096

clean:
	rm -f asm *.lst *.bin

