; ***************************************************************************
; 
; Configurable parameters

MHZ=5000000			; 5MHz clock speed
BAUD=9600			; serial baud rate

; ***************************************************************************
;
; 6522 VIA 

VIA=$100000				; base address

	; registers

VIA_B=$100000				; ORB/IRB
VIA_A=$101000				; ORA/IRA
VIA_DDR_B=$102000
VIA_DDR_A=$103000
VIA_T1CL=$104000
VIA_T1CH=$105000
VIA_T1LL=$106000
VIA_T1LH=$107000
VIA_T2L=$108000
VIA_T2H=$109000

VIA_ACR=$10b000
VIA_PCR=$10c000
VIA_IFR=$10d000
VIA_IER=$10e000

	; port A bits of interest

A_KBCLK=$02				; PS/2 keyboard clock (o/c)

	; port B bits of interest

B_XMIT=$80				; serial transmit output
B_KBDATA=$40				; PS/2 keyboard data (o/c)

	; other bits of interest

IRQ_ENABLE=$80
IRQ_T1=$40
IRQ_T2=$20				
IRQ_CB2=$08

; ***************************************************************************
;
; RAM storage

; direct page
; keep these where the 6502 did, $000000-$0000FF.

		org $0

scratch0:	bss 1			; general scratch use
scratch1:	bss 1			

vblk:		bss 1			; $FF/$00 = vertical blanking/not

	; RS-232 

recvc:		bss 1			; serial input shift register
recvcnt:	bss 1			; bit counter for serial input
recvbit:	bss 1			; receive bit state
recvhd:		bss 2			; head of recvbuf (write ptr)
recvtl:		bss 2			; tail of recvbuf (read ptr)

	; XMODEM 

xmaddr:		bss 3			; far pointer to buffer
xmblkno:	bss 1			; expected block #
xmretry:	bss 1			; retry counter
xmctl:		bss 1			; either ACK or NAK

	; CRC-16 routines

crc:		bss 2			; 16-bit CRC accumulator

; non direct-page storage

		org $100

stack:		bss 256			; stack's traditional location

	; RS-232 receive buffer must be a full page,
	; and must start at a page boundary.

recvbuf:	bss 256			; RS-232 receive buffer


