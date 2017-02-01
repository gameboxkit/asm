; ***************************************************************************
; 
; Configurable parameters

MHZ=5000000			; 5MHz clock speed
BAUD=9600			; serial baud rate

; ***************************************************************************
;
; RAM storage

; direct page
; keep these where the 6502 did, $000000-$0000FF.

		org $0

scratch0:	bss 1			; general scratch use
scratch1:	bss 1			

	; RS-232 

recvc:		bss 1			; serial input shift register
recvcnt:	bss 1			; bit counter for serial input
recvbit:	bss 1			; receive bit state
recvhd:		bss 2			; head of recvbuf (write ptr)
recvtl:		bss 2			; tail of recvbuf (read ptr)

	; XMODEM 

xmaddr:		bss 2			; far pointer to buffer
xmblkno:	bss 1			; expected block #
xmretry:	bss 1			; retry counter
xmctl:		bss 1			; either ACK or NAK

	; CRC-16 routines

crc:		bss 2			; 16-bit CRC accumulator

; non direct-page storage


		org $100

stack:		bss 256			; stack's traditional location
S0=.-1					; initial S value

	; RS-232 receive buffer must be a full page,
	; and must start at a page boundary.

recvbuf:	bss 256			; RS-232 receive buffer


; ***************************************************************************
;
; vectors
;

	; bounce

		org $ffd0

irqvec:		jmp <irq
sysvec:		jmp <sysenter
resvec:		jmp <reset

	; native

		org $ffe0

		word 0			; reserved
		word 0			; reserved
		word 0			; COP
		word sysvec		; BRK
		word 0 			; ABORT
		word 0			; NMI
		word 0 			; reserved
		word irqvec		; IRQ

	; emulation

		org $fffc

		word resvec		; RESET

; ***************************************************************************


		org $10000

crclo: 	byte $00,$21,$42,$63,$84,$A5,$C6,$E7,$08,$29,$4A,$6B,$8C,$AD,$CE,$EF
	byte $31,$10,$73,$52,$B5,$94,$F7,$D6,$39,$18,$7B,$5A,$BD,$9C,$FF,$DE
	byte $62,$43,$20,$01,$E6,$C7,$A4,$85,$6A,$4B,$28,$09,$EE,$CF,$AC,$8D
	byte $53,$72,$11,$30,$D7,$F6,$95,$B4,$5B,$7A,$19,$38,$DF,$FE,$9D,$BC
	byte $C4,$E5,$86,$A7,$40,$61,$02,$23,$CC,$ED,$8E,$AF,$48,$69,$0A,$2B
	byte $F5,$D4,$B7,$96,$71,$50,$33,$12,$FD,$DC,$BF,$9E,$79,$58,$3B,$1A
	byte $A6,$87,$E4,$C5,$22,$03,$60,$41,$AE,$8F,$EC,$CD,$2A,$0B,$68,$49
	byte $97,$B6,$D5,$F4,$13,$32,$51,$70,$9F,$BE,$DD,$FC,$1B,$3A,$59,$78
	byte $88,$A9,$CA,$EB,$0C,$2D,$4E,$6F,$80,$A1,$C2,$E3,$04,$25,$46,$67
	byte $B9,$98,$FB,$DA,$3D,$1C,$7F,$5E,$B1,$90,$F3,$D2,$35,$14,$77,$56
	byte $EA,$CB,$A8,$89,$6E,$4F,$2C,$0D,$E2,$C3,$A0,$81,$66,$47,$24,$05
	byte $DB,$FA,$99,$B8,$5F,$7E,$1D,$3C,$D3,$F2,$91,$B0,$57,$76,$15,$34
	byte $4C,$6D,$0E,$2F,$C8,$E9,$8A,$AB,$44,$65,$06,$27,$C0,$E1,$82,$A3
	byte $7D,$5C,$3F,$1E,$F9,$D8,$BB,$9A,$75,$54,$37,$16,$F1,$D0,$B3,$92
	byte $2E,$0F,$6C,$4D,$AA,$8B,$E8,$C9,$26,$07,$64,$45,$A2,$83,$E0,$C1
	byte $1F,$3E,$5D,$7C,$9B,$BA,$D9,$F8,$17,$36,$55,$74,$93,$B2,$D1,$F0

crchi: 	byte $00,$10,$20,$30,$40,$50,$60,$70,$81,$91,$A1,$B1,$C1,$D1,$E1,$F1
	byte $12,$02,$32,$22,$52,$42,$72,$62,$93,$83,$B3,$A3,$D3,$C3,$F3,$E3
	byte $24,$34,$04,$14,$64,$74,$44,$54,$A5,$B5,$85,$95,$E5,$F5,$C5,$D5
	byte $36,$26,$16,$06,$76,$66,$56,$46,$B7,$A7,$97,$87,$F7,$E7,$D7,$C7
	byte $48,$58,$68,$78,$08,$18,$28,$38,$C9,$D9,$E9,$F9,$89,$99,$A9,$B9
	byte $5A,$4A,$7A,$6A,$1A,$0A,$3A,$2A,$DB,$CB,$FB,$EB,$9B,$8B,$BB,$AB
	byte $6C,$7C,$4C,$5C,$2C,$3C,$0C,$1C,$ED,$FD,$CD,$DD,$AD,$BD,$8D,$9D
	byte $7E,$6E,$5E,$4E,$3E,$2E,$1E,$0E,$FF,$EF,$DF,$CF,$BF,$AF,$9F,$8F
	byte $91,$81,$B1,$A1,$D1,$C1,$F1,$E1,$10,$00,$30,$20,$50,$40,$70,$60
	byte $83,$93,$A3,$B3,$C3,$D3,$E3,$F3,$02,$12,$22,$32,$42,$52,$62,$72
	byte $B5,$A5,$95,$85,$F5,$E5,$D5,$C5,$34,$24,$14,$04,$74,$64,$54,$44
	byte $A7,$B7,$87,$97,$E7,$F7,$C7,$D7,$26,$36,$06,$16,$66,$76,$46,$56
	byte $D9,$C9,$F9,$E9,$99,$89,$B9,$A9,$58,$48,$78,$68,$18,$08,$38,$28
	byte $CB,$DB,$EB,$FB,$8B,$9B,$AB,$BB,$4A,$5A,$6A,$7A,$0A,$1A,$2A,$3A
	byte $FD,$ED,$DD,$CD,$BD,$AD,$9D,$8D,$7C,$6C,$5C,$4C,$3C,$2C,$1C,$0C
	byte $EF,$FF,$CF,$DF,$AF,$BF,$8F,$9F,$6E,$7E,$4E,$5E,$2E,$3E,$0E,$1E 


	; Reset vector.
	
	; Copy ROM to RAM, set up native mode with 16-bit 
	; indexes and an 8-bit accumulator, which is our
	; default/assumed state.

reset:		jmp <1f			; set PBR properly
1:		clc			; enter native mode
		xce
		rep #>%00110000 	; 16-bit registers

		lda #$ffff		; copy ROM to RAM
		ldy #0
		ldx #0	
		mvn $0101		; top 64K
		mvn $0000		; bottom 64K, DBR=0

		lda #0			; clear hidden accumulator
		sep #>%00100000		; 8-bit accumulator

		ldx #S0			; stack lives here
		txs 		

	; Initialize the VIA.
	; This has the side effect of disabling the ROM.

		lda #>$1c
		sta <VIA_A
		lda #>$9d		
		sta <VIA_DDR_A	

		; we're running from RAM now

		lda #>$b0
		sta <VIA_B
		sta <VIA_DDR_B

		lda #>$0		; no latching,
		sta <VIA_ACR		; timer 1/2 are one-shots
		lda #>$22		; independent, negative-edge
		sta <VIA_PCR		; triggers on CA/B 1/2

		lda #>$7f		; disable all interrupts
		sta <VIA_IER		
		sta <VIA_IFR		; and clear any pending garbage

	; Initialize the RS-232 "port".

		jsr recvflush
		jsr recvidle		; initialize RS-232 to idle

	; Done.  Enable interrupts.

		cli

; receive xmodem file

1:		jsr xmdrain
		lda #>$0d		; "GO"
		jsr xmit
		lda #>$0a		; "GO"
		jsr xmit
		lda #>$47		; "GO"
		jsr xmit
		lda #>$4f
		jsr xmit
		lda #>$0d		; "GO"
		jsr xmit
		lda #>$0a		; "GO"
		jsr xmit

		stz >xmaddr		; target = $400
		lda #>$04
		sta >xmaddr+1
		jsr xmodem_receive
		bcs >1b			; did not work

	; just echo the file back out

		jsr xmdrain		; wait a bit
		jsr xmdrain
		jsr xmdrain

	; and jump to that code

		jmp <$400

; ***************************************************************************
;
; XMODEM-CRC receiver
;

HUNDRETH=MHZ/100

SOH=1
EOT=4
ACK=6
NAK=21

	; xmodem_receive - download a file via XMODEM.
	;
	; Upon entry 'xmaddr' holds the 0-bank address of
	; destination buffer.
	;
	; Upon return, carry is clear if the transfer was
	; successful, or set if there was an error.

xmodem_receive:	lda #>$01		; start block is $01
		sta >xmblkno
		lda #>$43		; 'C' for Xmodem-CRC
		sta >xmctl

1:		lda #>10		; reset retry counter
		sta >xmretry
2: 		lda >xmctl		; send protocol byte
		jsr xmit
		ldx #300		; inter-block timeout = 3 sec
		jsr xmrecv		; wait for SOH/EOT
		bcs >8f			; branch if timeout
		cmp #>EOT		; end of transmission?
		bne >3f			; no, go around

		; got EOT

		lda #>ACK		; acknowledge EOT!
		jsr xmit
		clc			; c=0 = success
		rts			

3:		cmp #>SOH		; start of block?
		bne >7f			; nope, error

		ldx #100		; intra-block timeout = 1 sec

		jsr xmrecv		; get blkno
		bcs >7f			; timeout
		cmp >xmblkno		; correct block #?
		bne >7f			; nope, error

		jsr xmrecv		; get ~blkno
		bcs >7f			
		eor #>$ff	
		cmp >xmblkno		; correct block #?
		bne >7f			; nope, error

		ldy #0			; Y = byte count
		sty >crc		; init CRC-16
4:		jsr xmrecv		; get byte
		bcs >7f			
		sta (>xmaddr),y		; write byte to buffer
		jsr updcrc		; update CRC-16
		iny			; next byte
		cpy #128		; are we done?
		bne >4b			; nope, next byte
	
		jsr xmrecv		; get CRC high byte
		bcs >7f			
		cmp >crc+1		; is it correct?
		bne >7f			; nope, error

		jsr xmrecv		; get CRC lo byte
		bcs >7f
		cmp >crc
		bne >7f
		
		inc >xmblkno		; next block

		clc
		lda >xmaddr		; bump buffer ptr
		adc #>128		; an 8-bit accumulator
		sta >xmaddr		; definitely makes this
		lda >xmaddr+1		; a tad bit awkward
		adc #>0
		sta >xmaddr+1

		lda #>ACK		
		sta >xmctl
		bra >1b

7:		lda #>NAK
		sta >xmctl
8:		jsr xmdrain
		dec >xmretry		; one less try
		bne >2b			; if not zero, try again
		sec			; no more retries, fatal 
		rts

	; receive byte into A
	; X is the timeout in hundreths of a second
	; on return, carry is clear on success, or set on timeout

xmrecv:		phx
1:		lda #>HUNDRETH		; start timer
		sta <VIA_T2L
		lda #>HUNDRETH/$100
		sta <VIA_T2H
	
2:		jsr recv		; got a char?
		bcc >9f			; yep, we're done
		lda <VIA_IFR		; is T2 expired?
		and #>IRQ_T2	
		beq >2b			; nope.
		dex			; decrement timeout
		bne >1b			; not zero, go another tick
		sec			; flag timeout
9:		plx
		rts

	; Wait for the line to clear.
	; That is, wait until the line has been silent 
	; for one second.

xmdrain:	jsr recvflush		; flush receive buffer
		ldx #100		; one second
		jsr xmrecv
		bcc >xmdrain		; if we got a char, start again
		rts

; ***************************************************************************
; 
; RS-232 

BIT_DELAY=MHZ/BAUD		; number of cycles per bit
START_DELAY=BIT_DELAY-100	; delay after start bit; 100 cycles short
BIT15_DELAY=BIT_DELAY+BIT_DELAY/2	; 1.5 bit delays

	; empty/reset the receiver buffer
	; X destroyed

recvflush:	php
		ldx #recvbuf
		sei		; mask interrupts
		stx >recvhd
		stx >recvtl
		plp		; restore IRQ mask
		rts

	; returns next byte in buffer in A
	; carry clear if data available, set otherwise

recv:		lda >recvtl
		cmp >recvhd
		sec
		beq >1f		; no data

		lda (>recvtl)
		inc >recvtl	; bump tail ptr
		clc

1:		rts

	; send byte in accumulator out the serial port
	; X, Y preserved

xmit:		sta >scratch0
		lda #>8
		sta >scratch1

		; send a start bit and start timer 2

		lda <VIA_B
		and #>~B_XMIT		; start bit is 0
		sta <VIA_B		; send it

		lda #>START_DELAY	; use a shortened 
		sta <VIA_T2L		; counter for the start bit
		lda #>START_DELAY/$100
		sta <VIA_T2H

1:		jsr 9f			; delay
		lda <VIA_B		; assume 0 bit
		and #>~B_XMIT
		ror >scratch0		; next bit -> carry
		bcc >2f
		ora #>B_XMIT		; nope, 1 bit
2:		sta <VIA_B		; send bit
		dec >scratch1		; one less bit to send
		bne >1b			; if not done, loop

		jsr 9f			; wait for last bit out

		lda <VIA_B		; stop bit is 1
		ora #>B_XMIT
		sta <VIA_B
		jsr 9f			; two stop bits
		jsr 9f			; (this will help with bad timing)

		rts

		; wait for Timer 2 to expire
		; and reset it before returning

9:		lda <VIA_IFR		; wait for T2 to expire
		and #>IRQ_T2	
		beq >9b

		clc			; reset counter
		lda #>BIT_DELAY
		adc <VIA_T2L
		sta <VIA_T2L
		lda #>BIT_DELAY/$100
		adc <VIA_T2H
		sta <VIA_T2H

		rts

; ***************************************************************************
;
; CRC-16 calculation routines
; 
; updates direct-page 'crc' variable with the byte in A.
; XXX - assumes B (hidden accumulator) is zero!

updcrc:		phx
		eor >crc+1 	
       		tax
       		lda >crc	
       		eor <crchi,X
       		sta >crc+1
      	 	lda <crclo,X
       		sta >crc
		plx
       		rts

IRQ_LATENCY=42				; IRQ latency - see below
STABLE_DELAY=BIT_DELAY-IRQ_LATENCY	; sample delay after timeout
TRANS_DELAY=BIT_DELAY+BIT_DELAY/2	; sample delay after transition

irq: 		sep #>%00100000		; 8-bit accumulator
		pha

		lda <VIA_IFR		; identify source
		bit #>IRQ_CB2		; CB2 transition?
		bne >1f

	; if it's not a CB2 transition, we assume Timer 1 expired.
	; this might not be a good assumption for long.

		lda #>STABLE_DELAY	; set Timer 1 for 1 bit time
		sta <VIA_T1CL		; (this also clears the interrupt)
		lda #>STABLE_DELAY/$100
		sta <VIA_T1CH				
		bra >2f			; IRQ_LATENCY measures to this point!
					; N.B. don't forget 4~ bounce vector

	; CB2 transition, bit state changed.

1:		lda #>TRANS_DELAY	; set Timer 1 for 1.5 bit times
		sta <VIA_T1CL
		lda #>TRANS_DELAY/$100
		sta <VIA_T1CH
		lda #>IRQ_ENABLE+IRQ_T1	; enable Timer 1 IRQ
		sta <VIA_IER

		lda >recvbit		; toggle bit state
		eor #>$ff		
		sta >recvbit
		
		lda <VIA_PCR		; invert edge sensitivity
		eor #>$40
		sta <VIA_PCR

		lda #>IRQ_CB2		 ; clear CB2 interrupt
		sta <VIA_IFR

	; both paths converge here.

2:		inc >recvcnt		; bump state machine
		beq >9f			; recvcnt=0 is start bit (done)

		lda >recvbit		; get current bit state
		and #>$80		; only care about the top
		lsr >recvc		; make room for new bit
		ora >recvc		; and save it
		sta >recvc

		lda >recvcnt		; got a full char?
		cmp #>8		
		bne >9f			; nope

	; got a char, put it in buffer

		jsr recvidle		; idle the receiver

		lda >recvhd		; room in input buffer?
		inc
		cmp >recvtl
		beq >9f			; no, skip

		lda >recvc		; stash char
		sta (>recvhd)
		inc >recvhd		; and bump the pointer

9:		pla
		rti

recvidle:	lda #>IRQ_T1		; disable Timer 1 interrupts
		sta <VIA_IER

		lda <VIA_PCR		; CB2 = negative edge trigger
		and #>$BF
		sta <VIA_PCR

		lda #>$ff		; initialize state
		sta >recvcnt		; idle receiver
		sta >recvbit		; current bit state is HIGH
		
		lda #>IRQ_ENABLE+IRQ_CB2	; enable CB2 IRQ
		sta <VIA_IER
		lda #>IRQ_CB2		; discard any if pending
		sta <VIA_IFR		; (probably unnecessary)

		rts

; ***************************************************************************
;
; system calls (invoked by BRK)
;
; On system entry, it's assumed that PBR=DBR=D=0.
; The byte following the BRK instruction is the system call number.
;
; The stack frame is set up as follows:
;
;	10,S	caller PBR		assumed to always be 0!
;	8,S	caller PC
;	7,S	caller P
;	5,S	caller A		all user registers are
; 	3,S	caller X		saved as 16-bit
;	1,S	caller Y		regardless of their state on entry
; 
; System call entry points may assume 16-bit registers, and
; the high byte of the accumulator is 0.

REG_Y=1
REG_X=3
REG_A=5
REG_P=7
REG_PC=8
REG_PBR=10

sysenter:	rep #>%00110100		; 16-bit registers, enable IRQ
		pha
		phx
		phy

		lda >REG_PC,s		; retrieve BRK + post byte
		dec
		dec
		tax
		lda >0,x
		xba			; A = system call #
		cmp #NR_SYS		; valid call?
		bcs >syserr		; nope, branch
		asl			; A is now offset into 'syscalls'
		tax
		jmp (syscalls,x)	; go!


syserr:		lda >REG_P,s		; set carry (error)
		ora #1
		sta >REG_P,s
		bra >sysexit

sysok:		lda >REG_P,s		; clear carry, (no error)
		and #$fffe
		sta >REG_P,s
sysexit:	ply
		plx
		pla
		rti

	; system call table
	;
	; system calls can assume 16-bit registers,
	; and the high byte of A is zero.

syscalls:	word sys_emit
		word sys_key

SZ_SYS=.-syscalls		; size of system call table
NR_SYS=SZ_SYS/2			; number of system calls

	; EMIT: write a character (in A) to console

sys_emit:	sep #>%00100000		; 8-bit accumulator
		lda >REG_A,s		; byte to send
		jsr xmit
		rep #>%00100000		; 16-bit accumulator
		jmp sysok

	; KEY: return next console input in A

sys_key:	sep #>%00100000		; 8-bit accumulator
1:		jsr recv		; get input
		bcs >1b			; loop if nothing
		rep #>%00100000		; 16-bit accumulator
		sta >REG_A,s
		jmp sysok

