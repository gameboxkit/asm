; ***************************************************************************
; 
; Configurable parameters

MHZ=5000000			; 5MHz clock speed
BAUD=9600			; serial baud rate

TEXTFG=$FF      		; text foreground
TEXTBG=$00      		; text background
CURSBG=$E0      		; cursor background

; ***************************************************************************
;
; RAM storage

; direct page

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

xmaddr:		bss 3			; far pointer to buffer
xmblkno:	bss 1			; expected block #
xmretry:	bss 1			; retry counter
xmctl:		bss 1			; either ACK or NAK

	; CRC-16 routines

crc:		bss 2			; 16-bit CRC accumulator

	; TEXT module

text_scratch:   bss 3           ; scratch locations for text module
text_curs_x:     bss 1           ; cursor position
text_curs_y:     bss 1           ; do not separate text_curs_x, text_curs_y
text_map_y:     bss 1           ; map offset
text_state:     bss 1           ; cursor state, do not split from map_y

	; KEYB module

keyb_scan:	bss 1		; keyboard scan code

	; VBLANK stuff

ticks:		bss 2		; blank in progress


; low RAM storage

		org $100

stack:		bss 256			; stack's traditional location
S0=.-1					; initial S value
recvbuf:	bss 256			; RS-232 buffer; must be page-aligned

; high RAM storage

                org $5fc00

text_screen:    bss 40*25       ; shadow screen buffer
text_space:     bss 1           ; keep $20 here to speed scroll up

                bss 23          ; free for use!

; ***************************************************************************

		org $60000

	; (temporary) redirection vectors

		jmp text
		nop

		jmp cls
		nop

		jmp chrout	
		nop

		jmp hidecurs
		nop

		jmp showcurs
		nop


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
		mvn $0707		; top 64K
		mvn $0606		; bottom 64K

		; we repeat the copy to get the vectors/etc.
		; into the lower 64K and set the DBR properly.

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
		lda #>$26		; independent, CB2: neg, CA2: pos
		sta <VIA_PCR		; triggers on CA/B 1/2

		lda #>$7f		; disable all interrupts
		sta <VIA_IER		
		sta <VIA_IFR		; and clear any pending garbage

	; Initialize the RS-232 "port".

		jsr recvflush
		jsr recvidle		; initialize RS-232 to idle

	; Initialize display.

		jsr <text
		jsr <cls

		lda #>$47		; "GO"
		jsr <chrout
		lda #>$4f
		jsr <chrout
		lda #>$0d
		jsr <chrout

		jsr <showcurs

	; Done.  Enable interrupts.

		cli

; receive xmodem file

1:		jsr xmdrain

		stz >xmaddr		; target = $70000
		stz >xmaddr+1		
		lda #>$07
		sta >xmaddr+2
		jsr xmodem_receive
		bcs >1b			; did not work

	; just echo the file back out

		jsr xmdrain		; wait a bit
		jsr xmdrain
		jsr xmdrain

	; and jump to that code

		sei			; disable interrupts
		jmp <$70000		; jump to new code

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
		sta [>xmaddr],y		; write byte to buffer
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

updcrc:		phx

		xba		; need high byte to be 0
		lda #>0
		xba

		eor >crc+1 	
       		tax
       		lda >crc	
       		eor <crchi,X
       		sta >crc+1
      	 	lda <crclo,X
       		sta >crc
		plx
       		rts

IRQ_LATENCY=50				; IRQ latency - see below
STABLE_DELAY=BIT_DELAY-IRQ_LATENCY	; sample delay after timeout
TRANS_DELAY=BIT_DELAY+BIT_DELAY/2	; sample delay after transition

irq: 		sep #>%00100000		; 8-bit accumulator
		pha

		lda <VIA_IFR		; identify source

		bit #>IRQ_CB2		; CB2 transition (RX)?
		bne >6f			; yes, go

		bit #>IRQ_CA2		; CA2 transition (VBLANK)?
		beq >5f			; nope, go around

	; VBLANK

		lda #>IRQ_CA2		; acknowledge interrupt
		sta <VIA_IFR

		inc >ticks		; bump tick counter
		bne >1f
		inc >ticks+1

1:		lda >text_state		; are we in text mode?
		bmi >2f			; yes, go deal with cursor
		pla			; nope, we're done
		rti

		; blink cursor

2:		lda <VIA_A		; save interrupted D[8] state
		pha 

		and #>~A_D8		; default: not visible
		pha			
		
		lda >ticks		; check ticks against
		and #>$7f		; cursor blink bits
		and >text_state
		beq >3f			; if 0, cursor is off

		pla
		ora #>A_D8		; cursor on
		pha

3:		pla
		sta <VIA_A

		lda >text_curs_y
		asl			
		asl
		asl
		sta <AVC_BLOCKS+1	; sprite 0: Y_OFS + VISIBLE

		pla			; restore previous D[8]
		sta <VIA_A

		pla			; and NOW we're done
		rti


	; RS232 - timer 1 expired

5:		lda #>STABLE_DELAY	; set Timer 1 for 1 bit time
		sta <VIA_T1CL		; (this also clears the interrupt)
		lda #>STABLE_DELAY/$100
		sta <VIA_T1CH				
		bra >7f			; IRQ_LATENCY measures to this point!
					; N.B. don't forget 4~ bounce vector

	; RS232 - CB2 transition, bit state changed.

6:		lda #>TRANS_DELAY	; set Timer 1 for 1.5 bit times
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

7:		inc >recvcnt		; bump state machine
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
; TEXT module
;
; Handles management of the screen in 40x25 "text" mode.
;
; Public entry points 
;
; 	TEXT: reset system to text mode
; 	CLS: clear screen and home cursor 
; 	SHOWCURS: display cursor 
; 	HIDECURS: hide cursor 
; 	CHROUT: put character on screen (A = 7-bit character to print)
;

text:		phb
		phy
		phx
		pha
		php
		
		sep #>%00100000         ; 8-bit 

		lda #>$20
		sta <text_space

		lda #>0			
		sta <AVC_MAP_X
		sta <AVC_MAP_Y

		; set up default palette

		ldx #0			; default palette (identity mapping)
1:		sta <AVC_PALETTE,x	; 
		inc
		inx
		cpx #256*4
		bne >1b

		; load font into tiles

		lda #>text_font/65536	; DBR = text_font's bank
		pha
		plb

		lda #>TEXTBG		; load font 0-127 w/normal colors
		sta >text_scratch+1
		ldx #0
		jsr 1f
		lda #>CURSBG		; repeat to 127-255 w/cursor colors
		sta >text_scratch+1
		jsr 1f

		; sync screen with shadow buffer
		
		rep #>%00100000		; 16-bit 
		jsr text_redraw

		plp
		pla
		plx
		ply
		plb
		rtl

	; subroutine that loads bitmap font

1: 		ldy #0
2:		lda text_font,y
		sta >text_scratch
		lda #>8
3:		pha
		lda #>TEXTFG
		rol >text_scratch
		bcs >4f
		lda >text_scratch+1
4:		sta <AVC_TILES,x
		inx
		pla
		dec
		bne >3b
		iny
		cpy #text_fontsz
		bne >2b
		rts
	
	; text_redraw redraws the tile map
	; using the shadow text_screen data

	; expects 16-bit accumulator on entry;
	; trashes DBR

text_redraw:	ldx #text_screen
		ldy #AVC_MAP
1:		lda #39					; 40 columns to move
		mvn text_screen/65536*256+AVC_MAP/65536	; move!
		cpx #text_space				; end of screen?
		beq >2f					; branch if so
		tya					; bump map ptr
		clc					; to skip unused cols
		adc #24
		tay
		bra >1b
2:		rts

cls:		pha
		phb
		php
	
		lda #>$20				; pilot space
		sta <text_screen

		rep #>%00100000				; 16-bit acc
		ldx #text_screen
		ldy #text_screen+1
		lda #40*25-2
		mvn text_screen/65536*256+text_screen/65536	; fill 

		jsr text_redraw		; mirror to actual screen

		stz >text_curs_x		; home cursor (clears text_curs_y too)

		plp
		plb
		pla
		rtl

showcurs:	pha				
		php
		sep #>%00100000			; 8-bit
		lda #>$80			; cursor ON
		bra >1f

hidecurs:	pha			
		php
		sep #>%00100000			; 8-bit
		lda #>0

1:		pha				; save on/off flag
		jsr text_scrcurs		; get character at cursor
		lda [>text_scratch]
		ora >1,s			; apply cursor bit (if set)
		jsr text_mapcurs		; compute cursor map addr
		sta [>text_scratch]
		pla				; remove flag from stack

		plp
		pla
		rtl

chrout:		pha
		php
		phb

		jsr <hidecurs

		sep #>%00100000			; 8-bit

		and #>$7f			; strip high bit

		; CHR$(8)		
		
		cmp #>$08			; backspace?
		bne >1f
		dec >text_curs_x			; move back one space
		bpl >9f				; if not negative, we're done
		lda #>39			; move to end of previous line
		sta >text_curs_x
		dec >text_curs_y
		bpl >9f				; if still on screen, we're done
		stz >text_curs_x			; too far..
		stz >text_curs_y			; just home the cursor
		bra >9f				; and that's all

		; CHR$(13)

1:		cmp #>$0d			; carriage return?
		bne >1f				; nope
		stz >text_curs_x			; cursor to left
		inc >text_curs_y			; cursor down
		bra >8f				; check for scroll

		; CHR$(12)

1:		cmp #>$0c			; formfeed?
		bne >1f
		jsr <cls			; clear screen
		bra >9f

		; errbody else

1:		jsr text_scrcurs		; save in shadow RAM
		sta [>text_scratch]
		jsr text_mapcurs		; write to tile MAP
		sta [>text_scratch]

		; now update the cursor position

		inc >text_curs_x			; move right
		lda >text_curs_x		
		cmp #>40			; end of line?
		bne >9f				; nope, we're done

		stz >text_curs_x			; beginning of line
		inc >text_curs_y			; move down

		; check for scroll

8:		lda >text_curs_y
		cmp #>25			; end of screen?
		bne >9f				; nope, we're done

		; scroll up

		dec >text_curs_y			; move back up

		rep #>%00100000			; 16-bit acc
		ldx #text_screen+40		; start at second line
		ldy #text_screen		; move to first line
		lda #40*24			; 24 rows (+1 .. text_space)
		mvn text_screen/65536*256+text_screen/65536	; move

		ldx #text_space-40		; fill last line with spaces
		ldy #text_space-39
		lda #38
		mvn text_screen/65536*256+text_screen/65536	; move

		jsr text_redraw

9:		plb
		plp
		pla

		rtl

	; compute the screen (shadow RAM) address of
	; the cursor position to text_scratch 
	;
	; assumes 8-bit mode on entry
	; all registers preserved


text_scrcurs:	pha
		php

		lda #>text_screen/65536	; screen bank
		sta >text_scratch+2

		rep #>%00100000         ; 16-bit accumulator

		; text_scratch = text_screen + 40(text_curs_y)

		lda >text_curs_y
		and #$00ff		; clear out junk

		asl			; A = text_curs_y * 32
		asl
		asl
		asl
		asl
		sta >text_scratch

		lda >text_curs_y
		and #$00ff

		asl			; A = text_curs_y * 8
		asl
		asl

		clc		
		adc >text_scratch
		adc #text_screen

		bra >1f			; merge tail with text_mapcurs

	; compute the tile map address of
	; the cursor position to text_scratch

text_mapcurs:	pha
		php

		lda #>AVC_MAP/65536	; screen bank
		sta >text_scratch+2

		rep #>%00100000         ; 16-bit accumulator

		; text_scratch = AVC_MAP + 64(text_curs_y) + text_curs_x

		lda >text_curs_y
		and #$00ff		; clear out junk

		asl			; A = text_curs_y * 64
		asl
		asl
		asl
		asl
		asl

		clc
		adc #AVC_MAP

1:		sta >text_scratch		; add in X offset
		lda >text_curs_x
		and #$00ff
		adc >text_scratch
		sta >text_scratch

		plp
		pla
		rts

text_font:	bss 32*8

		byte $00,$00,$00,$00,$00,$00,$00,$00 ; (32)  
		byte $10,$10,$10,$10,$10,$00,$10,$00 ; (33) !
		byte $28,$28,$28,$00,$00,$00,$00,$00 ; (34) "
		byte $28,$28,$7c,$28,$7c,$28,$28,$00 ; (35) #
		byte $10,$3c,$50,$38,$14,$78,$10,$00 ; (36) $
		byte $60,$64,$08,$10,$20,$4c,$0c,$00 ; (37) %
		byte $30,$48,$50,$20,$54,$48,$34,$00 ; (38) &
		byte $30,$10,$20,$00,$00,$00,$00,$00 ; (39) '
		byte $08,$10,$20,$20,$20,$10,$08,$00 ; (40) (
		byte $20,$10,$08,$08,$08,$10,$20,$00 ; (41) )
		byte $00,$28,$10,$7c,$10,$28,$00,$00 ; (42) *
		byte $00,$10,$10,$7c,$10,$10,$00,$00 ; (43) +
		byte $00,$00,$00,$00,$30,$10,$20,$00 ; (44) ,
		byte $00,$00,$00,$7c,$00,$00,$00,$00 ; (45) -
		byte $00,$00,$00,$00,$00,$30,$30,$00 ; (46) .
		byte $00,$04,$08,$10,$20,$40,$00,$00 ; (47) /
		byte $38,$44,$4c,$54,$64,$44,$38,$00 ; (48) 0
		byte $10,$30,$10,$10,$10,$10,$38,$00 ; (49) 1
		byte $38,$44,$04,$08,$10,$20,$7c,$00 ; (50) 2
		byte $7c,$08,$10,$08,$04,$44,$38,$00 ; (51) 3
		byte $08,$18,$28,$48,$7c,$08,$08,$00 ; (52) 4
		byte $7c,$40,$78,$04,$04,$44,$38,$00 ; (53) 5
		byte $18,$20,$40,$78,$44,$44,$38,$00 ; (54) 6
		byte $7c,$04,$08,$10,$20,$20,$20,$00 ; (55) 7
		byte $38,$44,$44,$38,$44,$44,$38,$00 ; (56) 8
		byte $38,$44,$44,$3c,$04,$08,$30,$00 ; (57) 9
		byte $00,$30,$30,$00,$30,$30,$00,$00 ; (58) :
		byte $00,$30,$30,$00,$30,$10,$20,$00 ; (59) ;
		byte $04,$08,$10,$20,$10,$08,$04,$00 ; (60) <
		byte $00,$00,$7c,$00,$7c,$00,$00,$00 ; (61) =
		byte $40,$20,$10,$08,$10,$20,$40,$00 ; (62) >
		byte $38,$44,$04,$08,$10,$00,$10,$00 ; (63) ?
		byte $38,$44,$04,$34,$54,$54,$38,$00 ; (64) @
		byte $38,$44,$44,$44,$7c,$44,$44,$00 ; (65) A
		byte $78,$44,$44,$78,$44,$44,$78,$00 ; (66) B
		byte $38,$44,$40,$40,$40,$44,$38,$00 ; (67) C
		byte $70,$48,$44,$44,$44,$48,$70,$00 ; (68) D
		byte $7c,$40,$40,$78,$40,$40,$7c,$00 ; (69) E
		byte $7c,$40,$40,$70,$40,$40,$40,$00 ; (70) F
		byte $38,$44,$40,$40,$4c,$44,$38,$00 ; (71) G
		byte $44,$44,$44,$7c,$44,$44,$44,$00 ; (72) H
		byte $38,$10,$10,$10,$10,$10,$38,$00 ; (73) I
		byte $1c,$08,$08,$08,$08,$48,$30,$00 ; (74) J
		byte $44,$48,$50,$60,$50,$48,$44,$00 ; (75) K
		byte $40,$40,$40,$40,$40,$40,$7c,$00 ; (76) L
		byte $44,$6c,$54,$44,$44,$44,$44,$00 ; (77) M
		byte $44,$44,$64,$54,$4c,$44,$44,$00 ; (78) N
		byte $38,$44,$44,$44,$44,$44,$38,$00 ; (79) O
		byte $78,$44,$44,$78,$40,$40,$40,$00 ; (80) P
		byte $38,$44,$44,$44,$54,$48,$34,$00 ; (81) Q
		byte $78,$44,$44,$78,$50,$48,$44,$00 ; (82) R
		byte $3c,$40,$40,$38,$04,$04,$78,$00 ; (83) S
		byte $7c,$10,$10,$10,$10,$10,$10,$00 ; (84) T
		byte $44,$44,$44,$44,$44,$44,$38,$00 ; (85) U
		byte $44,$44,$44,$44,$44,$28,$10,$00 ; (86) V
		byte $44,$44,$44,$54,$54,$6c,$44,$00 ; (87) W
		byte $44,$44,$28,$10,$28,$44,$44,$00 ; (88) X
		byte $44,$44,$28,$10,$10,$10,$10,$00 ; (89) Y
		byte $7c,$04,$08,$10,$20,$40,$7c,$00 ; (90) Z
		byte $1c,$10,$10,$10,$10,$10,$1c,$00 ; (91) [
		byte $00,$40,$20,$10,$08,$04,$00,$00 ; (92) \
		byte $70,$10,$10,$10,$10,$10,$70,$00 ; (93) ]
		byte $10,$28,$44,$00,$00,$00,$00,$00 ; (94) ^
		byte $00,$00,$00,$00,$00,$00,$7c,$00 ; (95) _
		byte $20,$10,$08,$00,$00,$00,$00,$00 ; (96) `
		byte $00,$00,$38,$04,$3c,$44,$3c,$00 ; (97) a
		byte $40,$40,$58,$64,$44,$44,$78,$00 ; (98) b
		byte $00,$00,$38,$40,$40,$44,$38,$00 ; (99) c
		byte $04,$04,$34,$4c,$44,$44,$3c,$00 ; (100) d
		byte $00,$00,$38,$44,$7c,$40,$38,$00 ; (101) e
		byte $18,$24,$20,$70,$20,$20,$20,$00 ; (102) f
		byte $00,$00,$3c,$44,$3c,$04,$18,$00 ; (103) g
		byte $40,$40,$58,$64,$44,$44,$44,$00 ; (104) h
		byte $10,$00,$30,$10,$10,$10,$38,$00 ; (105) i
		byte $08,$00,$18,$08,$08,$48,$30,$00 ; (106) j
		byte $20,$20,$24,$28,$30,$28,$24,$00 ; (107) k
		byte $30,$10,$10,$10,$10,$10,$38,$00 ; (108) l
		byte $00,$00,$68,$54,$54,$44,$44,$00 ; (109) m
		byte $00,$00,$58,$64,$44,$44,$44,$00 ; (110) n
		byte $00,$00,$38,$44,$44,$44,$38,$00 ; (111) o
		byte $00,$00,$78,$44,$78,$40,$40,$00 ; (112) p
		byte $00,$00,$34,$4c,$3c,$04,$04,$00 ; (113) q
		byte $00,$00,$58,$64,$40,$40,$40,$00 ; (114) r
		byte $00,$00,$38,$40,$38,$04,$78,$00 ; (115) s
		byte $20,$20,$70,$20,$20,$24,$18,$00 ; (116) t
		byte $00,$00,$44,$44,$44,$4c,$34,$00 ; (117) u
		byte $00,$00,$44,$44,$44,$28,$10,$00 ; (118) v
		byte $00,$00,$44,$44,$54,$54,$28,$00 ; (119) w
		byte $00,$00,$44,$28,$10,$28,$44,$00 ; (120) x
		byte $00,$00,$44,$44,$3c,$04,$38,$00 ; (121) y
		byte $00,$00,$7c,$08,$10,$20,$7c,$00 ; (122) z
		byte $08,$10,$10,$20,$10,$10,$08,$00 ; (123) {
		byte $10,$10,$10,$10,$10,$10,$10,$00 ; (124) |
		byte $20,$10,$10,$08,$10,$10,$20,$00 ; (125) }
		byte $00,$10,$08,$7c,$08,$10,$00,$00 ; (126) ~
		byte $00,$10,$20,$7c,$20,$10,$00,$00 ; (127)

text_fontsz=.-text_font

; ***************************************************************************
;
; vectors
;

	; bounce

		org $6ffd0

irqvec:		jmp <irq
resvec:		jmp <reset

	; native

		org $6ffe0

		word 0			; reserved
		word 0			; reserved
		word 0			; COP
		word 0			; BRK
		word 0 			; ABORT
		word 0			; NMI
		word 0 			; reserved
		word irqvec		; IRQ

	; emulation

		org $6fffc

		word resvec		; RESET



