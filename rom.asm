; ***************************************************************************
; 
; Configurable parameters

MHZ=5000000			; 5MHz clock speed
BAUD=9600			; serial baud rate

TEXTFG=$FF      		; text foreground
TEXTBG=$00      		; text background
CURSOR=$E0      		; cursor background

; ***************************************************************************
;
; RAM storage

; direct page

		org $0

scratch:	bss 3			; general scratch use
irq_d8save:	bss 1			; IRQ saved D8 (VIA A) state

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

	; display

dispmode:	bss 1			; 0 = text, 1 = graphics

	; TEXT module

text_curs_x:	bss 1           ; cursor position
text_curs_y:	bss 1           ; do not separate text_curs_x, text_curs_y
text_map_y:	bss 1           ; map offset
text_blink:	bss 1           ; cursor state, do not split from map_y

	; VBLANK stuff

frames:		bss 2		; frame counter

	; keyboard stuff

kbd_shift:	bss 1		; shift state
kbd_state:	bss 1		; keyboard state


; low RAM storage

		org $100

stack:		bss 256			; stack's traditional location
S0=.-1					; initial S value
recvbuf:	bss 256			; RS-232 buffer; must be page-aligned


; ***************************************************************************

		org $60000

	; Reset vector.
	
	; Copy ROM to RAM, set up native mode with 16-bit 
	; indexes and an 8-bit accumulator, which is our
	; default/assumed state.

reset:		clc			; enter native mode
		xce
		rep #>%00110000 	; 16-bit registers

		lda #$ffff		; copy ROM to RAM
		ldy #0
		ldx #0	
		mvn $0707		; top 64K
		mvn $0606		; bottom 64K

		; we repeat the copy to get the vectors/etc.
		; into the lower 64K 

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

	; clear frame counter and enable frame interrupts

		stz >frames
		lda #>IRQ_CA2+IRQ_ENABLE
		sta <VIA_IER

	; Now, enable interrupts and wait for the frame counter
	; to start counting, which means the AVC is loaded and
	; ready.

		cli

1:		lda >frames
		cmp #>5
		bne >1b

	; ok, AVC is ready, initialize text mode

		jsr <text
		lda #>$20
		sta >text_blink

; receive xmodem file

1:		jsr xmdrain

		stz >xmaddr		; target = $70000
		stz >xmaddr+1		
		lda #>$07
		sta >xmaddr+2
		jsr xmodem_receive
		bcs >1b			; did not work

		jsr xmdrain		; wait a bit

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

xmit:		sta >scratch
		lda #>8
		sta >scratch+1

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
		ror >scratch		; next bit -> carry
		bcc >2f
		ora #>B_XMIT		; nope, 1 bit
2:		sta <VIA_B		; send bit
		dec >scratch+1		; one less bit to send
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

; ***************************************************************************
;
; TEXT module
;
; Handles management of the screen in 40x25 "text" mode.
;
; 	TEXT: reset system to text mode
; 	CLS: clear screen and home cursor 
;	CHROUT: write char in A to screen (TTY-style)
;

text:		phb
		phy
		phx
		pha
		php

		stz >dispmode		; text mode

		jsr <cls

		; set D8 off

		lda <VIA_A	
		and #>~A_D8
		sta <VIA_A

		rep #>%00100000         ; 16-bit
		
		; shut off all the sprites

		lda #0
1:		tax
		sta <AVC_BLOCKS,x	; sprite ON = { 0 }
		clc
		adc #256		; next sprite
		cmp #$4000		; done?
		bne >1b

		; set up palette for text tiles

		lda #TEXTBG+TEXTFG*256
		sta <AVC_PALETTE
		sta <AVC_PALETTE+256
		sta <AVC_PALETTE+512
		sta <AVC_PALETTE+768
		
		; load cursor sprite (sprite 0)

		ldx #0			; clear pixels 
		lda #0
1:		sta <AVC_SPRITES,x
		inx
		inx	
		cpx #64
		bne >1b

		sep #>%00100000         ; 8-bit 

		ldx #0			; 9 rows visible
		lda #>$ff
1:		sta <AVC_BLOCKS+16,x
		inx
		cpx #9
		bne >1b

		lda #>$0
1:		sta <AVC_BLOCKS+16,x
		inx
		cpx #16
		bne >1b

		lda #>CURSOR		; sprite 0 color 0 is cursor color
		sta <AVC_PALETTE+1024

		; load font into tiles

		lda #>text_font/65536	; DBR = text_font's bank
		pha
		plb

		ldx #0
 		ldy #0
2:		lda text_font,y
		sta >scratch
		lda #>8
3:		pha
		lda <VIA_A		; set D8 on foreground
		ora #>A_D8
		sta <VIA_A
		lda #>1			; text foreground color
		rol >scratch
		bcs >4f
		lda <VIA_A		; clear D8 on background
		and #>~A_D8
		sta <VIA_A		
		lda #>0			; text background color
4:		sta <AVC_TILES,x
		inx
		pla
		dec
		bne >3b
		iny
		cpy #text_fontsz
		bne >2b

		plp
		pla
		plx
		ply
		plb
		rtl
	
	;
	; CLS
	;

cls:		pha
		phx
		php

		; fill map with $20 (space) tiles

		rep #>%00100000         ; 16-bit
		ldx #0
		lda #$2020		; spaces
1:		sta <AVC_MAP,x
		inx
		inx
		cpx #2048		; map is 2K
		bne >1b

		; reset window and cursor state

		lda #0
		sta >text_curs_x	; also clears text_curs_y !
		sta >text_map_y		; also clears text_blink !
		sta <AVC_MAP_X		; also clears AVC_MAP_Y !
					; remember D8=0 from above

		plp
		plx
		pla
		rtl

	;
	; CHROUT
	;

chrout:		pha
		phy

		; CHR$(8)

		cmp #>$08	
		bne >1f			
		dec >text_curs_x	; move cursor left
		bpl >9f
		lda #>39		; wrap to previous line
		sta >text_curs_x
		dec >text_curs_y	
		bpl >9f			; if still on screen, done

		stz >text_curs_x	; too far, just home cursor
		stz >text_curs_y		
		bra >9f			; and that's it.

1:		; CHR$(13)

		cmp #>$0d
		bne >1f
		stz >text_curs_x	; carriage return
		inc >text_curs_y	; line feed
		bra >7f			; check scroll

1:		; "non-control" characters

		jsr text_addr		; poke into screen mem
		sta [>scratch]
		inc >text_curs_x	; cursor right
		lda >text_curs_x
		cmp #>40		; end of line?
		bne >9f			; nope, we're done
		stz >text_curs_x	; carriage return
		inc >text_curs_y	; linefeed

7:		; check for scroll

		; XXX - this assumes that scrolling 
		; only occurs due to line-wrap or CR,
		; thus text_curs_x is zero.
			
		; if the TTY gets more sophisticated
		; remember to fix that
		
		lda >text_curs_y
		cmp #>25		; end of screen?
		bne >9f			; nope, done
	
		dec >text_curs_y	; fix up cursor Y
		inc >text_map_y		; rotate vertical window
		lda >text_map_y
		asl
		asl
		asl
		sta <AVC_MAP_Y

		ldy #0			; wipe bottom line
		lda #>$20
		jsr text_addr
8:		sta [>scratch],y	
		iny
		cpy #40			 
		bne >8b

9:		ply
		pla
		rtl

	; compute the tile map address of
	; the cursor position to scratch

text_addr:	pha
		php

		lda #>AVC_MAP/65536	; screen bank
		sta >scratch+2

		; scratch = 
		; AVC_MAP + 64((text_curs_y + text_map_y) % 32) + text_curs_x

		lda >text_curs_y	
		clc
		adc >text_map_y		; A = text_curs_y + text_map_y

		rep #>%00100000         ; 16-bit accumulator

		and #$001f		; A %= 32

		asl			; A *= 64
		asl
		asl
		asl
		asl
		asl

		clc
		adc #AVC_MAP

		sta >scratch		; add in X offset
		lda >text_curs_x
		and #$00ff
		adc >scratch
		sta >scratch

		plp
		pla
		rts

text_font:	bss 32*8

		byte $00,$00,$00,$00,$00,$00,$00,$00 ; (32)  
		byte $00,$10,$10,$10,$10,$10,$00,$10 ; (33) !
		byte $00,$28,$28,$28,$00,$00,$00,$00 ; (34) "
		byte $00,$28,$28,$7c,$28,$7c,$28,$28 ; (35) #
		byte $00,$10,$3c,$50,$38,$14,$78,$10 ; (36) $
		byte $00,$60,$64,$08,$10,$20,$4c,$0c ; (37) %
		byte $00,$30,$48,$50,$20,$54,$48,$34 ; (38) &
		byte $00,$30,$10,$20,$00,$00,$00,$00 ; (39) '
		byte $00,$08,$10,$20,$20,$20,$10,$08 ; (40) (
		byte $00,$20,$10,$08,$08,$08,$10,$20 ; (41) )
		byte $00,$00,$28,$10,$7c,$10,$28,$00 ; (42) *
		byte $00,$00,$10,$10,$7c,$10,$10,$00 ; (43) +
		byte $00,$00,$00,$00,$00,$30,$10,$20 ; (44) ,
		byte $00,$00,$00,$00,$7c,$00,$00,$00 ; (45) -
		byte $00,$00,$00,$00,$00,$00,$30,$30 ; (46) .
		byte $00,$00,$04,$08,$10,$20,$40,$00 ; (47) /
		byte $00,$38,$44,$4c,$54,$64,$44,$38 ; (48) 0
		byte $00,$10,$30,$10,$10,$10,$10,$38 ; (49) 1
		byte $00,$38,$44,$04,$08,$10,$20,$7c ; (50) 2
		byte $00,$7c,$08,$10,$08,$04,$44,$38 ; (51) 3
		byte $00,$08,$18,$28,$48,$7c,$08,$08 ; (52) 4
		byte $00,$7c,$40,$78,$04,$04,$44,$38 ; (53) 5
		byte $00,$18,$20,$40,$78,$44,$44,$38 ; (54) 6
		byte $00,$7c,$04,$08,$10,$20,$20,$20 ; (55) 7
		byte $00,$38,$44,$44,$38,$44,$44,$38 ; (56) 8
		byte $00,$38,$44,$44,$3c,$04,$08,$30 ; (57) 9
		byte $00,$00,$30,$30,$00,$30,$30,$00 ; (58) :
		byte $00,$00,$30,$30,$00,$30,$10,$20 ; (59) ;
		byte $00,$04,$08,$10,$20,$10,$08,$04 ; (60) <
		byte $00,$00,$00,$7c,$00,$7c,$00,$00 ; (61) =
		byte $00,$40,$20,$10,$08,$10,$20,$40 ; (62) >
		byte $00,$38,$44,$04,$08,$10,$00,$10 ; (63) ?
		byte $00,$38,$44,$04,$34,$54,$54,$38 ; (64) @
		byte $00,$38,$44,$44,$44,$7c,$44,$44 ; (65) A
		byte $00,$78,$44,$44,$78,$44,$44,$78 ; (66) B
		byte $00,$38,$44,$40,$40,$40,$44,$38 ; (67) C
		byte $00,$70,$48,$44,$44,$44,$48,$70 ; (68) D
		byte $00,$7c,$40,$40,$78,$40,$40,$7c ; (69) E
		byte $00,$7c,$40,$40,$70,$40,$40,$40 ; (70) F
		byte $00,$38,$44,$40,$40,$4c,$44,$38 ; (71) G
		byte $00,$44,$44,$44,$7c,$44,$44,$44 ; (72) H
		byte $00,$38,$10,$10,$10,$10,$10,$38 ; (73) I
		byte $00,$1c,$08,$08,$08,$08,$48,$30 ; (74) J
		byte $00,$44,$48,$50,$60,$50,$48,$44 ; (75) K
		byte $00,$40,$40,$40,$40,$40,$40,$7c ; (76) L
		byte $00,$44,$6c,$54,$44,$44,$44,$44 ; (77) M
		byte $00,$44,$44,$64,$54,$4c,$44,$44 ; (78) N
		byte $00,$38,$44,$44,$44,$44,$44,$38 ; (79) O
		byte $00,$78,$44,$44,$78,$40,$40,$40 ; (80) P
		byte $00,$38,$44,$44,$44,$54,$48,$34 ; (81) Q
		byte $00,$78,$44,$44,$78,$50,$48,$44 ; (82) R
		byte $00,$3c,$40,$40,$38,$04,$04,$78 ; (83) S
		byte $00,$7c,$10,$10,$10,$10,$10,$10 ; (84) T
		byte $00,$44,$44,$44,$44,$44,$44,$38 ; (85) U
		byte $00,$44,$44,$44,$44,$44,$28,$10 ; (86) V
		byte $00,$44,$44,$44,$54,$54,$6c,$44 ; (87) W
		byte $00,$44,$44,$28,$10,$28,$44,$44 ; (88) X
		byte $00,$44,$44,$28,$10,$10,$10,$10 ; (89) Y
		byte $00,$7c,$04,$08,$10,$20,$40,$7c ; (90) Z
		byte $00,$1c,$10,$10,$10,$10,$10,$1c ; (91) [
		byte $00,$00,$40,$20,$10,$08,$04,$00 ; (92) \
		byte $00,$70,$10,$10,$10,$10,$10,$70 ; (93) ]
		byte $00,$10,$28,$44,$00,$00,$00,$00 ; (94) ^
		byte $00,$00,$00,$00,$00,$00,$00,$7c ; (95) _
		byte $00,$20,$10,$08,$00,$00,$00,$00 ; (96) `
		byte $00,$00,$00,$38,$04,$3c,$44,$3c ; (97) a
		byte $00,$40,$40,$58,$64,$44,$44,$78 ; (98) b
		byte $00,$00,$00,$38,$40,$40,$44,$38 ; (99) c
		byte $00,$04,$04,$34,$4c,$44,$44,$3c ; (100) d
		byte $00,$00,$00,$38,$44,$7c,$40,$38 ; (101) e
		byte $00,$18,$24,$20,$70,$20,$20,$20 ; (102) f
		byte $00,$00,$00,$3c,$44,$3c,$04,$18 ; (103) g
		byte $00,$40,$40,$58,$64,$44,$44,$44 ; (104) h
		byte $00,$10,$00,$30,$10,$10,$10,$38 ; (105) i
		byte $00,$08,$00,$18,$08,$08,$48,$30 ; (106) j
		byte $00,$20,$20,$24,$28,$30,$28,$24 ; (107) k
		byte $00,$30,$10,$10,$10,$10,$10,$38 ; (108) l
		byte $00,$00,$00,$68,$54,$54,$44,$44 ; (109) m
		byte $00,$00,$00,$58,$64,$44,$44,$44 ; (110) n
		byte $00,$00,$00,$38,$44,$44,$44,$38 ; (111) o
		byte $00,$00,$00,$78,$44,$78,$40,$40 ; (112) p
		byte $00,$00,$00,$34,$4c,$3c,$04,$04 ; (113) q
		byte $00,$00,$00,$58,$64,$40,$40,$40 ; (114) r
		byte $00,$00,$00,$38,$40,$38,$04,$78 ; (115) s
		byte $00,$20,$20,$70,$20,$20,$24,$18 ; (116) t
		byte $00,$00,$00,$44,$44,$44,$4c,$34 ; (117) u
		byte $00,$00,$00,$44,$44,$44,$28,$10 ; (118) v
		byte $00,$00,$00,$44,$44,$54,$54,$28 ; (119) w
		byte $00,$00,$00,$44,$28,$10,$28,$44 ; (120) x
		byte $00,$00,$00,$44,$44,$3c,$04,$38 ; (121) y
		byte $00,$00,$00,$7c,$08,$10,$20,$7c ; (122) z
		byte $00,$08,$10,$10,$20,$10,$10,$08 ; (123) {
		byte $00,$10,$10,$10,$10,$10,$10,$10 ; (124) |
		byte $00,$20,$10,$10,$08,$10,$10,$20 ; (125) }
		byte $00,$00,$10,$08,$7c,$08,$10,$00 ; (126) ~
		byte $00,$00,$10,$20,$7c,$20,$10,$00 ; (127) 

text_fontsz=.-text_font

; ***************************************************************************
;
; KEYB module
;
; Handles keyboard input.
;
; 	INKEY: return next key read from keyboard in A
;

KBD_LCTRL=$80		; kbd_shift
KBD_RCTRL=$40
KBD_LSHFT=$20
KBD_RSHFT=$10
KBD_LALT=$08
KBD_RALT=$04
KBD_CAPSLOCK=$02
KBD_NUMLOCK=$01

KBD_BREAK=$80		; kbd_state
KBD_E0=$40		
KBD_E1=$20

	; INKEY

inkey:		phx			; save regs
		phb		

		lda #>kbd_map/65536	; proper data bank
		pha
		plb

inkey0:		stz >kbd_state	

2:		jsr kbd_scan		; wait for scan code
		bcs >2b

		; process state changes
		
		cmp #>$f0		; break code
		bne >3f
		lda #>KBD_BREAK
		bra >4f

3:		cmp #>$e0		; extended 0
		bne >3f
		lda #>KBD_E0
		bra >4f

3:		cmp #>$e1		; extended 1
		bne >5f
		lda #>KBD_E1
4:		ora >kbd_state
		sta >kbd_state
		bra >2b

		; not a state change, we have a key code

5:		pha			; stack scan code

		; ignore all codes with an $E1 prefix

		lda >kbd_state
		bit #>KBD_E1		
		beq >2f			
		
		pla			; discard
		bra >inkey0		; restart

		; first, look for "interesting" break codes

2:		bit #>KBD_BREAK
		beq >4f

		ldx #kbd_shiftkey
		bit #>KBD_E0
		beq >2f
		ldx #kbd_shiftkey0
2: 		pla			; retrieve scan code
		jsr kbd_xlat
		bcs >inkey0		; carry set means no match
		eor #>$ff		; bit -> mask
		and >kbd_shift		; mask off bit
		sta >kbd_shift
		bra >inkey0
		
		; make codes come here

		; first look for shift keys

4:		ldx #kbd_shiftkey
		bit #>KBD_E0
		beq >4f
		ldx #kbd_shiftkey0
4:		pla			; scan code
		jsr kbd_xlat
		bcs >5f			; no match, not a shift key
		ora >kbd_shift		; change shift status
		sta >kbd_shift
		bra >inkey0

		; not a shift key, look for lock keys

5:		cmp #>$58		; CAPS LOCK?
		bne >5f
		lda #>KBD_CAPSLOCK
		bra >6f
5:		cmp #>$77		; NUM LOCK?
		bne >7f			
		lda #>KBD_NUMLOCK
6:		eor >kbd_shift		; toggle
		sta >kbd_shift
		bra >inkey0

inkey1:		bra >inkey0

		; not a special key, normal processing here

7: 		pha			; save scan code again

		ldx #kbd_map
		lda >kbd_state
		bit #>KBD_E0	
		beq >2f
		ldx #kbd_map0
2:		pla
		jsr kbd_xlat
		bcs >inkey1		; ignore if no match

		; ok, we have a valid key
		; run it through the filters

2:		pha			; stack code

		; shift filter

		lda >kbd_shift
		bit #>KBD_LSHFT+KBD_RSHFT
		beq >3f
		pla
		ldx #kbd_shifted
		jsr kbd_xlat
		bcs >inkey1		; ignore if no shifted version
		pha

		; ctrl, alt, caps filters 
		; work only on alpha characters

3:		pla
		pha

		and #>$df		; force uppercase
		cmp #>$41		; A
		bcc >6f			; too low, skip these filters
		cmp #>$5b		; Z+1
		bcs >6f			; too high, skip these filters

		; ctrl filter

		lda >kbd_shift
		bit #>KBD_LCTRL+KBD_RCTRL
		beq >4f
		pla
		and #>$1f
		pha

		; alt filter

4:		lda >kbd_shift
		bit #>KBD_LALT+KBD_RALT
		beq >5f
		pla
		and #>$df
		ora #>$80
		pha

		; caps lock

5:		lda >kbd_shift
		bit #>KBD_CAPSLOCK
		beq >6f
		pla
		eor #>$20
		pha

		; num lock

6:		pla
		pha

		cmp #>$e0
		bcc >7f

		lda >kbd_shift
		bit #>KBD_NUMLOCK
		beq >7f

		pla
		eor #>$10
		pha

		; normalization

7:		pla
		ldx #kbd_normalize
		jsr kbd_xlat

9:		plb
		plx
		rtl
		
	; Entry:
	; 	X = points to translation table
	; 	A = code to translate
	;
	; Exit:
	; 	A = resulting code
	; 	carry clear if match, set if no match

kbd_xlat:	

1:		pha
		lda 0,x		; check for end of table
		bne >2f		

		; end of table
		
		pla		; return original code
		sec		; carry set = no match
		rts

		; not end of table, check value

2:		pla		; retrieve code
		cmp 0,x
		beq >3f

		; no match, next entry

		inx
		inx
		bra >1b

		; matched, return translated value

3:		lda 1,x		; translated code
		clc		; carry clear = matched
		rts

	; read keyboard scan code
	;
	; carry clear means A holds scan code
	; carry set means keyboard timeout or error
	;
	; x, scratch0-2 destroyed

kbd_scan: 	lda <VIA_DDR_A		; release clock
		and #>~A_KBCLK
		sta <VIA_DDR_A

		jsr kbd_bit		; get start bit
		bcc >2f

1:		; error exit

		lda <VIA_DDR_A		; inhibit clock
		ora #>A_KBCLK
		sta <VIA_DDR_A
		sec
		rts

2:		bne >1b			; error if start bit not 0

		; got a start bit, let's read a scan code

		stz >scratch		; clear scratch (scan code)
		stz >scratch+2		; clear parity counter
		ldx #8			; read eight bits
3:		jsr kbd_bit		; get next bit from keyboard
		bcs >1b			; carry? error exit.
		beq >4f			; if 0, go ROR with C=0
		inc >scratch+2		; bump parity counter
		sec			; it was a 1
4: 		ror >scratch
		dex			; one less bit
		bne >3b			; not done yet

		; got the scan code
		; now get the parity bit

		jsr kbd_bit		; parity bit
		bcs >1b			; carry - error
		beq >5f			; skip if zero
		inc >scratch+2		; bump parity
5:		lda >scratch+2		; check parity
		bit #>$01		; should be odd
		beq >1b			; parity error

		; stop bit 

		jsr kbd_bit		; stop bit
		bcs >1b			; carry - error
		beq >1b			; stop bit must be 1, else error

		; that's it, all 11 bits check

		lda <VIA_DDR_A		; inhibit clock
		ora #>A_KBCLK
		sta <VIA_DDR_A

		lda >scratch
		clc
		rts

	; get bit from keyboard
	; carry set on error/timeout, zero flag contains input bit
	;
	; destroys a, scratch+1

kbd_bit:	lda >frames
		inc			; two frames in the future
		inc
		sta >scratch+1		; timeout

1:		lda <VIA_A		; wait for clock HIGH
		bit #>A_KBCLK
		bne >3f			; it's high, continue
		lda >frames		; not high yet, check timeout
		cmp >scratch+1		
		bne >1b			; no timeout, loop around
2:		sec			; timeout, set carry
		rts			; and return

3:		lda <VIA_A		; wait for clock LO
		bit #>A_KBCLK		
		beq >4f			; clock is low, continue
		lda >frames		; not low yet, check timeout
		cmp >scratch+1
		bne >3b			; nope, loop
		bra >2b			; error
	
4:		lda <VIA_B		; read data
		bit #>B_KBDATA		; set Z flag based on data
		clc
		rts

kbd_shiftkey:	byte $12,KBD_LSHFT
		byte $14,KBD_LCTRL
		byte $11,KBD_LALT
		byte $59,KBD_RSHFT
		byte 0

kbd_shiftkey0:	byte $14,KBD_RCTRL
		byte $11,KBD_RALT
		byte 0

kbd_shifted:	byte $61,$41,$62,$42,$63,$43,$64,$44,$65,$45	; A-E
		byte $66,$46,$67,$47,$68,$48,$69,$49,$6A,$4A	; F-J
		byte $6B,$4B,$6C,$4C,$6D,$4D,$6E,$4E,$6F,$4F	; K-O
		byte $70,$50,$71,$51,$72,$52,$73,$53,$74,$54	; P-T
		byte $75,$55,$76,$56,$77,$57,$78,$58,$79,$59	; U-Y
		byte $7A,$5A					; Z
		
		byte $30,$29,$31,$21,$32,$40,$33,$23,$34,$24	; ) ! @ # $
		byte $35,$25,$36,$5e,$37,$26,$38,$2a,$39,$28	; % ^ & * (

		byte $60,$7e,$2d,$5f,$3d,$2b,$5b,$7b,$5d,$7d	; ~ _ + { }
		byte $5c,$7c,$20,$20				; | SPC 
		
		byte $3b,$3a,$27,$22				; : "
		byte $2c,$3c,$2e,$3e,$2f,$3f			; < > ?

		byte $e0,$f0,$e1,$f1,$e2,$f2,$e3,$f3,$e4,$f4	; keypad pseudocodes
		byte $e5,$f5,$e6,$f6,$e7,$f7,$e8,$f8,$e9,$f9	
		byte $ea,$fa,$eb,$fb,$ec,$fc,$ed,$fd,$ee,$fe	

		byte 0

kbd_normalize:	byte $f0,$30,$f1,$31,$f2,$32,$f3,$33,$f4,$34	; 0-4
		byte $f5,$35,$f6,$36,$f7,$37,$f8,$38,$f9,$39	; 5-9
		byte $fa,$2f,$fb,$2a,$fc,$2d,$fd,$2b,$fe,$2e	; / * - + .
		byte $ea,$2f,$eb,$2a,$ec,$2d,$ed,$2b,$e5,$35	; / * - + 5

		byte $e0,$b4,$e1,$b7,$e2,$b1,$e3,$b9,$e4,$b2
		byte $e6,$b3,$e7,$b6,$e8,$b0,$e9,$b8,$ee,$7f

		byte 0

kbd_map:	byte $1c,$61,$32,$62,$21,$63,$23,$64,$24,$65	; a-e
		byte $2b,$66,$34,$67,$33,$68,$43,$69,$3b,$6a	; f-j
		byte $42,$6b,$4b,$6c,$3a,$6d,$31,$6e,$44,$6f	; k-o
		byte $4d,$70,$15,$71,$2d,$72,$1b,$73,$2c,$74	; p-t
		byte $3c,$75,$2a,$76,$1d,$77,$22,$78,$35,$79	; u-y
		byte $1a,$7a					; z 

		byte $45,$30,$16,$31,$1e,$32,$26,$33,$25,$34	; 0-4
		byte $2e,$35,$36,$36,$3d,$37,$3e,$38,$46,$39	; 5-9

		byte $76,$1b,$05,$a0,$06,$a1,$04,$a2,$0c,$a3	; ESC F1..4
		byte $03,$a4,$0b,$a5,$83,$a6,$0a,$a7,$01,$a8	; F5..F9
		byte $09,$a9,$78,$aa,$07,$ab			; F10..F12

		byte $0e,$60,$4e,$2d,$55,$3d,$54,$5b,$5b,$5d	; ` - = [ ]
		byte $66,$08,$0d,$09,$5d,$5c,$29,$20		; BS TAB \ SPC

		byte $4c,$3b,$52,$27,$5a,$0d			; ; ' ENT
		byte $41,$2c,$49,$2e,$4a,$2f			; , . /

		byte $70,$e0,$69,$e1,$72,$e2,$7a,$e3		; KP: 0-3
		byte $6b,$e4,$73,$e5,$74,$e6,$6c,$e7		; KP: 4-7
		byte $75,$e8,$7d,$e9				; KP: 8-9
		byte $7c,$eb,$7b,$ec,$79,$ed,$71,$ee		; KP: * - + .

		byte 0

kbd_map0:	byte $5a,$0d,$4a,$ea			; KP: ENT /
		byte $75,$b0,$72,$b1,$6b,$b2,$74,$b3	; arrows U D L R
		byte $70,$b4,$71,$7f,$6c,$b6,$69,$b7	; INS DEL HOME END
		byte $7d,$b8,$7a,$b9			; PGUP PGDN

		byte 0


; ***************************************************************************
; 
; IRQ handler
;

IRQ_LATENCY=50				; IRQ latency - see below
STABLE_DELAY=BIT_DELAY-IRQ_LATENCY	; sample delay after timeout
TRANS_DELAY=BIT_DELAY+BIT_DELAY/2	; sample delay after transition

irq: 		sep #>%00100000		; 8-bit accumulator
		pha

		lda <VIA_IFR		; identify source

		bit #>IRQ_CB2		; CB2 transition (RX)?
		bne >7f			; yes, go

		bit #>IRQ_CA2		; CA2 transition (VBLANK)?
		beq >6f			; nope, go around

	; VBLANK

		lda #>IRQ_CA2		; acknowledge interrupt
		sta <VIA_IFR

		inc >frames		; bump frame counter
		bne >1f
		inc >frames+1

1:		lda >dispmode		; are we in text mode?
		bne >5f			; nope, don't bother w/ cursor

		; text mode: update cursor position & blink

2:		lda <VIA_A		; save interrupted D[8] state
		sta >irq_d8save
		jsr irq_d8off

		lda >frames		; check frames counter
		and >text_blink		; against blink bits
		beq >3f			; if 0, cursor is off
		jsr irq_d8on

3:		lda >text_curs_y	; update Y position + cursor
		asl			
		asl
		asl
		sta <AVC_BLOCKS+1	; sprite 0: Y_OFS + VISIBLE

		jsr irq_d8off		; update X position
		lda >text_curs_x	
		asl
		asl
		asl
		bcc >4f
		jsr irq_d8on
	
4:		sta <AVC_BLOCKS		; sprite 0: X_OFS[8:0]

		lda >irq_d8save		; restore D8
		sta <VIA_A

5:		pla			; and we're done
		rti


	; RS232 - timer 1 expired

6:		lda #>STABLE_DELAY	; set Timer 1 for 1 bit time
		sta <VIA_T1CL		; (this also clears the interrupt)
		lda #>STABLE_DELAY/$100
		sta <VIA_T1CH				
		bra >8f			; IRQ_LATENCY measures to this point!
					; N.B. don't forget 4~ bounce vector

	; RS232 - CB2 transition, bit state changed.

7:		lda #>TRANS_DELAY	; set Timer 1 for 1.5 bit times
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

8:		inc >recvcnt		; bump state machine
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

irq_d8off:	pha
		lda >irq_d8save
		and #>~A_D8
		sta <VIA_A
		pla
		rts

irq_d8on:	pha
		lda >irq_d8save
		ora #>A_D8
		sta <VIA_A
		pla
		rts
		

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


; ***************************************************************************

		org $70000

		lda #>$44
		jsr <chrout

		stz >kbd_shift

1:		jsr <inkey
		jsr <chrout
		bra >1b

