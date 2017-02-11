TEXTFG=$FF      		; text foreground
TEXTBG=$00      		; text background
CURSOR=$1C			; cursor color

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
text_curs_x:	bss 1           ; cursor position
text_curs_y:    bss 1           ; do not separate text_curs_x, text_curs_y
text_map_y:	bss 1		; map offset
text_state:	bss 1		; cursor state, do not split from map_y

	; KEYB module

keyb_scan:	bss 1		; keyboard scan code

	; timer shit

ticks:		bss 2


; ***************************************************************************

		org $70000

		jsr <text

		jsr text_addr
		lda #>$41
		sta [>text_scratch]
		ldy #1
		sta [>text_scratch],y
		ldy #64
		sta [>text_scratch],y

		lda #>IRQ_ENABLE+IRQ_CA2
		sta <VIA_IER

		cli

		lda #>$90
		sta >text_state
1:		bra >1b

; ***************************************************************************
;
; TEXT module
;
; Handles management of the screen in 40x25 "text" mode.
;
; Public entry points, assume 8 bit accumulator
;
; 	TEXT: reset system to text mode
; 	CLS: clear screen and home cursor 
;

text:		phb
		phy
		phx
		pha
		php

		jsr <cls

		; simplest (and slow) way to set up palette

		lda <VIA_A		; clear low bit
		and #>~A_D8
		sta <VIA_A

		rep #>%00100000         ; 16-bit

		ldx #0			; tile colors are text
		lda #TEXTBG+TEXTFG*256
1:		sta <AVC_PALETTE,x
		inx
		inx
		cpx #1024
		bne >1b
		
		lda #CURSOR*256		; sprite colors are cursor
1:		sta <AVC_PALETTE,x
		inx
		inx
		cpx #2048
		bne >1b

		; shut off all the sprites

		lda #0
1:		tax
		sta <AVC_BLOCKS,x	; sprite ON = { 0 }
		clc
		adc #256		; next sprite
		cmp #$4000		; done?
		bne >1b

		; load cursor sprite (sprite 0)

		ldx #text_cursor	; pixels
		ldy #AVC_SPRITES
		lda #63
		mvn text_cursor/65536*256+AVC_SPRITES/65536

		ldy #AVC_BLOCKS+16	; visibility
		lda #15
		mvn text_cursor/65536*256+AVC_BLOCKS/65536

		; load font into tiles

		sep #>%00100000         ; 8-bit 

		lda #>text_font/65536	; DBR = text_font's bank
		pha
		plb

		ldx #0
 		ldy #0
2:		lda text_font,y
		sta >text_scratch
		lda #>8
3:		pha
		lda <VIA_A		; set D8 on foreground
		ora #>A_D8
		sta <VIA_A
		lda #>1			; text foreground color
		rol >text_scratch
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
		sta >text_map_y		; also clears text_state !
		sta <AVC_MAP_X		; also clears AVC_MAP_Y !

		; (call text_updcurs to fix sprite)

		plp
		plx
		pla
		rtl

	; compute the tile map address of
	; the cursor position to text_scratch

text_addr:	pha
		php

		lda #>AVC_MAP/65536	; screen bank
		sta >text_scratch+2

		; text_scratch = 
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

1:		sta >text_scratch		; add in X offset
		lda >text_curs_x
		and #$00ff
		adc >text_scratch
		sta >text_scratch

		plp
		pla
		rts

text_cursor:	byte $55,$55,$55,$55,$55,$55,$55,$55	; cursor sprite
		byte $55,$55,$55,$55,$55,$55,$55,$55	
		byte $55,$55,$55,$55,$55,$55,$55,$55	
		byte $55,$55,$55,$55,$55,$55,$55,$55	
		byte $55,$55,$55,$55,$00,$00,$00,$00	
		byte $00,$00,$00,$00,$00,$00,$00,$00
		byte $00,$00,$00,$00,$00,$00,$00,$00
		byte $00,$00,$00,$00,$00,$00,$00,$00

		byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	; visible
		byte $ff,$00,$00,$00,$00,$00,$00,$00

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

