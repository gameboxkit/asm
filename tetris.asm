; ****************************************************************************
;

WELL_OFFSET=135			; offset to upper-left corner of the well


; ****************************************************************************
;
; direct page variables

scratch0:	bss 1

tileptr:	bss 2		; pointer to bloc data
mapptr:		bss 2		; offset into map


; other variables

		org $100


; ****************************************************************************
;
; game entry point
		
		org $400

		sei		; mask interrupts

	; set up the tile palette
	;
	; we just map the colors to their 8-bit RGB
	; values for all 4 tile sets.

		ldx #0		
		lda #>0	
1:		sta <AVC_PALETTE,x
		inc
		inx
		cpx #256*4
		bne >1b

	; load graphical tiles

		ldx #0
1:		lda tiles,x
		sta <AVC_TILES,x
		inx
		cpx #tilesz
		bne >1b

	; load font tiles

		ldx #0
		ldy #0
1:		lda font,y
		sta >scratch0
		lda #>8
2:		pha
		lda #>$FF		; white
		rol >scratch0
		bcs >3f
		lda #>0
3:		sta <AVC_TILES+8192,x
		inx
		pla
		dec
		bne >2b
		iny
		cpy #fontsz
		bne >1b

	; clear map
		
		ldx #0
		lda #>0
1:		sta <AVC_MAP,x
		inx
		cpx #64*32
		bne >1b

		jsr drawwell

		ldx #bloc_z
		stx >tileptr
		ldx #268
		stx >mapptr
		jsr drawbloc

		lda #>128
		ldx #0
1:		sta <AVC_MAP,x
		inx
		inc
		cmp #>168
		bne >1b
	
9:		bra >9b

; ****************************************************************************
;
; drawbloc
;
; Entry:
;	(>tileptr) bloc data
;	(>mapptr) map offset
;
; Exit: A,X destroyed

drawbloc:	lda #>16			; 16 tiles per bloc

1:		pha				; save tile count
2:		ldx >mapptr
		lda (>tileptr)
		beq >3f				; don't write 0 tiles
		sta <AVC_MAP,x
3:		inx				; bump 
		stx >mapptr

		ldx >tileptr			; bump
		inx
		stx >tileptr

4:		pla				; one less tile
		dec				
		beq >5f				; if done, exit!
		bit #>3				; row boundary?
		bne >1b				; nope

		pha
		clc
		lda >mapptr
		adc #>60
		sta >mapptr
		bcc >2b
		inc >mapptr+1
		bra >2b

5:		rts

; ****************************************************************************
;
; drawwell
;

drawwell:	ldx #0
		stx >mapptr

		lda #>20			; outer loop
1:		pha

		lda #>10			; inner loop
2:		pha
		ldx >mapptr
		lda #>58			; 58 = grid tile
		sta <AVC_MAP+WELL_OFFSET,x
		inx
		stx >mapptr
		pla
		dec
		bne >2b

		pla
		dec
		beq >4f

		pha
		clc
		lda >mapptr
		adc #>54
		sta >mapptr
		bcc >3f
		inc >mapptr+1

3:		pla
		bra >1b

4:		rts

; ****************************************************************************
;

font:		byte $38,$4C,$C6,$C6,$C6,$64,$38,0	; 0
		byte $18,$38,$18,$18,$18,$18,$7E,0	; 1
		byte $7C,$C6,$0E,$3C,$78,$E0,$FE,0	; 2
		byte $7E,$0C,$18,$3C,$06,$C6,$7C,0	; 3
		byte $1C,$3C,$6C,$CC,$FE,$0C,$0C,0	; 4
		byte $FC,$C0,$FC,$06,$06,$C6,$7C,0	; 5
		byte $3C,$60,$C0,$FC,$C6,$C6,$7C,0	; 6
		byte $FE,$C6,$0C,$18,$30,$30,$30,0	; 7
		byte $7C,$C6,$C6,$7C,$C6,$C6,$7C,0	; 8
		byte $7C,$C6,$C6,$7E,$06,$0C,$78,0	; 9

		byte 0,0,0,0,0,0,0,0
		byte 0,0,0,0,0,0,0,0
		byte 0,0,0,0,0,0,0,0
		byte $3C,$42,$99,$A1,$A1,$99,$42,$3C 	; (C)
		byte 0,0,0,0,0,0,0,0
		byte 0,0,0,0,0,0,0,0

		byte 0,0,0,0,0,0,0,0 ; A
		byte 0,0,0,0,0,0,0,0 ; B
		byte 0,0,0,0,0,0,0,0 ; C
		byte 0,0,0,0,0,0,0,0 ; D
		byte 0,0,0,0,0,0,0,0 ; E
		byte 0,0,0,0,0,0,0,0 ; F
		byte 0,0,0,0,0,0,0,0 ; G
		byte 0,0,0,0,0,0,0,0 ; H
		byte 0,0,0,0,0,0,0,0 ; I
		byte 0,0,0,0,0,0,0,0 ; J
		byte 0,0,0,0,0,0,0,0 ; K
		byte 0,0,0,0,0,0,0,0 ; L
		byte 0,0,0,0,0,0,0,0 ; M
		byte 0,0,0,0,0,0,0,0 ; N
		byte 0,0,0,0,0,0,0,0 ; O
		byte 0,0,0,0,0,0,0,0 ; P
		byte 0,0,0,0,0,0,0,0 ; Q
		byte 0,0,0,0,0,0,0,0 ; R
		byte 0,0,0,0,0,0,0,0 ; S
		byte 0,0,0,0,0,0,0,0 ; T
		byte 0,0,0,0,0,0,0,0 ; U
		byte 0,0,0,0,0,0,0,0 ; V
		byte 0,0,0,0,0,0,0,0 ; W
		byte 0,0,0,0,0,0,0,0 ; X
		byte 0,0,0,0,0,0,0,0 ; Y
		byte 0,0,0,0,0,0,0,0 ; Z



fontsz=.-font



; ****************************************************************************

tiles:

	; 0 is the blank tile

		byte 0,0,0,0,0,0,0,0		; 0
		byte 0,0,0,0,0,0,0,0
		byte 0,0,0,0,0,0,0,0
		byte 0,0,0,0,0,0,0,0
		byte 0,0,0,0,0,0,0,0
		byte 0,0,0,0,0,0,0,0
		byte 0,0,0,0,0,0,0,0
		byte 0,0,0,0,0,0,0,0

	; yellow tiles for O bloc

Y2=$FC
Y1=$D8
Y0=$91

		byte Y2,Y2,Y2,Y2,Y2,Y2,Y2,Y2	; 1
		byte Y2,Y2,Y2,Y2,Y2,Y2,Y2,Y2
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1

		byte Y2,Y2,Y2,Y2,Y2,Y2,Y2,Y2	; 2 
		byte Y2,Y2,Y2,Y2,Y2,Y2,Y2,Y0
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0

		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1	; 3
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1
		byte Y2,Y2,Y1,Y1,Y1,Y1,Y1,Y1
		byte Y2,Y2,Y0,Y0,Y0,Y0,Y0,Y0
		byte Y2,Y0,Y0,Y0,Y0,Y0,Y0,Y0

		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0	; 4
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0
		byte Y1,Y1,Y1,Y1,Y1,Y1,Y0,Y0
		byte Y0,Y0,Y0,Y0,Y0,Y0,Y0,Y0
		byte Y0,Y0,Y0,Y0,Y0,Y0,Y0,Y0

	; cyan tiles for I bloc

C2=$17
C1=$12
C0=$09
		byte C2,C2,C2,C2,C2,C2,C2,C2	; 5
		byte C2,C2,C2,C2,C2,C2,C2,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
	
		byte C2,C2,C1,C1,C1,C1,C0,C0	; 6
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0

		byte C2,C2,C1,C1,C1,C1,C0,C0	; 7
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C1,C1,C1,C1,C0,C0
		byte C2,C2,C0,C0,C0,C0,C0,C0
		byte C2,C0,C0,C0,C0,C0,C0,C0

		byte C2,C2,C2,C2,C2,C2,C2,C2	; 8
		byte C2,C2,C2,C2,C2,C2,C2,C2
		byte C2,C2,C1,C1,C1,C1,C1,C1
		byte C2,C2,C1,C1,C1,C1,C1,C1
		byte C2,C2,C1,C1,C1,C1,C1,C1
		byte C2,C2,C1,C1,C1,C1,C1,C1
		byte C2,C2,C0,C0,C0,C0,C0,C0
		byte C2,C0,C0,C0,C0,C0,C0,C0

		byte C2,C2,C2,C2,C2,C2,C2,C2	; 9
		byte C2,C2,C2,C2,C2,C2,C2,C2
		byte C1,C1,C1,C1,C1,C1,C1,C1
		byte C1,C1,C1,C1,C1,C1,C1,C1
		byte C1,C1,C1,C1,C1,C1,C1,C1
		byte C1,C1,C1,C1,C1,C1,C1,C1
		byte C0,C0,C0,C0,C0,C0,C0,C0
		byte C0,C0,C0,C0,C0,C0,C0,C0

		byte C2,C2,C2,C2,C2,C2,C2,C2	; 10
		byte C2,C2,C2,C2,C2,C2,C2,C0
		byte C1,C1,C1,C1,C1,C1,C0,C0
		byte C1,C1,C1,C1,C1,C1,C0,C0
		byte C1,C1,C1,C1,C1,C1,C0,C0
		byte C1,C1,C1,C1,C1,C1,C0,C0
		byte C0,C0,C0,C0,C0,C0,C0,C0
		byte C0,C0,C0,C0,C0,C0,C0,C0

	; green tiles for S bloc

G2=$1c
G1=$10
G0=$08

		byte G2,G2,G2,G2,G2,G2,G2,G2	; 11
		byte G2,G2,G2,G2,G2,G2,G2,G2
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0

		byte G2,G2,G2,G2,G2,G2,G2,G2	; 12
		byte G2,G2,G2,G2,G2,G2,G2,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G0,G0,G0,G0,G0,G0,G0,G0
		byte G0,G0,G0,G0,G0,G0,G0,G0

		byte G2,G2,G2,G2,G2,G2,G2,G2 	; 13
		byte G2,G2,G2,G2,G2,G2,G2,G2
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G0,G0,G0,G0,G0,G0
		byte G2,G0,G0,G0,G0,G0,G0,G0

		byte G2,G2,G1,G1,G1,G1,G0,G0	; 14
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G0,G0,G0,G0,G0,G0,G0,G0
		byte G0,G0,G0,G0,G0,G0,G0,G0

		byte G2,G2,G2,G2,G2,G2,G2,G2	; 15
		byte G2,G2,G2,G2,G2,G2,G2,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0

		byte G2,G2,G1,G1,G1,G1,G0,G0	; 16
		byte G2,G2,G1,G1,G1,G1,G0,G2
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G1,G1,G1,G1,G1,G1
		byte G2,G2,G0,G0,G0,G0,G0,G0
		byte G2,G0,G0,G0,G0,G0,G0,G0

		byte G2,G2,G2,G2,G2,G2,G2,G2	; 17
		byte G2,G2,G2,G2,G2,G2,G2,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G1,G1,G1,G1,G1,G1,G0,G0
		byte G0,G0,G1,G1,G1,G1,G0,G0
		byte G0,G2,G1,G1,G1,G1,G0,G0

		byte G2,G2,G1,G1,G1,G1,G0,G0	; 18
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G1,G1,G1,G1,G0,G0
		byte G2,G2,G0,G0,G0,G0,G0,G0
		byte G2,G0,G0,G0,G0,G0,G0,G0

	; blue tiles for J bloc

B2=$03
B1=$02
B0=$01

		byte B2,B2,B2,B2,B2,B2,B2,B2	; 19
		byte B2,B2,B2,B2,B2,B2,B2,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0

		byte B2,B2,B1,B1,B1,B1,B0,B0	; 20
		byte B2,B2,B1,B1,B1,B1,B0,B2
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B0,B0,B0,B0,B0,B0
		byte B2,B0,B0,B0,B0,B0,B0,B0

		byte B2,B2,B2,B2,B2,B2,B2,B2	; 21
		byte B2,B2,B2,B2,B2,B2,B2,B2
		byte B1,B1,B1,B1,B1,B1,B1,B1
		byte B1,B1,B1,B1,B1,B1,B1,B1
		byte B1,B1,B1,B1,B1,B1,B1,B1
		byte B1,B1,B1,B1,B1,B1,B1,B1
		byte B0,B0,B0,B0,B0,B0,B0,B0
		byte B0,B0,B0,B0,B0,B0,B0,B0

		byte B2,B2,B2,B2,B2,B2,B2,B2	; 22
		byte B2,B2,B2,B2,B2,B2,B2,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B0,B0,B0,B0,B0,B0,B0,B0
		byte B0,B0,B0,B0,B0,B0,B0,B0

		byte B2,B2,B2,B2,B2,B2,B2,B2	; 23
		byte B2,B2,B2,B2,B2,B2,B2,B2
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0

		byte B2,B2,B2,B2,B2,B2,B2,B2	; 24
		byte B2,B2,B2,B2,B2,B2,B2,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B0,B0,B0,B0,B0,B0,B0,B0
		byte B0,B0,B0,B0,B0,B0,B0,B0

		byte B2,B2,B1,B1,B1,B1,B0,B0	; 25
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		
		byte B2,B2,B1,B1,B1,B1,B0,B0	; 26
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B1,B1,B1,B1,B0,B0	
		byte B2,B2,B0,B0,B0,B0,B0,B0	
		byte B2,B0,B0,B0,B0,B0,B0,B0	

		byte B2,B2,B2,B2,B2,B2,B2,B2	; 27
		byte B2,B2,B2,B2,B2,B2,B2,B2
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B1,B1,B1,B1,B1,B1
		byte B2,B2,B0,B0,B0,B0,B0,B0
		byte B2,B0,B0,B0,B0,B0,B0,B0

		byte B2,B2,B1,B1,B1,B1,B0,B0	; 28
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0	
		byte B1,B1,B1,B1,B1,B1,B0,B0	
		byte B1,B1,B1,B1,B1,B1,B0,B0	
		byte B1,B1,B1,B1,B1,B1,B0,B0	
		byte B0,B0,B0,B0,B0,B0,B0,B0
		byte B0,B0,B0,B0,B0,B0,B0,B0

		byte B2,B2,B2,B2,B2,B2,B2,B2	; 29
		byte B2,B2,B2,B2,B2,B2,B2,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B1,B1,B1,B1,B1,B1,B0,B0
		byte B0,B0,B1,B1,B1,B1,B0,B0
		byte B0,B2,B1,B1,B1,B1,B0,B0

		byte B2,B2,B1,B1,B1,B1,B0,B0	; 30
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B1,B1,B1,B1,B0,B0
		byte B2,B2,B0,B0,B0,B0,B0,B0
		byte B2,B0,B0,B0,B0,B0,B0,B0

	; orange tiles for L bloc

O2=$EC
O1=$C8
O0=$84

		byte O2,O2,O2,O2,O2,O2,O2,O2	; 31
		byte O2,O2,O2,O2,O2,O2,O2,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0

		byte O2,O2,O1,O1,O1,O1,O0,O0	; 32
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O1,O1,O1,O1,O1,O1,O0,O0	
		byte O1,O1,O1,O1,O1,O1,O0,O0	
		byte O1,O1,O1,O1,O1,O1,O0,O0	
		byte O1,O1,O1,O1,O1,O1,O0,O0	
		byte O0,O0,O0,O0,O0,O0,O0,O0
		byte O0,O0,O0,O0,O0,O0,O0,O0

		byte O2,O2,O2,O2,O2,O2,O2,O2	; 33
		byte O2,O2,O2,O2,O2,O2,O2,O2
		byte O1,O1,O1,O1,O1,O1,O1,O1
		byte O1,O1,O1,O1,O1,O1,O1,O1
		byte O1,O1,O1,O1,O1,O1,O1,O1
		byte O1,O1,O1,O1,O1,O1,O1,O1
		byte O0,O0,O0,O0,O0,O0,O0,O0
		byte O0,O0,O0,O0,O0,O0,O0,O0

		byte O2,O2,O2,O2,O2,O2,O2,O2	; 34
		byte O2,O2,O2,O2,O2,O2,O2,O2
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O0,O0,O0,O0,O0,O0
		byte O2,O0,O0,O0,O0,O0,O0,O0

		byte O2,O2,O1,O1,O1,O1,O0,O0	; 35
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0

		byte O2,O2,O1,O1,O1,O1,O0,O0	; 36
		byte O2,O2,O1,O1,O1,O1,O0,O2
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O0,O0,O0,O0,O0,O0
		byte O2,O0,O0,O0,O0,O0,O0,O0

		byte O2,O2,O2,O2,O2,O2,O2,O2	; 37
		byte O2,O2,O2,O2,O2,O2,O2,O0
		byte O1,O1,O1,O1,O1,O1,O0,O0
		byte O1,O1,O1,O1,O1,O1,O0,O0
		byte O1,O1,O1,O1,O1,O1,O0,O0
		byte O1,O1,O1,O1,O1,O1,O0,O0
		byte O0,O0,O0,O0,O0,O0,O0,O0
		byte O0,O0,O0,O0,O0,O0,O0,O0

		byte O2,O2,O2,O2,O2,O2,O2,O2	; 38
		byte O2,O2,O2,O2,O2,O2,O2,O0
		byte O1,O1,O1,O1,O1,O1,O0,O0
		byte O1,O1,O1,O1,O1,O1,O0,O0
		byte O1,O1,O1,O1,O1,O1,O0,O0
		byte O1,O1,O1,O1,O1,O1,O0,O0
		byte O0,O0,O1,O1,O1,O1,O0,O0
		byte O0,O2,O1,O1,O1,O1,O0,O0

		byte O2,O2,O1,O1,O1,O1,O0,O0	; 39
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O0,O0,O0,O0,O0,O0
		byte O2,O0,O0,O0,O0,O0,O0,O0

		byte O2,O2,O2,O2,O2,O2,O2,O2	; 40
		byte O2,O2,O2,O2,O2,O2,O2,O2
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O1,O1,O1,O1,O1,O1
		byte O2,O2,O1,O1,O1,O1,O0,O0
		byte O2,O2,O1,O1,O1,O1,O0,O0

	; purple tiles for P bloc

P2=$C3
P1=$82
P0=$41

		byte P2,P2,P2,P2,P2,P2,P2,P2	; 41
		byte P2,P2,P2,P2,P2,P2,P2,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0

		byte P2,P2,P2,P2,P2,P2,P2,P2	; 42
		byte P2,P2,P2,P2,P2,P2,P2,P2
		byte P2,P2,P1,P1,P1,P1,P1,P1
		byte P2,P2,P1,P1,P1,P1,P1,P1
		byte P2,P2,P1,P1,P1,P1,P1,P1
		byte P2,P2,P1,P1,P1,P1,P1,P1
		byte P2,P2,P0,P0,P0,P0,P0,P0
		byte P2,P0,P0,P0,P0,P0,P0,P0

		byte P2,P2,P1,P1,P1,P1,P0,P2	; 43
		byte P2,P2,P1,P1,P1,P1,P2,P2
		byte P1,P1,P1,P1,P1,P1,P1,P1
		byte P1,P1,P1,P1,P1,P1,P1,P1
		byte P1,P1,P1,P1,P1,P1,P1,P1
		byte P1,P1,P1,P1,P1,P1,P1,P1
		byte P0,P0,P0,P0,P0,P0,P0,P0
		byte P0,P0,P0,P0,P0,P0,P0,P0

		byte P2,P2,P2,P2,P2,P2,P2,P2	; 44
		byte P2,P2,P2,P2,P2,P2,P2,P0
		byte P1,P1,P1,P1,P1,P1,P0,P0
		byte P1,P1,P1,P1,P1,P1,P0,P0
		byte P1,P1,P1,P1,P1,P1,P0,P0
		byte P1,P1,P1,P1,P1,P1,P0,P0
		byte P0,P0,P0,P0,P0,P0,P0,P0
		byte P0,P0,P0,P0,P0,P0,P0,P0

		byte P2,P2,P1,P1,P1,P1,P0,P2	; 45
		byte P2,P2,P1,P1,P1,P1,P2,P2
		byte P2,P2,P1,P1,P1,P1,P1,P1
		byte P2,P2,P1,P1,P1,P1,P1,P1
		byte P2,P2,P1,P1,P1,P1,P1,P1
		byte P2,P2,P1,P1,P1,P1,P1,P1
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0

		byte P2,P2,P1,P1,P1,P1,P0,P0	; 46
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P0,P0,P0,P0,P0,P0
		byte P2,P0,P0,P0,P0,P0,P0,P0

		byte P2,P2,P2,P2,P2,P2,P2,P2	; 47
		byte P2,P2,P2,P2,P2,P2,P2,P2
		byte P1,P1,P1,P1,P1,P1,P1,P1
		byte P1,P1,P1,P1,P1,P1,P1,P1
		byte P1,P1,P1,P1,P1,P1,P1,P1
		byte P1,P1,P1,P1,P1,P1,P1,P1
		byte P0,P0,P1,P1,P1,P1,P0,P0
		byte P0,P2,P1,P1,P1,P1,P0,P0

		byte P2,P2,P1,P1,P1,P1,P0,P0	; 48
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P2,P2,P0,P0,P0,P0,P0,P0
		byte P2,P0,P0,P0,P0,P0,P0,P0

		byte P2,P2,P1,P1,P1,P1,P0,P0	; 49
		byte P2,P2,P1,P1,P1,P1,P0,P0
		byte P1,P1,P1,P1,P1,P1,P0,P0
		byte P1,P1,P1,P1,P1,P1,P0,P0
		byte P1,P1,P1,P1,P1,P1,P0,P0
		byte P1,P1,P1,P1,P1,P1,P0,P0
		byte P0,P0,P1,P1,P1,P1,P0,P0
		byte P0,P2,P1,P1,P1,P1,P0,P0

	; red tiles for Z bloc

R2=$C0
R1=$80
R0=$40

		byte R2,R2,R2,R2,R2,R2,R2,R2	; 50
		byte R2,R2,R2,R2,R2,R2,R2,R2
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R0,R0,R0,R0,R0,R0
		byte R2,R0,R0,R0,R0,R0,R0,R0

		byte R2,R2,R2,R2,R2,R2,R2,R2	; 51
		byte R2,R2,R2,R2,R2,R2,R2,R0
		byte R1,R1,R1,R1,R1,R1,R0,R0
		byte R1,R1,R1,R1,R1,R1,R0,R0
		byte R1,R1,R1,R1,R1,R1,R0,R0
		byte R1,R1,R1,R1,R1,R1,R0,R0
		byte R0,R0,R1,R1,R1,R1,R0,R0
		byte R0,R2,R1,R1,R1,R1,R0,R0

		byte R2,R2,R1,R1,R1,R1,R0,R0	; 52
		byte R2,R2,R1,R1,R1,R1,R0,R2
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R0,R0,R0,R0,R0,R0
		byte R2,R0,R0,R0,R0,R0,R0,R0

		byte R2,R2,R2,R2,R2,R2,R2,R2	; 53
		byte R2,R2,R2,R2,R2,R2,R2,R0
		byte R1,R1,R1,R1,R1,R1,R0,R0
		byte R1,R1,R1,R1,R1,R1,R0,R0
		byte R1,R1,R1,R1,R1,R1,R0,R0
		byte R1,R1,R1,R1,R1,R1,R0,R0
		byte R0,R0,R0,R0,R0,R0,R0,R0
		byte R0,R0,R0,R0,R0,R0,R0,R0

		byte R2,R2,R2,R2,R2,R2,R2,R2	; 54
		byte R2,R2,R2,R2,R2,R2,R2,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0

		byte R2,R2,R1,R1,R1,R1,R0,R0	; 55
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R0,R0,R0,R0,R0,R0
		byte R2,R0,R0,R0,R0,R0,R0,R0

		byte R2,R2,R1,R1,R1,R1,R0,R0	; 56
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R1,R1,R1,R1,R1,R1,R0,R0	
		byte R1,R1,R1,R1,R1,R1,R0,R0	
		byte R1,R1,R1,R1,R1,R1,R0,R0	
		byte R1,R1,R1,R1,R1,R1,R0,R0	
		byte R0,R0,R0,R0,R0,R0,R0,R0
		byte R0,R0,R0,R0,R0,R0,R0,R0

		byte R2,R2,R2,R2,R2,R2,R2,R2	; 57
		byte R2,R2,R2,R2,R2,R2,R2,R2
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R1,R1,R1,R1,R1,R1
		byte R2,R2,R1,R1,R1,R1,R0,R0
		byte R2,R2,R1,R1,R1,R1,R0,R0

	; other tiles

W2=$FF		
W1=$92
W0=$49

		byte W0,W0,W0,W0,W0,W0,W0,W0	; 58 
		byte W0,0,0,0,0,0,0,0
		byte W0,0,0,0,0,0,0,0
		byte W0,0,0,0,0,0,0,0
		byte W0,0,0,0,0,0,0,0
		byte W0,0,0,0,0,0,0,0
		byte W0,0,0,0,0,0,0,0
		byte W0,0,0,0,0,0,0,0	

		

tilesz=.-tiles

bloc_i:		byte 0,0,0,0
		byte 8,9,9,10
		byte 0,0,0,0
		byte 0,0,0,0

		byte 0,0,5,0
		byte 0,0,6,0
		byte 0,0,6,0
		byte 0,0,7,0

		byte 0,0,0,0
		byte 0,0,0,0
		byte 8,9,9,10
		byte 0,0,0,0

		byte 0,5,0,0
		byte 0,6,0,0
		byte 0,6,0,0
		byte 0,7,0,0

bloc_j:		byte 19,0,0,0
		byte 20,21,22,0
		byte 0,0,0,0
		byte 0,0,0,0

		byte 0,23,24,0
		byte 0,25,0,0
		byte 0,26,0,0
		byte 0,0,0,0

		byte 0,0,0,0
		byte 27,21,29,0
		byte 0,0,30,0
		byte 0,0,0,0

		byte 0,19,0,0
		byte 0,25,0,0
		byte 27,28,0,0
		byte 0,0,0,0

bloc_l:		byte 0,0,31,0
		byte 34,33,32,0
		byte 0,0,0,0
		byte 0,0,0,0

		byte 0,31,0,0
		byte 0,35,0,0
		byte 0,36,37,0
		byte 0,0,0,0

		byte 0,0,0,0
		byte 40,33,37,0
		byte 39,0,0,0
		byte 0,0,0,0

		byte 34,38,0,0
		byte 0,35,0,0
		byte 0,39,0,0
		byte 0,0,0,0

bloc_o:		byte 0,0,0,0
		byte 0,1,2,0
		byte 0,3,4,0
		byte 0,0,0,0

		byte 0,0,0,0
		byte 0,1,2,0
		byte 0,3,4,0
		byte 0,0,0,0

		byte 0,0,0,0
		byte 0,1,2,0
		byte 0,3,4,0
		byte 0,0,0,0

		byte 0,0,0,0
		byte 0,1,2,0
		byte 0,3,4,0
		byte 0,0,0,0

bloc_s:		byte 0,11,12,0
		byte 13,14,0,0
		byte 0,0,0,0
		byte 0,0,0,0

		byte 0,15,0,0
		byte 0,16,17,0
		byte 0,0,18,0
		byte 0,0,0,0

		byte 0,0,0,0
		byte 0,11,12,0
		byte 13,14,0,0
		byte 0,0,0,0

		byte 15,0,0,0
		byte 16,17,0,0
		byte 0,18,0,0
		byte 0,0,0,0

bloc_t:		byte 0,41,0,0
		byte 42,43,44,0
		byte 0,0,0,0
		byte 0,0,0,0

		byte 0,41,0,0
		byte 0,45,44,0
		byte 0,46,0,0
		byte 0,0,0,0

		byte 0,0,0,0
		byte 42,47,44,0
		byte 0,48,0,0
		byte 0,0,0,0

		byte 0,41,0,0
		byte 42,49,0,0
		byte 0,48,0,0
		byte 0,0,0,0

bloc_z:		byte 50,51,0,0
		byte 0,52,53,0
		byte 0,0,0,0
		byte 0,0,0,0
		
		byte 0,0,54,0
		byte 0,57,56,0
		byte 0,55,0,0
		byte 0,0,0,0

		byte 0,0,0,0
		byte 50,51,0,0
		byte 0,52,53,0
		byte 0,0,0,0

		byte 0,54,0,0
		byte 57,56,0,0
		byte 55,0,0,0
		byte 0,0,0,0

