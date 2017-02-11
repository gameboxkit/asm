; temp file used to quickly test PS/2 keyboard interface

keyb_scan=$16
chrout=$60008

		org $70000

		lda #>$f
		jsr hexout

1: 		lda <VIA_DDR_A		; release clock
		and #>~A_KBCLK
		sta <VIA_DDR_A

		jsr wait		; start bit

; XXX - check for proper start bit, error if not

		stz >keyb_scan		; clear keyb_scan

		jsr wait		; bit 0
		ror >keyb_scan

		jsr wait		; bit 1
		ror >keyb_scan
		
		jsr wait		; bit 2
		ror >keyb_scan
		
		jsr wait		; bit 3
		ror >keyb_scan
		
		jsr wait		; bit 4
		ror >keyb_scan
		
		jsr wait		; bit 5
		ror >keyb_scan

		jsr wait		; bit 6
		ror >keyb_scan

		jsr wait		; bit 7
		ror >keyb_scan

		jsr wait		; parity

; XXX - check for proper party, error if not

		jsr wait		; stop bit

; XXX - check for proper stop bit, error if not

		lda <VIA_DDR_A		; inhibit clock
		ora #>A_KBCLK
		sta <VIA_DDR_A

		lda >keyb_scan
		lsr
		lsr
		lsr
		lsr
		jsr hexout
		lda >keyb_scan
		and #>$0f
		jsr hexout

		bra >1b

chars:		byte $30,$31,$32,$33,$34,$35,$36,$37
		byte $38,$39,$41,$42,$43,$44,$45,$46

hexout:		xba
		lda #>0
		xba
		tax
		lda <chars,x
		jsr <chrout
		rts

wait:		lda <VIA_A		; wait for clock HIGH
		bit #>A_KBCLK
		beq >wait

1:		lda <VIA_A		; wait for clock LO
		bit #>A_KBCLK
		bne >1b
	
		lda <VIA_B		; read data
		clc
		bit #>B_KBDATA
		beq >2f
		sec
2:		rts

		

