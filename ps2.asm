; temp file used to quickly test PS/2 keyboard interface

scratch0=$80
scratch1=$81
kbdstate=$82

VIA_B=$100000
VIA_A=$101000
VIA_DDR_B=$102000
VIA_DDR_A=$103000

A_KBCLK=$02                            ; PS/2 keyboard clock (o/c)
B_KBDATA=$40                            ; PS/2 keyboard data (o/c)

		org $10000




1: 		lda <VIA_DDR_A		; release clock
		and #>~A_KBCLK
		sta <VIA_DDR_A

		jsr wait		; start bit

; XXX - check for proper start bit, error if not

		stz >scratch0		; clear scratch0

		jsr wait		; bit 0
		ror >scratch0

		jsr wait		; bit 1
		ror >scratch0
		
		jsr wait		; bit 2
		ror >scratch0
		
		jsr wait		; bit 3
		ror >scratch0
		
		jsr wait		; bit 4
		ror >scratch0
		
		jsr wait		; bit 5
		ror >scratch0

		jsr wait		; bit 6
		ror >scratch0

		jsr wait		; bit 7
		ror >scratch0

		jsr wait		; parity

; XXX - check for proper party, error if not

		jsr wait		; stop bit

; XXX - check for proper stop bit, error if not

		lda <VIA_DDR_A		; inhibit clock
		ora #>A_KBCLK
		sta <VIA_DDR_A

		lda >scratch0
		lsr
		lsr
		lsr
		lsr
		jsr hexout
		lda >scratch0
		and #>$0f
		jsr hexout

		bra >1b

chars:		byte $30,$31,$32,$33,$34,$35,$36,$37
		byte $38,$39,$41,$42,$43,$44,$45,$46

hexout:		tax
		lda <chars,x
		jsr <$60000
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

		

