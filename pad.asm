
ctrl0:		bss 1
ctrl1:		bss 1
ctrl2:		bss 1
ctrl3:		bss 1

		org $400

1:		lda <VIA_B		; clock and latch both low
		and #>~B_PADCLK
		and #>~B_PADLE
		sta <VIA_B

		ora #>B_PADLE		; toggle load
		sta <VIA_B	
		and #>~B_PADLE		
		sta <VIA_B

		ldy #8			; eight bits
2:		lda <VIA_B		; read controllers

		ror 			; lsb into carry
		ror >ctrl0
		ror
		ror >ctrl1
		ror
		ror >ctrl2
		ror
		ror >ctrl3

		lda <VIA_B		; toggle clock
		ora #>B_PADCLK
		sta <VIA_B
		and #>~B_PADCLK	
		sta <VIA_B

		dey
		cpy #0
		bne >2b

		lda >ctrl0
		lsr
		lsr
		lsr
		lsr
		jsr hexout
		lda >ctrl0
		and #>$0f
		jsr hexout

		bra >1b

chars:		byte $30,$31,$32,$33,$34,$35,$36,$37
		byte $38,$39,$41,$42,$43,$44,$45,$46

hexout:		tax
		lda <chars,x
		brk
		byte 0
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

		

