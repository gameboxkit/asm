; FORTH interpreter
;
; IP is held in register Y
; RP is a direct-page variable (rp)
; S is the data stack pointer
; X is W, at least after NEXT

		org $400

forth:		rep #>%00110000		; 16-bit registers

		ldy #test
		jmp next

test:		word lit
		word -64	 	; counter

top:		word dup		; print digit
		word lit
		word 64+48
		word plus
		word emit

		word onep		; increment counter
		word dup
		word lit
		word -54
		word less
		word zequ		; invert, because zbranch
		word zbranch
		word top-.
		word branch
		word -2

emit:		word .+2
		pla
		brk
		byte 0
		bra >next


	; 0BRANCH (f --- )
	; 
	; The run-time proceedure to conditionally branch. If f is false
	; (zero), the following in-line parameter is added to the interpretive
	; pointer to branch ahead or back. Compiled by IF, UNTIL, and WHILE.

_zbranch:	byte $87
		fcs /0BRANCH/
		word 0			; null link
zbranch:	word .+2
		pla			; get flag off stack
		beq >1f			; go branch if zero
		iny			; othewrwise, just bump IP
		iny			; to skip the branch target
		bra >next

	; BRANCH
	;
	; The run-time proceedure to unconditionally branch. An in-line offset
	; is added to the interpretive pointer IP to branch ahead or back.
	; BRANCH is compiled by ELSE, AGAIN, REPEAT.

_branch:	byte $86
		fcs /BRANCH/
		word _zbranch
branch:		word .+2
1:		tyx			; X=IP
		tya			; A=IP
		clc			; add (IP) to A
		adc >0,x		
		tay			; IP=A
		bra >next	

	; DUP ( n --- n n )
	;
	; Duplicate the value on the stack.

_dup:		byte $83
		fcs /DUP/
		word _branch
dup:		word .+2
		pla
		pha
		pha
		bra >next

	; DROP ( n --- )
	; 
	; Drop the number from the stack.

_drop:		byte $84
		fcs /DROP/
		word _dup
drop:		word .+2
		pla
		bra >next

	; LIT

_lit:		byte $83
		fcs /LIT/
		word _drop
lit:		word .+2	
		ldx >0,y
		phx
		iny
		iny
		bra >next

	; 1+ ( n1 --- n2 )
	; 
        ; Increment n1 by l.

_onep:		byte $82
		fcs /1+/
		word _lit
onep:		word .+2
		pla
		inc
		pha
		bra >next

	; 2+ ( n1 --- n2 )
	; 
        ; Increment n1 by 2.

_twop:		byte $82
		fcs /2+/
		word _onep
twop:		word .+2
		pla
		inc
		inc
		pha
		bra >next


docol:		tya
		sta (>rp)		; push IP onto return stack
		dec >rp
		dec >rp
		inx			; W + 2 -> DFA
		inx
		txy			; which is the new IP

		; FALL THROUGH 

	; next is the core of the address interpreter

next:		ldx >0,y		; X points to CFA now.
		iny			; bump IP
		iny
		jmp (0,x)		; jump to (CFA)

	; ;S 

semis:		inc >rp
		inc >rp
		lda (>rp)		; pop IP from return stack
		tay
		bra >next

	; ! ( n addr --- )
	; 
	; Store 16 bits of n at address.

_store:		byte $81
		fcs /!/
		word _twop
store:		word .+2
		plx			; X = addr
		pla			; A = n
		sta >0,x		
		bra >next

	; + ( n1 n2 --- sum )
	;
	; Leave the sum of n1+n2.

_plus:		byte $81
		fcs /+/
		word _store
plus:		word .+2
		pla
		clc
		adc >1,s
		sta >1,s
		bra >next

	; - ( n1 n2 -- diff )
	;
	; Leave the difference of n1-n2.

_sub:		byte $81
		fcs /-/
		word _plus
sub:		word .+2
		lda >3,s		; A=n1
		sec
		sbc >1,s		; A=n1-n2
		sta >3,s
		pla
		bra >next

	; SWAP ( n1 n2 --- n2 n1 )
	;
	; Exchange the top two values on the stack.

_swap:		byte $84
		fcs /SWAP/
		word _sub
swap:		word .+2
		pla
		plx
		pha
		phx
		bra >next

	; OVER ( n1 n2 --- nl n2 n1 )
	; 
	; Copy the second stack value, placing it as the new top.

_over:		byte $84
		fcs /OVER/
		word _swap
over:		word .+2
		lda >3,s
		pha
		bra >next

	; = ( n1 n2 --- f )
	;
	; Leave a true flag if n1=n2, otherwise leave a false flag.

_equal:		byte $81
		fcs /=/
		word _over
equal:		word .+2
		pla
		cmp >1,s
		beq >true_flag

false_flag:	lda #0
		sta >1,s
		bra >next

true_flag:	lda #$ffff
		sta >1,s
		bra >next

	; 0< ( n --- f )
	;
	; Leave a true flag if the number is less than zero (negative),
	; otherwise leave a false flag.

_zless:		byte $82
		fcs /0</
		word _equal
zless:		word .+2
		lda >1,s
		bmi >true_flag
		bra >false_flag

	; 0= ( n --- f )
	;
	; Leave a true flag is the number is equal to zero, otherwise leave a
	; false flag.

_zequ:		byte $82
		fcs /0=/
		word _zless
zequ:		word .+2
		lda >1,s
		beq >true_flag
		bra >false_flag

	; > ( n1 n2 --- f ) 
	;
	; Leave a true flag if n1 is greater than n2, otherwise a false flag.

_great:		byte $81
		fcs />/
		word _zequ
great:		word .+2
		pla			; A = n2
		sec
		sbc >1,s		
		bvc >1f
		eor #$8000
1:		bpl >false_flag		; no N flag means n2 >= n1
		bra >true_flag

	; < ( n1 n2 --- f )
	;
	; Leave a true flag if n1 is less than n2, otherwise a false flag.

_less:		byte $81
		fcs /</
		word _great
less:		word .+2
		pla			; A = n2
		sec
		sbc >1,s	
		beq >false_flag		; Z flag means n2 = n1
		bvc >1f
		eor #$8000
1:		bmi >false_flag		; N flag means n2 < n1
		bra >true_flag
	
