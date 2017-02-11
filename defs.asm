; ***************************************************************************
;
; AVC 

AVC_BLOCKS=$80000		; sprite control blocks
AVC_TILES=$88000		; tile pixel data
AVC_MAP=$8C000			; map 
AVC_MAP_X=$8C800		; map X offset
AVC_MAP_Y=$8C801		; map Y offset
AVC_PALETTE=$8D000		; color palette
AVC_SPRITES=$8E000		; sprite pixel RAM
AVC_AUDIO=$8F000		; audio control

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
A_D8=$01				; D8 to AVC

	; port B bits of interest

B_XMIT=$80				; serial transmit output
B_KBDATA=$40				; PS/2 keyboard data (o/c)
B_PADCLK=$20				; controller clock
B_PADLE=$10				; controller latch enable
B_PAD4=$08				; controller 4 data
B_PAD3=$04				; controller 3 data
B_PAD2=$02				; controller 2 data
B_PAD1=$01				; controller 1 data

	; other bits of interest

IRQ_ENABLE=$80
IRQ_T1=$40
IRQ_T2=$20				
IRQ_CB2=$08
IRQ_CA2=$01

