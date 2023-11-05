.segment "HEADER"
	.byte "NES"		;identification string
	.byte $1A
	.byte $02		;amount of PRG ROM in 16K units
	.byte $01		;amount of CHR ROM in 8K units
	.byte $00		;mapper and mirroing
	.byte $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00
.segment "ZEROPAGE"
VAR:	.RES 1	;reserves 1 byte of memory for a variable named VAR
Pressing = $21
.segment "STARTUP"

RESET:
	SEI 		;disables interupts
	CLD			;turn off decimal mode

	LDX #%1000000	;disable sound IRQ
	STX $4017
	LDX #$00
	STX $4010		;disable PCM

	;initialize the stack register
	LDX #$FF
	TXS 		;transfer x to the stack

	; Clear PPU registers
	LDX #$00
	STX $2000
	STX $2001

	;WAIT FOR VBLANK
:
	BIT $2002
	BPL :-

	;CLEARING 2K MEMORY
	TXA
CLEARMEMORY:		;$0000 - $07FF
	STA $0000, X
	STA $0100, X
	STA $0300, X
	STA $0400, X
	STA $0500, X
	STA $0600, X
	STA $0700, X
		LDA #$FF
		STA $0200, X
		LDA #$00
	INX
	CPX #$00
	BNE CLEARMEMORY

	;WAIT FOR VBLANK
:
	BIT $2002
	BPL :-

	;SETTING SPRITE RANGE
	LDA #$02
	STA $4014
	NOP

	LDA #$3F	;$3F00
	STA $2006
	LDA #$00
	STA $2006

	LDX #$00
LOADPALETTES:
	LDA PALETTEDATA, X
	STA $2007
	INX
	CPX #$20
	BNE LOADPALETTES

;LOADING SPRITES
	LDX #$00
LOADSPRITES:
	LDA SPRITEDATA, X
	STA $0200, X
	INX
	CPX #$20	;16bytes (4 bytes per sprite, 8 sprites total)
	BNE LOADSPRITES

;LOADING BACKGROUND

LOADBACKGROUND:
	LDA $2002		;read PPU status to reset high/low latch
	LDA #$21
	STA $2006
	LDA #$00
	STA $2006
	LDX #$00
LOADBACKGROUNDP1:
	LDA Test, X
	STA $2007
	INX
	CPX #$00
	BNE LOADBACKGROUNDP1
LOADBACKGROUNDP2:
	LDA BACKGROUNDDATA+256, X
	STA $2007
	INX
	CPX #$00
	BNE LOADBACKGROUNDP2

;LOAD BACKGROUND PALETTEDATA
	LDA #$23	;$23D0
	STA $2006
	LDA #$D0
	STA $2006
	LDX #$00
LOADBACKGROUNDPALETTEDATA:
	LDA BACKGROUNDPALETTEDATA, X
	STA $2007
	INX
	CPX #$20
	BNE LOADBACKGROUNDPALETTEDATA

	;RESET SCROLL
	LDA #$00
	STA $2005
	STA $2005

;ENABLE INTERUPTS
	CLI

	LDA #%10010000
	STA $2000			;WHEN VBLANK OCCURS CALL NMI

	LDA #%00011110		;show sprites and background
	STA $2001

	INFLOOP:
    jsr Control
	ldx #0
	jsr Move
	ldx #16
	jsr Move
    JMP INFLOOP



Control:
  jsr intiControl
  lda $20
  sta $21
  rts

intiControl:
  ;Inti Output mem
    lda #1

    ;Send latch to pulse the controller
    sta $4016
    lda #0
    sta $4016
    ldx #0
    jmp read_loop

read_loop:

    lda $4016
    lsr
    ror $20
    inx
    cpx #8
    bne read_loop

    rts


    ldy #0
    ldx #%10000000
    stx $20
    Check:
    iny
    lda Pressing
    and $20
    cmp $20
    bne :+
    sta $21,Y
:
    lsr $20
    cpy #8
    bne Check
    rts
NMI:

	LDA #$02	;LOAD SPRITE RANGE
	STA $4014

	RTI
	Move:
	lda Pressing
	cmp #%10010000
	beq UpRight
	lda Pressing
	cmp #%10100000
	beq DownRight
	lda Pressing
	cmp #%01100000
	beq DownLeft
	lda Pressing
	cmp #%01010000
	beq UpLeft
	lda Pressing
	cmp #%00010000
	beq Up
	lda Pressing
	cmp #%00100000
	beq Down
	lda Pressing
	cmp #%10000000
	beq Right
	lda Pressing
	cmp #%01000000
	beq Left
	rts

UpRight:
	ldy #%10000000
	jsr Up
	jsr Right
	jmp Wait
UpLeft:
	ldy #%10000000
	jsr Up
	jsr Left
	jmp Wait

DownRight:
	ldy #%10000000
	jsr Down
	jsr Right
	jmp Wait

DownLeft:
	ldy #%10000000
	jsr Down
	jsr Left
	jmp Wait


Up:
	dec $0200,X
	dec $0204,X
	dec $0208,X
	dec $020C,X
	cpy #%10000000
	beq :+
	jmp Wait
	:
	rts
Down:
	lda $0200,X
	cmp #$d7
	beq :+
	inc $0200,X
	inc $0204,X
	inc $0208,X
	inc $020C,X
	:
	cpy #%10000000
	beq :+
	jmp Wait
	:
	rts
Left:
	dec $0203,X
	dec $0207,X
	dec $020b,X
	dec $020f,X
	cpy #%10000000
	beq :+
	jmp Wait
	:
	rts
Right:
	inc $0203,X
	inc $0207,X
	inc $020b,X
	inc $020f,X
	cpy #%10000000
	beq :+
	jmp Wait
	:
	rts



Wait:
	ldy #0
	ldx #0
	:
	iny
	:
	inx
	cpx #255
	bne :-
	cpy #8
	bne :--
	rts


PALETTEDATA:
	.byte $00, $0F, $00, $10, 	$00, $0A, $15, $01, 	$00, $29, $28, $27, 	$00, $34, $24, $14 	;background palettes
	.byte $31, $0F, $15, $30, 	$00, $0F, $11, $30, 	$00, $0F, $30, $27, 	$00, $3C, $2C, $1C 	;sprite palettes

SPRITEDATA:
;Y, SPRITE NUM, attributes, X
;76543210
;||||||||
;||||||++- Palette (4 to 7) of sprite
;|||+++--- Unimplemented
;||+------ Priority (0: in front of background; 1: behind background)
;|+------- Flip sprite horizontally
;+-------- Flip sprite vertically

	.byte $40, $00, $20, $40
	.byte $40, $01, $20, $48
	.byte $48, $10, $20, $40
	.byte $48, $11, $20, $48

	;sword
	.byte $50, $08, %00000001, $80
	.byte $50, $08, %01000001, $88
	.byte $58, $18, %00000001, $80
	.byte $58, $18, %01000001, $88


BACKGROUNDDATA:	;512 BYTES
	.byte $A2,$9F,$A6,$A6,$A9,$00,$00,$00,$00,$01,$02,$03,$04,$05,$00,$00,$00,$00,$00,$00,$00,$06,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$08,$09,$0a,$0b,$0b,$0b,$0c,$0d,$0e,$0f,$10,$11,$56,$13,$14,$0b,$15,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$16,$17,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$18,$19,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$1a,$1b,$1c,$1d,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$1e,$06,$1f,$00,$00,$00,$00,$00
	.byte $00,$00,$20,$21,$22,$23,$18,$24,$25,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$26,$27,$28,$00,$29,$2a,$00,$00,$00,$00,$00
	.byte $00,$00,$2b,$2c,$2d,$0b,$11,$2e,$2f,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$30,$31,$32,$33,$34,$35,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$36,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$18,$37,$38,$39,$3a,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$3b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$3c,$3d,$3e,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$3f,$40,$0b,$0b,$0b,$41,$42,$43,$44,$0b,$0b,$45,$0b,$0b,$0b,$0b,$46,$47,$48,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$49,$0b,$0b,$4a,$4b,$00,$4c,$4d,$0b,$4e,$4f,$50,$0b,$0b,$51,$00,$52,$53,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$3f,$54,$55,$12,$00,$00,$00,$57,$58,$59,$00,$5a,$5b,$5c,$5d,$00,$5e,$5f,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$60,$61,$00,$00,$62,$63,$64,$65,$00,$66,$67,$68,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$69,$00,$00,$6a,$6b,$6c,$00,$6d,$6e,$6f,$70,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$71,$72,$73,$0b,$74,$75,$76,$77,$78,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$4c,$83,$84,$85,$86,$35,$87,$00,$00,$00,$00,$00,$00

Test: ;512 BYTES
.byte $A2,$9F,$A6,$A6,$A9,$00,$B1,$A9,$AC,$A6,$9E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$A2,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

BACKGROUNDPALETTEDATA:	;32 bytes
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55

.segment "VECTORS"
	.word NMI
	.word RESET
	; specialized hardware interurpts
.segment "CHARS"
	.incbin "rom.chr"




