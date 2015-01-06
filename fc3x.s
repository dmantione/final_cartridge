; da65 V2.14 - Git d112322
; Created:    2015-01-06 18:50:57
; Input file: fc3x.bin
; Page:       1


        .setcpu "6502"

L0073           := $0073
L0079           := $0079
L819F           := $819F
L81FE           := $81FE
L8315           := $8315
L8B54           := $8B54
L8C02           := $8C02
L9229           := $9229
L94C9           := $94C9
L9511           := $9511
L9881           := $9881
L9900           := $9900
L9903           := $9903
LA161           := $A161
LA19C           := $A19C
LA1C5           := $A1C5
LA1CB           := $A1CB
LA533           := $A533
LA613           := $A613
LA68E           := $A68E
LA6F3           := $A6F3
LA724           := $A724
LA7AE           := $A7AE
LA7EF           := $A7EF
LA82C           := $A82C
LA96B           := $A96B
LAB47           := $AB47
LAD8A           := $AD8A
LAE8D           := $AE8D
LB395           := $B395
LB7F7           := $B7F7
LBBA6           := $BBA6
LBC49           := $BC49
LBD7E           := $BD7E
LBDCD           := $BDCD
LBDD7           := $BDD7
LBDDD           := $BDDD
LE257           := $E257
LE37B           := $E37B
LE422           := $E422
LEB42           := $EB42
LEB48           := $EB48

.segment        "fc3x": absolute

LDE00:  rti

        sta     LDFFF
        rts

LDE05:  pha
        lda     #$40
LDE08:  sta     LDFFF
        pla
        rts

        sty     $01
LDE0F:  pha
        lda     #$70
        bne     LDE08
        jsr     LDE0F
        jmp     LE37B

LDE1A:  ora     #$07
        sta     $01
        bne     LDE05
        tay
        tay
        lda     $01
        pha
        jsr     LDE1A
        jsr     L9900
LDE2B:  tax
        pla
        sta     $01
        txa
        ldx     $AE
        jmp     LDE0F

        lda     $01
        pha
        jsr     LDE1A
        jsr     L9903
        jmp     LDE2B

        lda     $01
        jsr     LDE1A
        jmp     L81FE

        jsr     LDE05
        jmp     L8C02

        jsr     LDE05
        jmp     L819F

        lda     $02A7
        beq     LDE5D
        jmp     LEB42

LDE5D:  lda     $A000
        jmp     LDF80

        sta     $01
        lda     ($AC),y
        inc     $01
        inc     $01
        rts

        dec     $01
        lda     ($BB),y
        inc     $01
        rts

        jsr     LDF1B
        jsr     L8315
        jsr     LDE0F
        jmp     LA7AE

        jsr     LDE0F
        jmp     LA7EF

        jsr     LDE0F
        jsr     LBD7E
        jmp     LDE05

        jsr     LDE0F
        jmp     LAE8D

        jsr     LDE0F
        jsr     LAD8A
        jsr     LB7F7
        jmp     LDE05

        jsr     LDE05
        jsr     L8B54
        jmp     L9881

        jsr     LDE0F
        jmp     LEB48

        jsr     LDE0F
        jsr     LA96B
        jmp     LDE05

        jsr     LDE0F
        jsr     LAB47
        jmp     LDE05

        jsr     LDE0F
        jsr     LA68E
        jmp     LDE05

        jsr     LDE0F
        jsr     LA82C
        jmp     LDE05

        jsr     LDE0F
        jsr     LA533
        beq     LDEE1
        jsr     LDE0F
        jsr     LE257
LDEE1:  jmp     LDE05

        jsr     LDE0F
        jsr     LBC49
        jsr     LBDDD
        jmp     LDE05

        jsr     LDE0F
        jsr     LB395
        jmp     LDEFF

        jsr     LDE0F
        jsr     LBBA6
LDEFF:  iny
        jsr     LBDD7
        jmp     LDE05

        jsr     LDE0F
        jsr     LBDCD
        jmp     LDE05

        jsr     LDE0F
        jsr     LA613
        php
        jsr     LDE05
        plp
        rts

LDF1B:  jsr     LDE0F
        jsr     L0073
LDF21:  php
        jsr     LDE05
        plp
        rts

        jsr     LDE0F
        jsr     L0079
        jmp     LDF21

        jsr     LDE0F
        lda     ($5A),y
        jmp     LDE05

        jsr     LDE0F
        lda     ($5F),y
        jmp     LDE05

        jsr     LDE0F
        lda     ($AE,x)
        jmp     LDE05

        jsr     LDE0F
        lda     ($7A),y
        jmp     LDE05

        jsr     LDE0F
        lda     ($7A,x)
        jmp     LDE05

        jsr     LDE0F
        lda     ($22),y
        jmp     LDE05

        jsr     LDE0F
        lda     ($8B),y
        jmp     LDE05

        jsr     LDE0F
        jmp     LA724

        jsr     LDE0F
        jmp     LA6F3

        jsr     LDE0F
        jsr     LE422
        jsr     LDE05
        jmp     L9511

LDF80:  .addr   L94C9
        bne     LDF8A
        jsr     LDE05
        jmp     L9229

LDF8A:  jmp     LEB48

        jsr     LDE05
        .byte   $20,$53,$82,$4C,$0F,$DE,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        jsr     LDE05
        jsr     LA161
        jmp     LDE0F

        jsr     LDE05
        jmp     LA19C

        jsr     LDE05
        jmp     LA1C5

        jsr     LDE05
        jmp     LA1CB

        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF
        sei
        lda     #$42
        sta     LDFFF
        lda     LDE00
        pha
        lda     $A000
        pha
        lda     #$41
        sta     LDFFF
        .byte   $3A,$2A,$FF,$FF
LDFFF:  .byte   $FF

; End of "fc3x" segment
.code

