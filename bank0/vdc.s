
.segment "vdc"

.global init_vdc
.global vdc_wait
.global vdc_reg_read
.global vdc_reg_write

vdc_wait:
        ldy     #63
:       dey
        beq     @r
        bit     $d600
        bpl     :-
@r:     rts


vdc_reg_read:
        tya
        stx     $d600
vdld:   jsr     vdc_wait
        tay
        lda     $d601
        rts

vdc_reg_write:
        pha
        tya
        stx     $d600
vdst:   jsr     vdc_wait
        tay
        pla
        sta     $d601
        rts

;
; Detects whether we run on a C128 in C64 mode
;
; Returns:
;  A<>0 - Commodore 64
;  A=0  - Commodore 128
;  Z=0  - Commodore 64
;  Z=1  - Commodore 128
;
detect_c128:
        lda  #$fe
        sta  $d02f
        sta  $d030
        eor  $d02f
        eor  $d030
        eor  #$fe
        bne  _rts ; no c128
        ; A=0
        sta  $d02f
        sta  $d030
        eor  $d02f
        eor  $d030
        eor  #$04
_rts:   rts

vdc_addr_1000:
        lda     #>$1000
vdc_addr:
        ldx     #$12
        jsr     vdc_reg_write
        inx
        lda     #$00
        jsr     vdc_reg_write
        ldx     #$1f ; VDC data register
        stx     $d600
        tya
        jsr     vdc_wait ; always
        tay
        rts

init_vdc:
        jsr     detect_c128
        bne     _rts
        ;       initializing VDC
        jsr     init_vdc_regs

        ;       detecting 16K/64K VRAM
        ldy     #$00
        jsr     vdc_addr_1000
        lda     $d601
        sty     $02  ; zero page will be erased, can use any addr as temp
        pha
@1:     jsr     y_to_1000
        lda     #>$9000
        jsr     vdc_addr
        cpy     $d601
        bne     :+
:       iny
        bne     @1
        lda     #$2f ; 16K RAM
        dec     $02
        bne     :+
        lda     #$3f ; 64K RAM
:       ldx     #$1c ; VDC RAM tyoe register
        jsr     vdc_reg_write
        pla
        tay
y_to_1000:
        jsr     vdc_addr_1000
        sty     $d601
        rts


        ;       initializing VDC
init_vdc_regs:
        ldy     #$00
        jsr     @progregs
        lda     $d600
        and     #$07
        beq     :+ ; version 0
        ; version 1/2
        jsr     @progregs
:       lda     $02A6
        beq     @x
        ldy     #palregvals-regvals
        bne     @progregs ; always
@l:     iny
        lda     regvals,y
        iny
        jsr     vdc_reg_write
@progregs:
        ldx     regvals,y
        bpl     @l
        iny
@x:     rts

        ;       CRTC 8563 Set Up Pairs <- from $e2f8 in C128 Kernal

regvals:
        .byte    $00,$7e,$01,$50,$02,$66,$03,$49
        .byte    $04,$20,$05,$00,$06,$19,$07,$1d
        .byte    $08,$00,$09,$07,$0a,$20,$0b,$07
        .byte    $0c,$00,$0d,$00,$0e,$00,$0f,$00
        .byte    $14,$08,$15,$00,$17,$08,$18,$20
        .byte    $19,$40,$1a,$f0,$1b,$00
        .byte    $1d,$07,$22,$7d,$23,$64,$24,$05
        .byte    $16,$78,$ff
ver12regvals:
        .byte    $19,$47,$ff
palregvals:
        .byte    $04,$26,$07,$20,$00,$7f,$ff
