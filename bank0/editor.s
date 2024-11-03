; ----------------------------------------------------------------
; Screen Editor Additions
; ----------------------------------------------------------------
; This adds the following features to the KERNAL screen editor:
; * CTRL + HOME: put cursor at bottom left
; * CTRL + DEL: delete to end of line
; * CTRL + CR: print screen
; * F-key shortcuts with SpeedDOS layout (LIST/RUN/DLOAD/DOS"$")
; * auto-scrolling of BASIC programs: when the screen scrolls
;   either direction, a new BASIC line is LISTed

.include "../core/kernal.i"
.include "persistent.i"

; from basic
.import list_line
.import store_d1_spaces
.import print_dec
.import send_printer_listen

; from printer
.import set_io_vectors
.import set_io_vectors_with_hidden_rom

.global kbd_handler
.global print_screen

.segment "screen_editor"

kbd_handler:
        lda     $CC
        bne     pass_to_kernal ; if cursor is off, don't intervene
        ldy     $CB      ; Matrix code of key
        lda     ($F5),y  ; Convert to PETSCII
        cmp     #3
        bne     :+
        jsr     reset_input
        beq     pass_to_kernal ; Always
:       ldx     $028D ; Current shift keys
        cpx     #4 ; CTRL key down
        beq     ctrl_down
        cpx     #2 ; CBM key down?
        bcc     shift_or_nothing
        bcs     pass_to_kernal ; CBM

ctrl_down:
        cmp     #$13 ; CTRL + HOME: put cursor at bottom left
        bne     :+
        jsr     hide_cursor
        ldy     #0
        sty     PNTR
        ldy     #24
        jsr     $E56A ; set cursor line
        jsr     reset_input ; Set Z=1
        beq     done

:       cmp     #$14 ; CTRL + DEL: delete to end of line
        bne     :+
        jsr     hide_cursor
        jsr     delete_rest_of_line
        jmp     done

:       cmp     #CR ; CTRL + CR: print screen
        bne     pass_to_kernal
        jsr     hide_cursor
        inc     $02A7
        inc     $CC
        jsr     print_screen
        jmp     cursor_on_done

pass_to_kernal:
        jmp     _evaluate_modifier

shift_or_nothing:
        cmp     #$11 ; CRSR DOWN
        beq     L92DD
        pha
        lda     #0
        sta     $02AB
        pla
        sec
        sbc     #$85 ; KEY_F1
        bcc     pass_to_kernal  ; Below F1
        cmp     #4
        bcs     pass_to_kernal  ; Higher than F7
        ; Function key
        cpy     $C5
        beq     done
        sty     $C5
        txa
        sta     $028E
        asl     a
        asl     a
        adc     ($F5),y
        sbc     #$83
        tay

        ; Y = Number of function key string
        ldx     #$FF
@ns:    inx
        dey
        beq     @fcp
@l:     lda     fkey_strings,x
        beq     @ns
        inx
        bne     @l

@fcp:   lda     fkey_strings,x
        sta     KEYD,y
        beq     @d
        inx
        iny
        bne     @fcp
@d:     sty     NDX

cursor_on_done:
        lsr     $02a7
        inc     $CC
done:   lda     #$7F
        sta     $DC00
        jmp     _disable_fc3rom

cursor_on_pass_kernal:
        lsr     $02a7
        lsr     $CC
jmp_ptk:
        jmp     pass_to_kernal

;
; CRSR Up/Down -- scrolling
;
L92DD:
        inc     $02A7
        inc     $CC           ; Cursor off
        txa
        and     #1
        bne     crsr_up
        lda     TBLX
        cmp     #24
        bne     cursor_on_pass_kernal
        ; Scroll down
        jsr     hide_cursor
        bit     $02AB
        bmi     L9312
        ldx     #25
:       dex
        bmi     cursor_on_pass_kernal
        lda     $D9,x
        bpl     :-
        jsr     read_line_num
        bcs     :-   ; no line numer on botton row
        inc     $14
        bne     :+
        inc     $15
:       jsr     _search_for_line
        bcs     L9322
        beq     cursor_on_pass_kernal
        bcc     L9322
L9312:  ldy     #0
        jsr     _lda_5f_indy
        tax
        iny
        jsr     _lda_5f_indy
        beq     cursor_on_pass_kernal
        stx     $5F
        sta     $60
L9322:  lda     #$8D
        jsr     $E716 ; output character to the screen
        jsr     L9448
        lda     #$80
        sta     $02AB
        ldy     PNTR
        beq     L933A
L9333:  cpy     #40
        beq     L933A
        dey
        bne     L9333
L933A:  sty     PNTR
        lda     #24
        sta     TBLX
        bne     cursor_on_done ; Always

crsr_up:
        lda     TBLX
        bne     cursor_on_pass_kernal
        ; Scroll up
        jsr     hide_cursor
        bit     $02AB
        bvs     @3
        ldx     #$FF
:       inx
        cpx     #25
        beq     @2
        lda     $D9,x
        bpl     :-
        jsr     read_line_num ; read line number on top of screen
        bcs     :-
        jsr     _search_for_line
@3:     lda     $5F
        ldx     $60
        cmp     $2B
        bne     @1
        cpx     $2C
        bne     @1
        lda     #0
        sta     $02AB
@2:     jmp     cursor_on_pass_kernal

@1:     sta     TXTPTR
        dex
        stx     TXTPTR + 1
        ldy     #$FF
@4:     iny
        jsr     _lda_TXTPTR_indy
:       tax
        bne     @4
        iny
        jsr     _lda_TXTPTR_indy
        cmp     $5F
        bne     @4
        iny
        jsr     _lda_TXTPTR_indy
        cmp     $60
        bne     :-
        dey
        tya
        clc
        adc     TXTPTR
        sta     $5F
        lda     TXTPTR + 1
        adc     #0
        sta     $60
        jsr     L9416
        jsr     $E566 ; cursor home
        jsr     L9448
        jsr     $E566 ; cursor home
        lda     #$40
        sta     $02AB
        jmp     cursor_on_done

hide_cursor:
        lsr     $CF   ; Cursor visible?
        bcc     :+
        ldy     $CE   ; Character below cursor
        ldx     $0287 ; Colour below cursor
        jsr     $EA18 ; put a character in the screen
:       rts

read_line_num:
        ldy     $ECF0,x ; low bytes of screen line addresses
        sty     TXTPTR
        and     #3
        ora     $0288
        sta     TXTPTR + 1
        ldy     #0
        jsr     _lda_TXTPTR_indy
        ; Try to read a lin number
        cmp     #':'
        bcs     @x
        sbc     #$2F ; Subtracts #$30 due to carry
        sec
        sbc     #$D0 ; Test for values <#$30
        bcs     @x   ; and blast off if that's the case
        ldy     #0
        sty     $14
        sty     $15
@1:     sbc     #$2F ; Subtracts #$30 due to carry, undoes subtract $D0
        sta     $07  ; Store digit

        ; Multiply $14/$15 by 10
        lda     $15
        sta     $22
        cmp     #25
        bcs     @x
        lda     $14
        asl     a
        rol     $22
        asl     a
        rol     $22
        adc     $14
        sta     $14
        lda     $22
        adc     $15
        sta     $15
        asl     $14
        rol     $15

        lda     $14
        adc     $07
        sta     $14
        bcc     :+
        inc     $15
:       jsr     _CHRGET
        bcc     @1
        clc
@x:     rts

L9416:  inc     $0292
        ldx     #25
:       dex
        beq     :+
        jsr     $E9F0 ; fetch a screen address
        lda     $ECEF,x
        sta     $AC
        lda     $D8,x
        jsr     $E9C8 ; shift screen line
        bmi     :-
:       jsr     $E9FF ; clear screen line X
        ldx     #$17
@1:     lda     $DA,x
        and     #$7F
        ldy     $D9,x
        bpl     :+
        ora     #$80
:       sta     $DA,x
        dex
        bpl     @1
        lda     $D9
        ora     #$80
        sta     $D9
        rts

L9448:  ldy     #1
        sty     $0F
        jsr     _lda_5f_indy
        beq     delete_rest_of_line
        iny
        jsr     _lda_5f_indy
        tax
        iny
        jsr     _lda_5f_indy
        jsr     print_dec
        jsr     list_line
reset_input:
        ; Reset quotation, reverse mode and insertions
        lda     #0
        sta     $D4
        sta     $D8
        sta     $C7
        ; Z=1, code depends on it
        rts

delete_rest_of_line:
        jsr     store_d1_spaces
        bcs     reset_input
L946E:  lda     #3
        sta     $9A
        rts

print_screen:
        lda     #7 ; secondary address
        jsr     send_printer_listen
        bcs     L946E
        jsr     set_io_vectors
        ldy     #0
        sty     $AC
        lda     $0288 ; video RAM address hi
        sta     $AD
        ldx     #25 ; lines
L9488:  lda     #CR
        jsr     BSOUT
        ldy     #0
L948F:  lda     ($AC),y
        sta     $D7
        and     #$3F
        asl     $D7
        bit     $D7
        bpl     L949D
        ora     #$80
L949D:  bvs     L94A1
        ora     #$40
L94A1:  jsr     BSOUT
        iny
        cpy     #40 ; columns
        bne     L948F
        tya
        clc
        adc     $AC
        sta     $AC
        bcc     L94B3
        inc     $AD
L94B3:  dex
        bne     L9488
        lda     #CR
        jsr     BSOUT
        jsr     CLRCH
        jmp     set_io_vectors_with_hidden_rom

fkey_strings:
        .byte   $8D, "LIST:", CR, 0
        .byte   $8D, "RUN:", CR, 0
        .byte   "DLOAD", CR, 0
        .byte   $8D, $93, "DOS",'"', "$",CR, 0
        .byte   $8D, "M", 'O' + $80, ":", CR, 0
        .byte   $8D, "OLD:", CR, 0
        .byte   "DSAVE", '"', 0
        .byte   "DOS", '"', 0

