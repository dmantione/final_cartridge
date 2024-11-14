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
.include "../core/fc3ioreg.i"
.include "persistent.i"

; from basic
.import print_line_basic
.import delete_rest_of_line
;.import print_dec
.import reset_input
.import send_printer_listen

; from printer
.import set_io_vectors
.import set_io_vectors_with_hidden_rom

; from monitor
.import scrolldown_monitor
.import scrollup_monitor

.global kbd_handler
.global print_screen

.segment "screen_editor"

kbd_handler:
        ; Either the FC3 ROM bank 0 or BASIC ROM will be visible, other situations will not
        ; occur. A will contain the value of $BE00, or the current FC3 bank if FC3 ROM is
        ; visible.
        eor     #fcio_nmi_line|fcio_c64_16kcrtmode|fcio_bank_0
        php
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
        beq     ctrl_pressed
        cpx     #2 ; CBM key down?
        bcc     shift_or_nothing
        bcs     pass_to_kernal ; CBM

ctrl_pressed:
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
        plp
        ; If FC3 ROM was not visible on entry, hide it, otherwise exit normally
        beq     :+ 
        jmp     _evaluate_modifier
:       jmp     kernal_check_modifier_keys

done:   lda     #$7F
        sta     $DC00
        plp
        ; If FC3 ROM was not visible on entry, hide it, otherwise exit normally
        beq     :+
        jmp     _disable_fc3rom
:       rts

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
        cpy     $C5 ; Key continously pressed?
        php         ; Save result until we know it is not scrolling
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
        lda     CBINV+1
        cmp     #$02
        bne     @t
        ldx     #fkey_strings_monitor-fkey_strings_basic-1
        ; F3 = 2, F5 = 3
        tya
        lsr     ; C = Distinction between F3/F5
        eor     #$01 ; Z = F3 or F5
        bne     @t
        ;  Scroll with F3/F5
        inc     $02A7
        inc     $CC           ; Cursor off
        bcs     :+
        plp     ; Don't care whether key was continously pressed, ignore
        lda     #0
        sta     TBLX
        jmp     scr_up
:       plp     ; Don't care whether key was continously pressed, ignore
        lda     #24
        sta     TBLX
        jmp     scr_dn
@t:     plp     ; Key continously pressed?
        beq     done
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
        jmp     done

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

;crsr_dn:
        lda     TBLX
        cmp     #24
        bne     cursor_on_pass_kernal
        ; Scroll down

scr_dn:
        jsr     hide_cursor
        ldx     #25
        ldy     CBINV+1
        cpy     #$02
        bne     @1
        jsr     scrolldown_monitor
        jmp     @2
@1:     jsr     scrolldown_basic
@2:     bcs     cursor_on_pass_kernal
        lda     #$80
        sta     $02AB
        ; Cursor on leftmost position
        lda     #24
        jsr     set_cursor
;        ldy     PNTR
;        beq     @3
;:       cpy     #40
;        beq     @3
;        dey
;        bne     :-
;@3:     sty     PNTR
        lda     #24
        sta     TBLX
        bne     cursor_on_done ; Always

crsr_up:
        lda     TBLX
        bne     cursor_on_pass_kernal
        ; Scroll up
scr_up:
        jsr     hide_cursor
        ldx     #$FF
        ldy     CBINV+1
        cpy     #$02
        bne     @1
        jsr     scrollup_monitor
        jmp     @2
@1:     jsr     scrollup_basic
@2:     bcs     cursor_on_pass_kernal
        lda     #$40
        sta     $02AB
        lda     #0
        jsr     set_cursor
;        jsr     $E566 ; cursor home
        jmp     cursor_on_done

hide_cursor:
        lsr     $CF   ; Cursor visible?
        bcc     :+
        ldy     $CE   ; Character below cursor
        ldx     $0287 ; Colour below cursor
        jsr     $EA18 ; put a character in the screen
:       rts


set_cursor:
        sta     TBLX
        ldy     #0
        lda     CBINV+1
        cmp     #$02
        bne     :+
        ldy     #7
:       sty     PNTR
        rts

scrolldown_basic:
        bit     $02AB
        bmi     @1
        ; Find a line number
:       dex
        bmi     @xcs
        lda     $D9,x ; high byte to pointer in screen ram
        bpl     :-
        jsr     read_line_num_basic
        bcs     :-   ; no line numer on botton row

        ; line in $14/$15, find next line
        inc     $14
        bne     :+
        inc     $15
:       jsr     _search_for_line
        bcs     @2    ; line found
        beq     @xcs  ; not found, no line after
        bcc     @2    ; not found, but there is a line after
@1:     ; Avoid scanning screen if key remains pressed, simply load next
        ; line in linked list.
        ldy     #0
        jsr     _lda_5f_indy
        tax
        iny
        jsr     _lda_5f_indy
        beq     @xcs
        stx     $5F
        sta     $60
@2:     ; We have the line to be shown in ($14), now do the scroll
        lda     #$8D
        jsr     $E716 ; shift+return, scrolls screen down
        jsr     print_line_basic
        clc
        .byte $24 ; skip next instruction
@xcs:   sec
        rts

scrollup_basic:
        bit     $02AB
        bvs     @3
:       inx
        cpx     #25
        beq     @2
        lda     $D9,x
        bpl     :-
        jsr     read_line_num_basic
        bcs     :-
        jsr     _search_for_line
@3:     ; End of program reached?
        lda     $5F
        ldx     $60
        cmp     $2B
        bne     @1
        cpx     $2C
        bne     @1
        lda     #0
        sta     $02AB
@2:     sec
        rts
        ; Scan linked list of lines until we have the previous line
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
        jsr     scroll_screen_up
        jsr     $E566 ; cursor home
        jsr     print_line_basic
        clc
        rts

;
; Attempt to read a BASIC line number
;
read_line_num_basic:
        ldy     $ECF0,x ; low bytes of screen line addresses
        sty     TXTPTR
        and     #3      ; Screen ram is only 4 pages
        ora     $0288   ; Base address of screen
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
        cmp     #6400 >> 8 ; Lines > 64000 do not exist
        bcs     @x
        sta     $22
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

        ; Add current digit
        lda     $14
        adc     $07
        sta     $14
        bcc     :+
        inc     $15
:       jsr     _CHRGET
        bcc     @1
        clc
@x:     rts


.global scroll_screen_up
scroll_screen_up:
        inc     $0292
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
@2:     lda     #CR
        jsr     BSOUT
        ldy     #0
@1:     lda     ($AC),y
        sta     $D7
        and     #$3F
        asl     $D7
        bit     $D7
        bpl     :+
        ora     #$80
:       bvs     :+
        ora     #$40
:       jsr     BSOUT
        iny
        cpy     #40 ; columns
        bne     @1
        tya
        clc
        adc     $AC
        sta     $AC
        bcc     :+
        inc     $AD
:       dex
        bne     @2
        lda     #CR
        jsr     BSOUT
        jsr     CLRCH
        jmp     set_io_vectors_with_hidden_rom

fkey_strings:
fkey_strings_basic:
        .byte   $8D, "LIST:", CR, 0
        .byte   $8D, "RUN:", CR, 0
        .byte   "DLOAD", CR, 0
        .byte   $8D, $93, "DOS",'"', "$",CR, 0
        .byte   $8D, "M", 'O' + $80, ":", CR, 0
        .byte   $8D, "OLD:", CR, 0
        .byte   "DSAVE", '"', 0
        .byte   "DOS", '"', 0
fkey_strings_monitor:
        .byte   $8D, "R", CR, 0
        .byte   0
        .byte   0
        .byte   $8D, $93, "@$",CR, 0
        .byte   $8D, "X", CR, 0
        .byte   0
        .byte   0
        .byte   "@", 0

