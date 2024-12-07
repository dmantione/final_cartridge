; ----------------------------------------------------------------
; BASIC Extension
; ----------------------------------------------------------------
; "AUTO" Command    - automatically number a BASIC program
; "HELP" Command    - list BASIC line of last error
; "MREAD" Command   - read 192 bytes from RAM into buffer
; "MWRITE" Command  - write 192 bytes from buffer into RAM
; "DEL" Command     - delete BASIC lines
; "RENUM" Command   - renumber BASIC lines
; "FIND" Command    - find a string in a BASIC program
; "OLD" Command     - recover a deleted program
; "OFF" Command     - disable BASIC extensions
; "KILL" Command    - disable all cartridge functionality
; "MON" Command     - enter machine code monitor
; "BAR" Command     - enable/disable pull-down menu
; "DESKTOP" Command - start Desktop
; "DLOAD" Command   - load a program from disk
; "DVERIFY" Command - verify a program on disk
; "DSAVE" Command   - save a program to disk
; "DAPPEND" Command - append a program from disk to program in RAM
; "APPEND" Command  - append a program to program in RAM
; "DOS" Command     - send command to drive
; "PLIST" Command   - send BASIC listing to printer
; "PDIR" Command    - send disk directoy to printer
; "DUMP" Command    - show list of all BASIC variables
; "ARRAY" Command   - show list of all BASIC arrays
; "MEM" Command     - display memory usage
; "TRACE" Command   - enable/disable printing each BASIC line executed
; "REPLACE" Command - replace a string in a BASIC program
; "ORDER" Command   - reorder BASIC lines after APPEND
; "UNPACK" Command  - decompress a program
; "PACK" Command    - compress a program

.include "../core/kernal.i"
.include "../core/fc3ioreg.i"
.include "persistent.i"

; from monitor
.import monitor
.importzp et_call

; from drive
.import listen_6F_or_error
.import listen_or_error
.import device_not_present
.import cmd_channel_listen
.import command_channel_talk
.import listen_second
.import print_line_from_drive
.import talk_second

; from printer
.import set_io_vectors
.import set_io_vectors_with_hidden_rom
.import cent_rs232_or_cbm
.import new_bsout2

; from wrappers
.import WA3BF
.import WA49F
.import WA560
.import WA663_E386
.import WA6C3
.import WA8F8
.import WAF08
.import WE159
.import WE16F
.import WE175
.import WE1D4
.import disable_rom_jmp_overflow_error
.import disable_rom_then_warm_start

; from init
.import go_basic
.import go_desktop
.import cond_init_load_save_vectors

; from format
.import fast_format
.import init_read_disk_name
.import init_write_bam

; from constants
.import pow10lo
.import pow10hi

; from drive
.import set_colon_asterisk
.import set_drive

.global bar_flag
.global evaluate_hex_expression
.global new_mainloop
.global new_tokenize
.global new_execute
;.global list_line
;.global clear_right_from_cursor
.global print_dec
.global print_msg
.global messages
.global a_ready
.global send_drive_command
.global send_printer_listen
.global reset_warmstart
.global new_detokenize

; variables
trace_flag      := $02AA
bar_flag        := $02A8

.segment "basic_commands"

;        .byte 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17

evaluate_hex_expression:
        lda     #0
        ldx     #10
@1:
        sta     $5D,x
        dex
        bpl     @1
L81B9:  jsr     _CHRGET
        bcc     L81C4
        cmp     #'A'
        bcc     L81DF
        sbc     #8
L81C4:  sbc     #$2F
        cmp     #$10
        bcs     L81DF
        pha
        lda     $61
        beq     L81D8
        adc     #4
        bcc     L81D6
        jmp     disable_rom_jmp_overflow_error

L81D6:  sta     $61
L81D8:  pla
        jsr     _add_a_to_fac1
        jmp     L81B9

L81DF:  clc
        jmp     _disable_fc3rom

set_bsout_to_screen:
        lda     #<PRT  ; Kernel print to screen routine
        sta     $0326
        lda     #>PRT
        sta     $0327
        rts

; ----------------------------------------------------------------
; "AUTO" Command - automatically number a BASIC program
; ----------------------------------------------------------------
AUTO:   jsr     load_auto_defaults
        jsr     L8512
        jsr     L84ED
        pla
        pla
        lda     #$40
L81FB:  sta     $02A9
; code is laid out so it flows into new_mainloop

; ----------------------------------------------------------------

auto_current_line_number   := $0334
auto_line_number_increment := $0336

new_mainloop:
        jsr     set_irq_and_kbd_handlers
        jsr     cond_init_load_save_vectors
        jsr     set_bsout_to_screen
        jsr     WA560
        stx     TXTPTR
        sty     TXTPTR + 1
        jsr     set_io_vectors_with_hidden_rom
        jsr     uninstall_kbd_handler
        jsr     CHRGET
        tax
        beq     L81FB
        ldx     $3A
        stx     $02AC
        ldx     #$FF
        stx     $3A
        bcc     L822B
mainloop_call_tokenize:
        jsr     new_tokenize
        jmp     _new_execute

L822B:  jsr     _basic_string_to_word
        tax
        bne     L8234
        sta     $02A9
L8234:  bit     $02A9
        bvc     L824D
        clc
        lda     $14
        adc     auto_line_number_increment
        sta     auto_current_line_number
        lda     $15
        adc     auto_line_number_increment + 1
        sta     auto_current_line_number + 1
        jsr     L84ED
L824D:
        jmp     WA49F

; this is 99% identical with the code in BASIC ROM at $A57C
new_tokenize:
.ifdef JIFFY_TOKENIZER
        pla
        pha            ; This checks whether we were called from the mainloop
        cmp     #<(mainloop_call_tokenize+2)
        bne     @normal_tokenize
        jsr     is_jiffy_special_char
        bne     @normal_tokenize
        ldx     $7a
        ldy     #4
        tya
        bne     @load_skip_a ; Always
@normal_tokenize:
.endif
; **** this is the same code as BASIC ROM $A579-$A5AD (start) ****
        ldx     TXTPTR
        ldy     #4
        sty     $0F
@loadchar:
        lda     $0200,x ; read character from direct mode
        bpl     @notoken
        cmp     #$FF ; PI
        beq     @nextchar
        inx
        bne     @loadchar
@notoken:
        cmp     #' '
        beq     @nextchar
        sta     $08              ; If '"' then '"' is the char that ends skipping
        cmp     #'"'
        beq     @skip_til_end
        bit     $0F
        bvs     @nextchar
        cmp     #'?'
        bne     @not_print
        lda     #$99 ; PRINT token
        bne     @nextchar
@not_print:
        cmp     #'0'
        bcc     @punctuation
        cmp     #'<'
        bcc     @nextchar
@punctuation:
        ; PETSCII characters less than '0', i.e. punctuation characters like @#?,.
        sty     $71
; **** this is the same code as BASIC ROM $A579-$A5AD (end) ****

        stx     TXTPTR
        ldy     #<(new_basic_keywords - 1)
        sty     $22
        ldy     #>(new_basic_keywords - 1)
        sty     $23
@zero_token:
        ldy     #0  ; Start at token 0
        sty     $0B
        dex
@incptr_cmpchar:
        inx
        inc     $22
        bne     @cmpchar
        inc     $23
@cmpchar:
        lda     $0200,x
        sec
        sbc     ($22),y
        beq     @incptr_cmpchar
        cmp     #$80
        bne     @char_nomatch
        ldy     $23
        cpy     #>basic_keywords  ; $22/$23 pointing to original token table?
        bcs     :+
        lda     $0B
        adc     #$CC   ; Final Cartridge III tokens start at $CC
        .byte   $2C
:       ora     $0B
@maybe_skip:
        ldy     $71
; **** this is the same code as BASIC ROM $A5C9-$A5F8 (start) ****
@nextchar:
        inx
        iny
        sta     $0200 - 5,y
        ora     #$00
        beq     @done
        sec
        sbc     #':'      ; Is it a ":" ?
        beq     @colon_or_data
        cmp     #$83-':'  ; Is it a DATA token?
        bne     @nor_colon_nor_data
@colon_or_data:
        sta     $0F
@nor_colon_nor_data:
        sec
        sbc     #$8f-':'  ; Is it a REM token ?
        bne     @loadchar
        ; REM token, no further tokenizaton until end of line
@load_skip_a:
        sta     $08
@loadskip:
        lda     $0200,x
        beq     @nextchar
        cmp     $08       ; Value in $08 = char that stops skipping
        beq     @nextchar
@skip_til_end:
        iny
        sta     $0200 - 5,y
        inx
        bne     @loadskip
@char_nomatch:
        ldx     TXTPTR
        inc     $0B   ; increase token
; **** this is the same code as BASIC ROM $A5C9-$A5F8 (end) ****

        ; Move the token pointer to the next token
@tokenptr_nexttoken:
        lda     ($22),y ; Load current char of token
        php
        inc     $22     ; Inc token pointer low byte
        bne     :+
        inc     $23     ; Inc token pointer high byte
:       plp
        bpl     @tokenptr_nexttoken
        lda     ($22),y
        bne     @cmpchar
        lda     #fcio_bank_0|fcio_c64_8kcrtmode|fcio_nmi_line
        sta     fcio_reg
        ; End of token table
        lda     $23     ; $22/$23 pointing to original token table?
        cmp     #>(basic_keywords - 1)
        bcs     @get_next_char
        lda     #>(basic_keywords - 1)
        sta     $23
        lda     #<(basic_keywords - 1)
        sta     $22
        bne     @zero_token ; always

; **** this is the same code as BASIC ROM $A604-$A612 (start) ****
@get_next_char:
        lda     $0200,x
        bpl     @maybe_skip
@done:  sta     $01FD,y
        dec     TXTPTR + 1
        lda     #$FF
        sta     TXTPTR
        rts
; **** this is the same code as BASIC ROM $A604-$A612 (end) ****

new_execute:
        beq     L8342
        ldx     $3A
        inx
        beq     L8327 ; direct mode
        ldx     trace_flag
        beq     L8327 ; no tracing
        jsr     trace_command
        jsr     _CHRGOT
L8327:  cmp     #$CC ; first new token
        bcs     L832F
        sec
L832C:  jmp     _execute_statement

L832F:  cmp     #$E9 ; last new token + 1
        bcs     L832C
        sbc     #$CB
        asl     a
        tay
        lda     command_vectors+1,y
        pha
        lda     command_vectors,y
        pha
        jmp     _CHRGET

L8342:  jmp     _disable_fc3rom

trace_command:
        lda     PNTR ; save cursor state
        pha
        lda     $D5
        pha
        lda     TBLX
        pha
        lda     $D4
        pha
L8351:  lda     $028D ; modifier key
        lsr     a
        lsr     a
        bcs     L8351 ; CBM is down
        lsr     a
        bcc     L8369 ; CTRL is now down
        lda     #2
        ldx     #0
L835F:  iny
        bne     L835F ; delay for a bit
        inx
        bne     L835F
        sbc     #1
        bne     L835F
L8369:  jsr     $E566 ; cursor home
        jsr     L839D
        ldx     $B1
        jsr     $E88C ; set cursor row
        ldy     $B0
        sty     PNTR
        lda     ($D1),y
        eor     #$80
        sta     ($D1),y
        pla
        sta     $D4
        pla
        tax
        jsr     $E88C ; set cursor row
        pla
        sta     $D5
        pla
        sta     PNTR
L838C:  rts


.ifdef JIFFY_TOKENIZER

.proc is_jiffy_special_char
        ldy   #jiffy_special_characters_size-1
        jsr   CHRGOT
:       cmp   jiffy_special_characters,y
        beq   :+
        dey
        bpl   :-
:       rts
.endproc

.endif

; ----------------------------------------------------------------
; "HELP" Command - list BASIC line of last error
; ----------------------------------------------------------------
HELP:   ldx     $3A ; line number hi
        inx
        bne     L839D ; not direct mode
        lda     TXTPTR + 1
        cmp     #>$0200
        bne     L839D ; not direct mode
        ldx     $02AC
        stx     $3A ; line number hi
L839D:  ldx     $3A ; line number hi
        txa
        inx
        beq     L838C ; RTS
        ldx     $39 ; line number lo
        jsr     print_dec
        jsr     _search_for_line
        lda     PNTR
        sta     $B0
        lda     TBLX
        sta     $B1
        jsr     list_line
L83BA:  lda     #CR
        jsr     _basic_bsout
        bit     $13
        bpl     L838C ; RTS
        lda     #$0A ; LF
        jmp     _basic_bsout

list_line:
        ldy     #3
        sty     $49
        sty     $0F
        lda     #' '
; **** the following code is very similar to BASIC ROM $A6F1
@l:     jsr     _basic_bsout
        cmp     #'"'
        bne     :+
        lda     $0F
        eor     #$80 ; BASIC ROM says: "eor #$FF"
        sta     $0F
:       iny
        ldx     $60
        tya
        clc
        adc     $5F
        bcc     :+
        inx
:       cmp     TXTPTR
        bne     :+
        cpx     TXTPTR + 1 ; chrget hi
        bne     :+
        lda     PNTR
        sta     $B0
        lda     TBLX
        sta     $B1
:       jsr     _lda_5f_indy
        beq     clear_right_from_cursor
        jsr     do_detokenize
        jmp     @l ; loop

clear_right_from_cursor:
        lda     #' '
        ldy     PNTR
        beq     @rts
:       sta     ($D1),y
        cpy     $D5
        iny
        bcc     :-
@rts:   rts

print_dec:
        stx     $C1
        sta     $C2
        lda     #$31
        sta     $C3
        ldx     #4
L841C:  dec     $C3
L841E:  lda     #$2F
        sta     $C4
        sec
        ldy     $C1
        .byte   $2C
L8426:  sta     $C2
        sty     $C1
        inc     $C4
        tya
        sbc     pow10lo,x
        tay
        lda     $C2
        sbc     pow10hi,x
        bcs     L8426
        lda     $C4
        cmp     $C3
        beq     L8443
        jsr     new_bsout2
        dec     $C3
L8443:  dex
        beq     L841C
        bpl     L841E
        rts

; Prints a the BASIC program line pointed to by ($14) with line number at cursor position
; Used by the screen editor for scrolling
.global print_line_basic
print_line_basic:
        ldy     #1  ; Disable quotation mode
        sty     $0F
        jsr     _lda_5f_indy  ; Get high byte of link
        beq     delete_rest_of_line ; End of program? Then empty line
        ; Print line number
        iny
        jsr     _lda_5f_indy
        tax
        iny
        jsr     _lda_5f_indy
        jsr     print_dec
        ; Print line
        jsr     list_line
.global reset_input
reset_input:
        ; Reset quotation, reverse mode and insertions
        lda     #0
        sta     $D4
        sta     $D8
        sta     $C7
        ; Z=1, code depends on it
        rts
.global delete_rest_of_line
delete_rest_of_line:
        jsr     clear_right_from_cursor
        bcs     reset_input ; always

; ----------------------------------------------------------------

.global print_dir
print_dir:
        lda     #$60
        jsr     talk_second
        jsr     IECIN ; skip load address
        jsr     IECIN
@nextline:
        jsr     maybe_retalk
        jsr     IECIN
        jsr     IECIN ; skip link word
        jsr     IECIN ; line number lo
        tax
        jsr     IECIN ; line number hi
        ldy     ST
        bne     @end_of_dir
        jsr     maybe_relisten_prn
        jsr     print_dec
        lda     #' '
;        jsr     _basic_bsout
        jsr     new_bsout2
        bcs     @end_of_dir
        ldx     #$18
@rl:    jsr     maybe_retalk
        jsr     IECIN
@l:     cmp     #CR
        beq     @cr
        cmp     #CR + $80
        bne     :+
@cr:    lda     #$1F
:       ldy     ST
        bne     @end_of_dir
        jsr     maybe_relisten_prn
;        jsr     _basic_bsout
        jsr     new_bsout2
        bcs     @end_of_dir
        inc     $D8
        jsr     GETIN
        cmp     #3 ; STOP
        beq     @end_of_dir
        cmp     #' '
        bne     @goon
        ; space bar, wait for key
:       jsr     GETIN
        beq     :-
@goon:  dex
        bpl     @rl
        jsr     maybe_retalk
        jsr     IECIN
        bne     @l
        jsr     maybe_relisten_prn
        lda     #CR
;        jsr     _basic_bsout
        jsr     new_bsout2
        bcs     @end_of_dir
        bne     @nextline
@end_of_dir:
        lda     #$E0
        jsr     talk_second
        jmp     UNLSTN

maybe_retalk:
        lda     $9A
        cmp     #3
        beq     L84DB ; output to screen
        bit     $DD0C
        bmi     L84DB ; centronics printer disabled
        jsr     UNLSTN
        lda     #$60
        jsr     talk_second
L84DB:  rts

maybe_relisten_prn:
        bit     $DD0C
        bmi     L84EC ; centronics printer disabled
        pha
        lda     $9A
        cmp     #3
        beq     L84EB ; output to screen
        jsr     L8B19
L84EB:  pla
L84EC:  rts

; ----------------------------------------------------------------
; common code of RENUM/AUTO/DEL/ORDER/FIND/REPLACE

L84ED:  lda     auto_current_line_number
        ldy     auto_current_line_number + 1
        jsr     L8508
        ldy     #0
:       iny
        lda     $FF,y
        php
        ora     #$20
        sta     KEYD - 1,y
        plp
        bne     :-
        sty     NDX
        rts

L8508:  sta     $63
        sty     $62
        ldx     #$90
        sec
        jmp     _int_to_ascii

L8512:  jsr     _CHRGOT
        beq     L8528
        ldy     #0
        jsr     L858E
        beq     L8528
        cmp     #$2C
        bne     L852C
        jsr     _CHRGET
        jsr     L858E
L8528:  rts

; ??? unreferenced?
;        jmp     disable_rom_jmp_overflow_error

L852C:  jmp     WAF08 ; SYNTAX ERROR

L852F:  beq     L852C
L8531:  php
        ldy     #0
        jsr     L85A0
        pha
        jsr     _search_for_line
        pla
        ldx     $AC
        stx     $AE
        ldx     $AD
        stx     $AF
        plp
        bne     :+
        dec     $AF
        dec     $15
:       tax
        beq     :+
        cmp     #$3A
        beq     :+
        cmp     #$AB
        bne     L852C
        jsr     _CHRGET
        php
        ldy     #2
        jsr     L85A0
        bne     L852C
        plp
        bne     :+
        dec     $15
        dec     $AF
:       lda     $AE
        cmp     $AC
        lda     $AF
        sbc     $AD
        bcc     L852C
        lda     $5F
        sta     TXTPTR
        lda     $60
        sta     TXTPTR + 1
        jsr     _search_for_line
        bcc     :+
        ldy     #0
        jsr     _lda_5f_indy
        tax
        iny
        jsr     _lda_5f_indy
        sta     $60
        stx     $5F
:       rts

L858E:  jsr     _basic_string_to_word
        lda     $14
        sta     auto_current_line_number,y
        iny
        lda     $15
        sta     auto_current_line_number,y
        iny
        jmp     _CHRGOT

L85A0:  jsr     _basic_string_to_word
        ldx     $14
        stx     $AC,y
        ldx     $15
        stx     $AD,y
        jmp     _CHRGOT

L85AE:  lda     auto_current_line_number
        sta     $AC
        lda     auto_current_line_number + 1
        sta     $AD
        jmp     _set_txtptr_to_start

L85BB:  jsr     L85BF
        tay
L85BF:  inc     TXTPTR
        bne     :+
        inc     TXTPTR + 1
:       ldx     #0
        jsr     _lda_TXTPTR_indx
        rts

L85CB:  clc
        lda     $AC
        adc     auto_line_number_increment
        sta     $AC
        lda     $AD
        adc     auto_line_number_increment + 1
        sta     $AD
        bcs     :+
        cmp     #$FA
:       rts

L85DF:  jsr     L85BB
L85E2:  jsr     L85BF
        bne     L85E2
        rts

save_chrget_ptr:
        lda     TXTPTR
        sta     $5A
        lda     TXTPTR + 1
        sta     $5B
        rts

load_auto_defaults: ; for AUTO and RENUM
        ldx     #auto_defaults_end - auto_defaults - 1
:       lda     auto_defaults,x
        sta     auto_current_line_number,x
        dex
        bpl     :-
        rts

L85FD:
        .byte   $9B,$8A,$A7,$89,$8D,$CB
L85FD_end:

L8603:
        .byte   $AB,$A4,$2C
L8603_end:

auto_defaults:
        .word   100 ; default start line number for AUTO
        .word   10 ; default increment for AUTO
auto_defaults_end:


; ----------------------------------------------------------------

new_basic_keywords:
        .byte   "OF", 'F' + $80
        .byte   "AUT", 'O' + $80
        .byte   "DE", 'L' + $80
        .byte   "RENU", 'M' + $80
        .byte   "HEL", 'P' + $80
        .byte   "FIN", 'D' + $80
        .byte   "OL", 'D' + $80
        .byte   "DLOA", 'D' + $80
        .byte   "DVERIF", 'Y' + $80
        .byte   "DSAV", 'E' + $80
        .byte   "APPEN", 'D' + $80
        .byte   "DAPPEN", 'D' + $80
        .byte   "DO", 'S' + $80
        .byte   "KIL", 'L' + $80
        .byte   "MO", 'N' + $80
        .byte   "PDI", 'R' + $80
        .byte   "PLIS", 'T' + $80
        .byte   "BA", 'R' + $80
        .byte   "DESKTO", 'P' + $80
        .byte   "DUM", 'P' + $80
        .byte   "ARRA", 'Y' + $80
        .byte   "ME", 'M' + $80
        .byte   "TRAC", 'E' + $80
        .byte   "REPLAC", 'E' + $80
        .byte   "ORDE", 'R' + $80
        .byte   "PAC", 'K' + $80
        .byte   "UNPAC", 'K' + $80
        .byte   "MREA", 'D' + $80
        .byte   "MWRIT", 'E' + $80
        .byte 0

command_vectors:
        .word   OFF-1
        .word   AUTO-1
        .word   DEL-1
        .word   RENUM-1
        .word   HELP-1
        .word   FIND-1
        .word   OLD-1
        .word   DLOAD-1
        .word   DVERIFY-1
        .word   DSAVE-1
        .word   APPEND-1
        .word   DAPPEND-1
        .word   DOS-1
        .word   KILL-1
        .word   MON-1
        .word   PDIR-1
        .word   PLIST-1
        .word   BAR-1
        .word   DESKTOP-1
        .word   DUMP-1
        .word   ARRAY-1
        .word   MEM-1
        .word   TRACE-1
        .word   REPLACE-1
        .word   ORDER-1
        .word   PACK-1
        .word   UNPACK-1
        .word   MREAD-1
        .word   MWRITE-1

.ifdef JIFFY_TOKENIZER
jiffy_special_characters: .byte $40,$5f,$2a,$ac,$22,$12,$2f,$ad,$25,$5e,$ae,$27,$5c
jiffy_special_characters_size = .sizeof(jiffy_special_characters)
.endif

; ----------------------------------------------------------------
; "MREAD" Command - read 192 bytes from RAM into buffer
; ----------------------------------------------------------------
MREAD:  jsr     _get_int
        jsr     install_memcpy_code
        jmp     $0110

; ----------------------------------------------------------------
; "MWRITE" Command - write 192 bytes from buffer into RAM
; ----------------------------------------------------------------
MWRITE: jsr     _get_int
        jsr     install_memcpy_code
        lda     #$B2 ; switch source and dest
        sta     memcpy_selfmod1 - memcpy_code_at_0110 + 1 + $0110
        lda     #$14
        sta     memcpy_selfmod2 - memcpy_code_at_0110 + 1 + $0110
        sei
        jmp     $0110

install_memcpy_code:
        ldy     #memcpy_code_at_0110_end - memcpy_code_at_0110 - 1 + 6 ; XXX
L86EC:  lda     memcpy_code_at_0110,y
        sta     $0110,y
        dey
        bpl     L86EC
        ldy     #$C1
        sei
        rts

memcpy_code_at_0110:
        lda     #$34
        sta     $01
L86FD:  dey
memcpy_selfmod1:
        lda     ($14),y
memcpy_selfmod2:
        sta     ($B2),y
        cpy     #0
        bne     L86FD
        lda     #$37
        sta     $01
        cli
        rts
memcpy_code_at_0110_end:

; ----------------------------------------------------------------
; "DEL" Command - delete BASIC lines
; ----------------------------------------------------------------
DEL:    jsr     L852F
        ldy     #0
L8711:  jsr     _lda_5f_indy
        sta     (TXTPTR),y
        inc     $5F
        bne     L871C
        inc     $60
L871C:  jsr     L85BF
        lda     $5F
        cmp     $2D
        lda     $60
        sbc     $2E
        bcc     L8711
        lda     TXTPTR
        sta     $2D
        lda     TXTPTR + 1
        sta     $2E
        jmp     L897D

L8734:  jmp     disable_rom_jmp_overflow_error

L8737:  jmp     WAF08 ; SYNTAX ERROR

; ----------------------------------------------------------------
; "RENUM" Command - renumber BASIC lines
; ----------------------------------------------------------------
RENUM:  jsr     load_auto_defaults
        jsr     L8512
        beq     L8749
        cmp     #','
        bne     L8737
        jsr     _CHRGET
L8749:  jsr     L8531
        ldx     #3
L874E:  lda     $AC,x
        sta     $8B,x
        dex
        bpl     L874E
        jsr     L85AE
L8758:  jsr     L85BB
        beq     L8783
        jsr     L85BB
        jsr     L8FF9
        bcc     L877E
        lda     $AD
        sta     $15
        lda     $AC
        sta     $14
        jsr     L8FF9
        bcs     L8779
        jsr     _search_for_line
        bcc     L8779
        beq     L8734
L8779:  jsr     L85CB
        bcs     L8734
L877E:  jsr     L85DF
        beq     L8758
L8783:  jsr     L87B8
        jsr     L878C
        jmp     L8F7C

L878C:  jsr     L85AE
L878F:  jsr     L85BB
        beq     L87C0
        ldy     #2
        jsr     _lda_TXTPTR_indy
        pha
        dey
        jsr     _lda_TXTPTR_indy
        tay
        pla
        jsr     L8FF9
        bcc     L87B3
        ldy     #1
        lda     $AC
        sta     (TXTPTR),y
        iny
        lda     $AD
        sta     (TXTPTR),y
        jsr     L85CB
L87B3:  jsr     L85DF
        beq     L878F
L87B8:  jsr     L85AE
L87BB:  jsr     L85BB
        bne     L87C1
L87C0:  rts

L87C1:  jsr     L85BB
        lda     #$10
        sta     $C1
L87C8:  lda     #$10
        .byte   $2C
L87CB:  lda     #$20
        eor     $C1
        sta     $C1
L87D1:  jsr     _CHRGET
L87D4:  tax
        beq     L87BB
        cmp     #'"'
        beq     L87C8
        ldy     $C1
        bne     L87D1
        cmp     #$8F
        beq     L87CB
        ldx     #L85FD_end - L85FD - 1
L87E5:  cmp     L85FD,x
        beq     L87EF
        dex
        bpl     L87E5
        bmi     L87D1 ; always

L87EF:  jsr     save_chrget_ptr
        jsr     _CHRGET
L87F5:  ldx     #L8603_end - L8603 - 1
L87F7:  cmp     L8603,x
        beq     L87EF
        dex
        bpl     L87F7
        jsr     _CHRGOT
        bcs     L87D4
        jsr     _basic_string_to_word
        lda     $15
        ldy     $14
        jsr     L8FF9
        bcs     L881A
        jsr     L88B9
L8813:  jsr     _CHRGET
        bcc     L8813
        bcs     L87F5
L881A:  jsr     L85AE
L881D:  jsr     L85BB
        beq     L883A
        jsr     L85BB
        cmp     $15
        bne     L882D
        cpy     $14
        beq     L8840
L882D:  jsr     L8FF9
        bcc     L8835
        jsr     L85CB
L8835:  jsr     L85E2
        beq     L881D
L883A:  ldy     #$F9
        lda     #$FF
        bne     L8844
L8840:  ldy     $AD
        lda     $AC
L8844:  jsr     L8508
        jsr     L88B9
        ldx     #1
        stx     $AF
        dex
        stx     $AE
        jsr     _CHRGET
L8854:  inc     $AE
        jsr     _lda_ae_indx
        beq     L8873
        bcc     L8862
        ldy     #$FF
        jsr     L8882
L8862:  jsr     _lda_ae_indx
        sta     (TXTPTR,x)
        jsr     L85BF
        cmp     #$3A
        bcs     L8854
        jsr     $E3B3 ; clear carry if byte = "0"-"9" (CHRGET!)
        bpl     L8854
L8873:  jsr     _CHRGOT
        bcc     L887B
        jmp     L87F5

L887B:  ldy     #1
        jsr     L8882
        beq     L8873
L8882:  lda     #3
        sta     $15
        jsr     _lda_TXTPTR_indy
        bne     L888D
        inc     $15
L888D:  tax
        lda     TXTPTR
        pha
        lda     TXTPTR + 1
        pha
        txa
        ldx     #0
        iny
L8898:  sta     $14
        jsr     _lda_TXTPTR_indy
        pha
        lda     $14
        sta     (TXTPTR,x)
        beq     L88A8
        lda     #4
        sta     $15
L88A8:  jsr     L85BF
        pla
        dec     $15
        bne     L8898
        pla
        sta     TXTPTR + 1
        pla
        sta     TXTPTR
        ldx     #0
        rts

L88B9:  lda     $5A
        sta     TXTPTR
        lda     $5B
        sta     TXTPTR + 1
        ldx     #0
        rts

L88C4:  jmp     WAF08 ; SYNTAX ERROR

; ----------------------------------------------------------------
; "FIND" Command - find a string in a BASIC program
; ----------------------------------------------------------------
FIND:   ldy     #0
        sty     $C2
        eor     #$22
        bne     L88D4
        jsr     L85BF
        ldy     #$22
L88D4:  sty     $C1
        jsr     save_chrget_ptr
L88D9:  ldx     #0
        stx     $C4
        beq     L88EB
L88DF:  cmp     #$2C
        bne     L88E6
        tya
        beq     L88FD
L88E6:  jsr     L85BF
        inc     $C4
L88EB:  jsr     _lda_TXTPTR_indx
        beq     L8903
        cmp     #$22
        bne     L88DF
        jsr     _CHRGET
        beq     L8904
        cmp     #$2C
        bne     L88C4
L88FD:  jsr     _CHRGET
        jmp     L8904

L8903:  sec
L8904:  jsr     L8531
        jsr     _set_txtptr_to_start
        bit     $C2
        bmi     L8912
        lda     $C4
        sta     $C3
L8912:  jsr     L85BB
        beq     L896F
        jsr     L85BB
        sta     $3A
        sty     $39 ; line number lo
        cpy     $AC
        sbc     $AD
        bcc     L892E
        ldy     $AE
        cpy     $39 ; line number lo
        lda     $AF
        sbc     $3A
        bcs     L8933
L892E:  jsr     L85E2
        beq     L8912
L8933:  lda     $C1
        sta     $9F
L8937:  ldy     #0
        jsr     L85BF
        beq     L8912
        cmp     #$22
        bne     L8946
        eor     $9F
        sta     $9F
L8946:  lda     $9F
        bne     L8937
        ldx     $C3
L894C:  jsr     _lda_5a_indy
        sta     $02
        jsr     _lda_TXTPTR_indy
        cmp     $02
        bne     L8937
        iny
        dex
        bne     L894C
        jsr     _check_for_stop
        bit     $C2
        bpl     L8966
        jsr     L8F1F
L8966:  jsr     L839D
        bit     $C2
        bpl     L892E
        bmi     L8937
L896F:  bit     $C2
        bmi     L897D
        jmp     disable_rom_then_warm_start

; ----------------------------------------------------------------
; "OLD" Command - recover a deleted program
; ----------------------------------------------------------------
OLD:    bne     L89BC
        lda     #8
        sta     $0802
L897D:  jsr     L8986
L8980:  ldx     #$FC
        txs
        jmp     WA663_E386

L8986:  jsr     _relink
        clc
        lda     #2
        adc     $22
        sta     $2D
        lda     #0
        adc     $23
        sta     $2E
        rts

; ----------------------------------------------------------------
; "OFF" Command - disable BASIC extensions
; ----------------------------------------------------------------
OFF:    bne     L89BC
        sei
        jsr     $FD15
        jsr     $E453 ; assign $0300 BASIC vectors
        jsr     cond_init_load_save_vectors
        cli
        jmp     disable_rom_then_warm_start

; ----------------------------------------------------------------
; "KILL" Command - disable all cartridge functionality
; ----------------------------------------------------------------
KILL:   bne     L89BC
        sei
        jsr     $FD15
        jsr     $E453 ; assign $0300 BASIC vectors
        cli
        lda     #>$E385
        pha
        lda     #<$E385 ; BASIC warm start
        pha
        lda     #$F0 ; cartridge off
        jmp     _jmp_bank

L89BC:  rts

; ----------------------------------------------------------------
; "MON" Command - enter machine code monitor
; ----------------------------------------------------------------
MON:    bne     L89BC
        lda     #et_call
        jmp     monitor

; ----------------------------------------------------------------
; "BAR" Command - enable/disable pull-down menu
; ----------------------------------------------------------------
BAR:    tax
        lda     #0 ; bar off
        cpx     #$CC
        beq     L89CB ; OFF
        lda     #$80 ; bar on
L89CB:  sta     bar_flag
        jmp     WA8F8

; ----------------------------------------------------------------
; "DESKTOP" Command - start Desktop
; ----------------------------------------------------------------
DESKTOP:
        bne     L89BC
        ; Skip the ARE YOU SURE question if there is no program in memory.
        ldy     #0
        lda     ($2B),y  ; $2B = start of BASIC program ($0801)
        iny
        ora     ($2B),y
        beq     L89EC
        ldx     #a_are_you_sure - messages
        jsr     print_msg
L89D8:  lda     $DC00
        and     $DC01
        and     #$10
        beq     L89EC
        jsr     GETIN
        beq     L89D8
        cmp     #$59
        bne     _rts2
L89EC:  jmp     go_desktop

L89EF:  inx
        jsr     LE716 ; output character to the screen
print_msg:
        lda     a_are_you_sure,x
        bne     L89EF
_rts2:  rts

messages:
a_are_you_sure:
        .byte   "ARE YOU SURE (Y/N)?", CR, 0
a_ready: ; XXX this is only used by desktop_helper.s, it should be defined there
        .byte   CR,"READY.", CR, 0

; ----------------------------------------------------------------
; "DLOAD" Command - load a program from disk
; ----------------------------------------------------------------
DLOAD:
        lda     #0 ; load flag
        .byte   $2C
; ----------------------------------------------------------------
; "DVERIFY" Command - verify a program on disk
; ----------------------------------------------------------------
DVERIFY:
        lda     #1 ; verify flag
        sta     $0A
        jsr     set_filename_or_colon_asterisk
        jmp     WE16F

; ----------------------------------------------------------------
; "DSAVE" Command - save a program to disk
; ----------------------------------------------------------------
DSAVE:  jsr     set_filename_or_empty
        jmp     WE159

; ----------------------------------------------------------------
; "DAPPEND" Command - append a program from disk to program in RAM
; ----------------------------------------------------------------
DAPPEND:
        jsr     set_filename_or_colon_asterisk
        jmp     L8A35

; ----------------------------------------------------------------
; "APPEND" Command - append a program to program in RAM
; ----------------------------------------------------------------
APPEND: jsr     WE1D4
L8A35:  jsr     L8986
        lda     #0
        sta     SA
        ldx     $22
        ldy     $23
        jmp     WE175

; ----------------------------------------------------------------
; "DOS" Command - send command to drive
; ----------------------------------------------------------------
DOS:    cmp     #'"'
        beq     @5 ; DOS with a command
@3:     jsr     listen_6F_or_error
        jsr     UNLSTN
        jsr     command_channel_talk
        jsr     print_line_from_drive
@rts:   rts

@2:     ; carry is already clear
        adc     #$0A
@4:     and     #$0F
        sta     FA
        bne     @5
        jmp     L852C

@5:     jsr     _CHRGET
        beq     @3
        cmp     #'$'
        bne     @6
        jmp     DIR

        ; Accept device numbers 8..15
@6:     cmp     #'8'
        beq     @4
        cmp     #'9'
        beq     @4
        cmp     #'1'
        bne     @1
        jsr     _CHRGET
        beq     @3
        cmp     #'0'
        bcc     @1
        cmp     #'6'
        bcc     @2
@1:
        jsr     listen_6F_or_error
rts_ := @rts

send_drive_command:
        ldy     #0
        jsr     _lda_TXTPTR_indy
        cmp     #'D' ; drive command "D": change disk name
        beq     change_disk_name
        cmp     #'F' ; drive command "F": fast format
        bne     L8A84
        jsr     fast_format
L8A84:  jmp     L8BE3

; drive command "D": change disk name
change_disk_name:
        iny
        lda     (TXTPTR),y
        cmp     #':'
        bne     L8A84
        jsr     UNLSTN
        jsr     init_read_disk_name
        bne     rts_

L8A97:  lda     #$62
        jsr     listen_second
        ldy     #2
:       jsr     L8BDB
        beq     L8AB2
        cmp     #','
        beq     L8AB2
        jsr     IECOUT
        iny
        cpy     #$12
        bne     :-
        jsr     L8BDB
L8AB2:  pha
        tya
        pha
L8AB5:  cpy     #$12
        beq     L8AC1
        lda     #$A0
        jsr     IECOUT
        iny
        bne     L8AB5
L8AC1:  pla
        tay
        pla
        cmp     #','
        bne     L8ADF
        lda     #$A0
        jsr     IECOUT
        jsr     IECOUT
        iny
        ldx     #4
L8AD3:  jsr     L8BDB
        beq     L8ADF
        jsr     IECOUT
        iny
        dex
        bpl     L8AD3
L8ADF:  jsr     L8BF0
        jsr     init_write_bam
        jsr     cmd_channel_listen
        lda     #'I'
        jsr     IECOUT
        jmp     UNLSTN

; ----------------------------------------------------------------
; common code for PLIST/PDIR
; ----------------------------------------------------------------

get_secaddr_and_send_listen:
        cmp     #','
        bne     :+
        jsr     _CHRGET
        bcs     L8B3A ; SYNTAX ERROR
        jsr     _basic_string_to_word
        lda     $15
        bne     L8B3A ; must be < 256, otherwise SYNTAX ERROR
        lda     $14
        bpl     send_printer_listen ; 0-128 ok, everything else $FF
:       lda     #$FF
send_printer_listen:
        sta     SA
        jsr     cent_rs232_or_cbm
        bcc     set_dev_4 ; No listen required for either rs232 or centronics
        lda     SA
        bpl     :+
        lda     #$00
:       and     #$0F
        ora     #$60
        sta     SA
L8B19:  jsr     UNLSTN
        lda     #0
        sta     ST
        lda     #4
        jsr     LISTEN
        lda     SA
        bpl     L8B2E
        jsr     $EDBE ; set ATN
        bne     L8B31
L8B2E:  jsr     SECOND
L8B31:  lda     ST
        cmp     #$80
set_dev_4:
        lda     #4
        sta     $9A   ; Current output device
        rts

L8B3A:  jmp     WAF08 ; SYNTAX ERROR

; ----------------------------------------------------------------
; "PLIST" Command - send BASIC listing to printer
; ----------------------------------------------------------------
PLIST:  jsr     get_secaddr_and_send_listen
        bcs     L8B6D
        lda     $2B
        ldx     $2C
        sta     $5F
        stx     $60
        lda     #<_new_warmstart
        ldx     #>_new_warmstart
        jsr     L8B66 ; set $0300 vector, catch direct mode at "reset_warmstart"
        jmp     WA6C3

reset_warmstart:
        jsr     set_io_vectors
        lda     #CR
        jsr     BSOUT
        jsr     CLRCH
        jsr     set_io_vectors_with_hidden_rom
        lda     #<$E38B
        ldx     #>$E38B ; default value
L8B66:  sta     $0300
        stx     $0301 ; $0300 IERROR basic warm start
        rts

L8B6D:  lda     #3
        sta     $9A
        jmp     device_not_present

; ----------------------------------------------------------------
; "PDIR" Command - send disk directoy to printer
; ----------------------------------------------------------------
PDIR:   jsr     get_secaddr_and_send_listen
        bcs     L8B6D
DIR:    jsr     UNLSTN
        lda     #$F0
        jsr     listen_or_error
        lda     $9A
        cmp     #4
        bne     :+
        lda     #$24
        jsr     IECOUT
        jsr     UNLSTN
        jmp     L8B95

:       jsr     L8BE3
L8B95:  jsr     print_dir
        jsr     set_io_vectors
        jsr     CLRCH
        jmp     set_io_vectors_with_hidden_rom

; ----------------------------------------------------------------
; common code for DLOAD/DVERIDY/DSAVE/DOS
; ----------------------------------------------------------------

set_filename_or_colon_asterisk:
;        lda     #<(_a_colon_asterisk_end - _a_colon_asterisk); ":*" (XXX "<" required to make ca65 happy)
        lda     #1  ; * is string of length 1
        .byte   $2C
set_filename_or_empty:
        lda     #0 ; empty filename
;        jsr     set_filename
;        rts ; XXX omit jsr and rts

set_filename:
        jsr     set_colon_asterisk
        tax
        ldy     #1
        jsr     SETLFS
        jsr     $E206 ; RTS if end of line
        jmp     _get_filename

L8BDB:  jsr     _lda_TXTPTR_indy
        beq     :+
        cmp     #'"'
:       rts

L8BE3:  ldy     #0
:       jsr     L8BDB
        beq     L8BF0
        jsr     IECOUT
        iny
        bne     :-
L8BF0:  cmp     #'"'
        bne     :+
        iny
:       tya
        clc
        adc     TXTPTR
        sta     TXTPTR
        bcc     L8BFF
        inc     TXTPTR + 1
L8BFF:  jmp     UNLSTN

; ----------------------------------------------------------------
; Detokenize: Decode a BASIC token to a keyword
; ----------------------------------------------------------------
new_detokenize:
        tax
:       lda     $028D
        and     #2
        bne     :-   ; wait while CBM key is pressed
        txa
        jsr     do_detokenize
        jmp     _list_print_non_token_byte

do_detokenize:
        cmp     #$E9
        bcs     L8C5F ; token above
        cmp     #$80
        bcc     L8C59 ; below
        bit     $0F
        bmi     L8C55
        cmp     #$CC
        bcc     :+ ; standard C64 token
        sbc     #$4C
        ldx     #<new_basic_keywords
        stx     $22
        ldx     #>new_basic_keywords
        bne     L8C31
        ; Need 8K cartridge mode to make token table in BASIC ROM visible
:       ldx     #fcio_bank_0|fcio_c64_8kcrtmode|fcio_nmi_line
        stx     fcio_reg
        ldx     #<basic_keywords
        stx     $22
        ldx     #>basic_keywords
L8C31:  stx     $23
        tax
        sty     $49
        ldy     #0
        asl     a
        beq     L8C4B
L8C3B:  dex
        bpl     L8C4A
L8C3E:  inc     $22
        bne     :+
        inc     $23
:       lda     ($22),y
        bpl     L8C3E
        bmi     L8C3B ; Always
L8C4A:  ; Enough tokens skipped, write next token to display
        iny
L8C4B:
        ; Need 8K cartridge mode to make token table in BASIC ROM visible
        ; (again because _basic_bsout sets 16K mode)
        ldx     #fcio_bank_0|fcio_c64_8kcrtmode|fcio_nmi_line
        stx     fcio_reg
        lda     ($22),y
        bmi     L8C62
        jsr     _basic_bsout
        jmp     L8C4A

L8C55:  cmp     #CR + $80
        beq     L8C5D
L8C59:  cmp     #CR
        bne     L8C5F
L8C5D:  lda     #$1F
L8C5F:  inc     $D8
L8C61:  rts
L8C62:  ldy     $49
        and     #$7F
        bpl     L8C61 ; Always

; ----------------------------------------------------------------
; Keyboard handler and BAR support setup
; ----------------------------------------------------------------

.global uninstall_kbd_handler
uninstall_kbd_handler:
        jsr     L8C92
        lda     #<$EB48 ; evaluate modifier keys
        ldx     #>$EB48
        bne     L8C78 ; always
.global set_irq_and_kbd_handlers
set_irq_and_kbd_handlers:
        jsr     set_irq_handler
        lda     #<_kbd_handler
        ldx     #>_kbd_handler
L8C78:  sei
        sta     $028F ; set keyboard decode pointer
        stx     $0290
        lda     #0
        sta     $02A7
        sta     $02AB
        cli
        rts

set_irq_handler:
        lda     #<_bar_irq
        ldx     #>_bar_irq
        bit     bar_flag
        bmi     L8C96 ; bar on
L8C92:  lda     #<$EA31
        ldx     #>$EA31
L8C96:  sei
        sta     $0314
        stx     $0315
        cli
_rts:   rts

; ----------------------------------------------------------------
; "DUMP" Command - show list of all BASIC variables
; ----------------------------------------------------------------
DUMP:   bne     _rts
        lda     $2D
        ldy     $2E
@l:     sta     $5F
        sty     $60
        cpy     $30
        bne     :+
        cmp     $2F
:       bcs     _rts
        adc     #2
        bcc     :+
        iny
:       sta     $22
        sty     $23
        jsr     _check_for_stop
        jsr     print_var_name
        lda     #'='
        jsr     _basic_bsout
        txa
        bpl     :+
        jsr     load_int_into_fac1
        jmp     @1

:       tya
        bmi     :+
        jsr     _int_to_fac1
        jmp     @1
:       jsr     print_string
@1:     jsr     L83BA
        lda     $5F
        ldy     $60
        clc
        adc     #7
        bcc     @l
        iny
        bcs     @l

print_var_name:
        ldy     #0
        jsr     _lda_5f_indy
        tax
        and     #$7F
        jsr     _basic_bsout
        iny
        jsr     _lda_5f_indy
        tay
        and     #$7F
        beq     :+
        jsr     _basic_bsout
:       txa
        bmi     L8D07
        tya
        bmi     L8D0A
L8D06:  rts

L8D07:  lda     #'%'
        .byte   $2C
L8D0A:  lda     #'$'
        .byte   $2C
L8D0D:  lda     #$22 ; '"'
        jmp     _basic_bsout

load_int_into_fac1:
        ldy     #0
        jsr     _lda_22_indy
        tax
        iny
        jsr     _lda_22_indy
        tay
        txa
        jmp     _ay_to_fac1

print_string:
        jsr     L8D0D
        ldy     #0
        jsr     _lda_22_indy
        beq     L8D0D
        sta     $26
        iny
        jsr     _lda_22_indy
        tax
        iny
        jsr     _lda_22_indy
        stx     $22
        sta     $23
        ldy     #0
:       jsr     _lda_22_indy
        jsr     _basic_bsout
        iny
        cpy     $26
        bne     :-
        beq     L8D0D ; Always

; ----------------------------------------------------------------
; "ARRAY" Command - show list of all BASIC arrays
; ----------------------------------------------------------------
ARRAY:  bne     L8D06
        ldx     $30
        lda     $2F
L8D54:  sta     $5F
        stx     $60
        cpx     $32
        bne     L8D5E
        cmp     $31
L8D5E:  bcs     L8D06
        ldy     #4
        adc     #5
        bcc     L8D67
        inx
L8D67:  sta     $5A
        stx     $5B
        jsr     _check_for_stop
        jsr     _lda_5f_indy
        asl     a
        tay
        adc     $5A
        bcc     L8D78
        inx
L8D78:  sta     $C1
        stx     $C2
        dey
        sty     $C3
        lda     #0
L8D81:  sta     $0205,y
        dey
        bpl     L8D81
        bmi     L8DC5
L8D89:  ldy     $C3
L8D8B:  dey
        sty     $C4
        tya
        tax
        inc     $0206,x
        bne     L8D98
        inc     $0205,x
L8D98:  jsr     _lda_5a_indy
        sta     $02
        lda     $0205,y
        cmp     $02
        bne     L8DAF
        iny
        jsr     _lda_5a_indy
        sta     $02
        lda     $0205,y
        cmp     $02
L8DAF:  bcc     L8DC5
        lda     #0
        ldy     $C4
        sta     $0205,y
        sta     $0206,y
        dey
        bpl     L8D8B
        lda     $C1
        ldx     $C2
        jmp     L8D54

L8DC5:  jsr     print_var_name
        ldy     $C3
        lda     #'('
L8DCC:  jsr     _basic_bsout
        lda     $0204,y
        ldx     $0205,y
        sty     $C4
        jsr     _print_ax_int
        lda     #$2C
        ldy     $C4
        dey
        dey
        bpl     L8DCC
        lda     #')'
        jsr     _basic_bsout
        lda     #'='
        jsr     _basic_bsout
        lda     $C1
        ldx     $C2
        sta     $22
        stx     $23
        ldy     #0
        jsr     _lda_5f_indy
        bpl     :+
        jsr     load_int_into_fac1
        lda     #2
        bne     L8E14
:       iny
        jsr     _lda_5f_indy
        bmi     L8E0F
        jsr     _int_to_fac1
        lda     #5
        bne     L8E14
L8E0F:  jsr     print_string
        lda     #3
L8E14:  clc
        adc     $C1
        sta     $C1
        bcc     :+
        inc     $C2
:       jsr     L83BA
        jmp     L8D89

L8E23:  rts

; ----------------------------------------------------------------
; "MEM" Command - display memory usage
; ----------------------------------------------------------------
MEM:    bne     L8E23 ; rts
        ldy     #s_basic - s_basic
        lda     #$0C
        ldx     #$00
        jsr     print_string_and_int
        ldy     #s_program - s_basic
        lda     #$02
        ldx     #$00
        jsr     print_string_and_int
        ldy     #s_variables - s_basic
        lda     #$04
        ldx     #$02
        jsr     print_string_and_int
        ldy     #s_arrays - s_basic
        lda     #$06
        ldx     #$04
        jsr     print_string_and_int
        ldy     #s_strings - s_basic
        lda     #$0C
        ldx     #$08
        jsr     print_string_and_int
        ldy     #s_free - s_basic
        lda     #$08
        ldx     #$06
print_string_and_int:
        pha
        jsr     print_mem_string
        pla
        tay
        lda     $2B,y
        sec
        sbc     $2B,x
        sta     $C1
        lda     $2C,y
        sbc     $2C,x
        ldx     $C1
        ldy     #10 ; column of next character
        sty     PNTR
        jsr     _print_ax_int ; print number of bytes
        ldy     #16 ; column of next character
        sty     PNTR
        ldy     #s_bytes - s_basic ; print "BYTES"
print_mem_string:
        lda     s_basic,y
        php
        and     #$7F
        jsr     _basic_bsout
        iny
        plp
        bpl     print_mem_string
        rts

s_basic:
        .byte   CR, "BASI",'C'+$80
s_program:
        .byte   "PROGRA", 'M'+$80
s_variables:
        .byte   "VARIABLE", 'S'+$80
s_arrays:
        .byte   "ARRAY", 'S'+$80
s_strings:
        .byte   "STRING", 'S'+$80
s_free:
        .byte   "FRE", 'E'+$80
s_bytes: .byte   "BYTES", CR+$80

; ----------------------------------------------------------------
; "TRACE" Command - enable/disable printing each BASIC line executed
; ----------------------------------------------------------------
TRACE:  tax
        lda     trace_flag
        cpx     #$CC
        beq     :+ ; OFF
        ora     #1
        .byte   $2C
:       and     #$FE
        sta     trace_flag
        jmp     WA8F8

; ----------------------------------------------------------------
; "REPLACE" Command - replace a string in a BASIC program
; ----------------------------------------------------------------
L8ECE:  jmp     L852C

REPLACE:
        ldy     #0
        eor     #$22
        bne     L8EDC
        jsr     L85BF
        ldy     #$22
L8EDC:  sty     $C1
        jsr     save_chrget_ptr
        ldx     #0
        stx     $C3
        beq     L8EF3
L8EE7:  cmp     #$2C
        bne     L8EEE
        tya
        beq     L8F03
L8EEE:  jsr     L85BF
        inc     $C3
L8EF3:  jsr     _lda_TXTPTR_indx
        beq     L8ECE
        cmp     #$22
        bne     L8EE7
        jsr     _CHRGET
        cmp     #$2C
        bne     L8ECE
L8F03:  tya
        beq     L8F0D
        jsr     _CHRGET
        cmp     #$22
        bne     L8ECE
L8F0D:  jsr     _CHRGET
        lda     TXTPTR
        sta     $8B
        lda     TXTPTR + 1
        sta     $8C
        lda     #$80
        sta     $C2
        jmp     L88D9

L8F1F:  lda     $C3
        ldy     #1
        sec
        sbc     $C4
        beq     L8F41
        bcs     L8F31
        eor     #$FF
        adc     #1
        clc
        ldy     #$FF
L8F31:  sty     $60
        sta     $61
L8F35:  ldy     $60
        jsr     L8F65
        dec     $61
        bne     L8F35
        jsr     _relink
L8F41:  ldy     #0
        ldx     $C4
        beq     L8F5C
L8F47:  jsr     _lda_8b_indy
        sta     (TXTPTR),y
        iny
        dex
        bne     L8F47
        dey
        tya
        clc
        adc     TXTPTR
        sta     TXTPTR
        bcc     L8F5B
        inc     TXTPTR + 1
L8F5B:  rts

L8F5C:  lda     TXTPTR
        bne     L8F62
        dec     TXTPTR + 1
L8F62:  dec     TXTPTR
L8F64:  rts

L8F65:  lda     #3
        sta     $15
        jsr     _lda_TXTPTR_indy
        bne     L8F77
        cpy     #$FF
        beq     L8F75
        inc     $15
        .byte   $2C
L8F75:  lda     #1
L8F77:  jmp     L888D

; ----------------------------------------------------------------
; "ORDER" Command - reorder BASIC lines after APPEND
; ----------------------------------------------------------------
ORDER:  bne     L8F64
L8F7C:  jsr     _relink
        jsr     _set_txtptr_to_start
        lda     #0
        lda     $8B
        sta     $8C
L8F88:  jsr     L85BB
        beq     L8FF6
        jsr     L85BB
        sta     $15
        sty     $14
        cpy     $8B
        pha
        sbc     $8C
        pla
        bcs     L8FEC
        jsr     _search_for_line
        lda     $5F
        sta     $8D
        lda     $60
        sta     $8E
        sec
        lda     TXTPTR
        sbc     #3
        sta     $5A
        lda     TXTPTR + 1
        sbc     #0
        sta     $5B
        ldy     #0
L8FB6:  jsr     _lda_5a_indy
        sta     $033C,y
        iny
        cpy     #5
        bcc     L8FB6
        cmp     #0
        bne     L8FB6
        sty     $8F
        tya
        clc
        adc     $5A
        sta     $58
        lda     $5B
        adc     #0
        sta     $59
        jsr     WA3BF
        ldy     #0
L8FD8:  lda     $033C,y
        sta     ($8D),y
        iny
        cpy     $8F
        bne     L8FD8
        jsr     _relink
        ldx     #0
        stx     $033C
        beq     L8FF0
L8FEC:  sta     $8C
        sty     $8B
L8FF0:  jsr     L85E2
        jmp     L8F88

L8FF6:  jmp     L897D

L8FF9:  sty     $39 ; line number lo
        sta     $3A
        cpy     $8B
        sbc     $8C
        bcc     L900B
        lda     $8D
        cmp     $39 ; line number lo
        lda     $8E
        sbc     $3A
L900B:  rts

; ----------------------------------------------------------------
; "UNPACK" Command - decompress a program
; ----------------------------------------------------------------
.import __unpack_header_LOAD__
.import __unpack_header_RUN__
UNPACK: bne     L900B
        ldx     #$11 ; arbitrary length
L9010:  lda     __unpack_header_LOAD__,x
        cmp     __unpack_header_RUN__,x
        bne     L900B ; do nothing if not packed
        dex
        bpl     L9010
        ldx     #alt_pack_run_end - alt_pack_run - 1
L901D:  lda     alt_pack_run,x
        sta     pack_run,x
        dex
        bpl     L901D
        lda     #>(unpack_entry - 1)
        pha
        lda     #<(unpack_entry - 1)
        pha
        jmp     _disable_fc3rom

alt_pack_run:
        jsr     $A663 ; CLR
        jmp     $E386 ; BASIC warm start
alt_pack_run_end:

L9035:  jmp     L8734

; ----------------------------------------------------------------
; "PACK" Command - compress a program
; ----------------------------------------------------------------
.import __pack_code_LOAD__
.import __pack_code_RUN__
PACK:   bne     L900B
        lda     $2B
        cmp     $2D
        lda     $2C
        sbc     $2E
        bcs     L9035
        lda     $2E
        cmp     #$FE
        bcs     L9035
        ldx     #$FE
        txs
.ifdef use_ill
        lax     #0
.else
        lda     #0
        tax
.endif
L9050:  sta     $FE00,x
        sta     $FF00,x
        inx
        bne     L9050
        sty     $AE
        sty     $AC
        sty     $AD
        lda     $2C
        sta     $AF
        ldy     $2B
        ldx     #0
L9067:  lda     __pack_code_LOAD__,x
        sta     __pack_code_RUN__,x
        inx
        cpx     #pack_code_end - pack_code
        bne     L9067
        sei
        lda     #$34
        jsr     pack_code
        ldy     #0
L907A:  lda     __unpack_header_LOAD__,y
        sta     __unpack_header_RUN__,y
        iny
        cpy     #unpack_header_end - unpack_header
        bne     L907A
        lda     $FF
        sta     $0848
        sta     $087E
        lda     $2B
        sta     $084C
        sta     $0892
        lda     $2C
        sta     $084D
        sta     $0893
        lda     $2D
        sta     $085B
        lda     $2E
        sta     $0861
        lda     $AE
        clc
        adc     #1
        sta     $2D
        lda     $AF
        adc     #0
        sta     $2E
        sec
        lda     #<pack_data
        sbc     $2D
        sta     $0813
        lda     #>pack_data
        sbc     $2E
        sta     $0817
        jmp     L8980

.segment "pack_code"

; this lives at $0100
pack_code:
        sta     $01
L90C8:  lda     ($AE),y
        tax
        inc     $FE00,x
        bne     L90D3
        inc     $FF00,x
L90D3:  iny
        bne     L90D8
        inc     $AF
L90D8:  cpy     $2D
        lda     $AF
        sbc     $2E
        bcc     L90C8
        ldx     #0
        ldy     #1
L90E4:  lda     $FF00,x
        cmp     $FF00,y
        bcc     L90F8
        bne     L90F6
        lda     $FE00,x
        cmp     $FE00,y
        bcc     L90F8
L90F6:  tya
        tax
L90F8:  iny
        bne     L90E4
        stx     $FF
        lda     $2D
        sta     $AE
        lda     $2E
        sta     $AF
L9105:  lda     $AC
        bne     L910B
        dec     $AD
L910B:  dec     $AC
        lda     $AE
        bne     L9113
        dec     $AF
L9113:  dec     $AE
        lda     ($AE),y
        sta     ($AC),y
        lda     $2B
        cmp     $AE
        lda     $2C
        sbc     $AF
        bcc     L9105
        lda     #<pack_data
        sta     $AE
        lda     #>pack_data
        sta     $AF
        jsr     L01B8
L912E:  sta     ($AE),y
        cmp     $FF
        beq     L9169
L9134:  cpx     #0
        beq     L9179
        jsr     L01B8
        cpx     #0
        beq     L9143
        cmp     ($AE),y
        beq     L9153
L9143:  cpy     #4
        bcs     L9159
L9147:  inc     $AE
        bne     L914D
        inc     $AF
L914D:  dey
        bpl     L9147
        iny
        beq     L912E
L9153:  iny
        sta     ($AE),y
        bne     L9134
        dey
L9159:  pha
        tya
        ldy     #1
        sta     ($AE),y
        dey
        lda     $FF
        sta     ($AE),y
        pla
        ldy     #2
        bne     L9147
L9169:  iny
        lda     #0
        sta     ($AE),y
        cpx     #0
        beq     L9175
        jsr     L01B8
L9175:  ldy     #1
        bne     L9147
L9179:  lda     #$37
        sta     $01
        rts

L01B8:
        ldx     #0
        lda     ($AC,x)
        inc     $AC
        bne     L9188
        inc     $AD
L9188:  ldx     $AD
        rts
pack_code_end:

.segment "unpack_header"

unpack_header:
        .word   pack_link ; BASIC link pointer
        .word   1987 ; line number
        .byte   $9E ; SYS token
        ; decimal ASCII representation of "unpack_entry" :)
        .byte   <(((unpack_entry /  1000) .mod 10) + '0')
        .byte   <(((unpack_entry /   100) .mod 10) + '0')
        .byte   <(((unpack_entry /    10) .mod 10) + '0')
        .byte   <(((unpack_entry /     1) .mod 10) + '0')
        .byte   0 ; BASIC line end marker
pack_link:
        .word 0 ; BASIC link pointer
; decompression
unpack_entry:
        sei
        lda     #$34
        sta     $01
        lda     #0
        sta     $AE
        lda     #0
        sta     $AF
L91A4:  dec     $2E
        dec     pack_selfmod + 2
        ldy     #0
L91AB:  lda     ($2D),y
pack_selfmod:
        sta     $0000,y
        dey
        bne     L91AB
        lda     $2E
        cmp     #7
        bne     L91A4
        ldx     #stack_code_end - stack_code - 1
        txs
L91BC:  lda     stack_code,x; copy to $0100
        pha
        dex
        bpl     L91BC
        txs
        jmp     $0100

; this lives at $0100
; it's double copied:
; * PACK copies the whole unpacker from ROM to $0801
; * when running it, it copies the core to $0100
; cl65 can't deal with the double copying, so we need to
; adjust addresses manually
stack_code:
        ldx     #0
L91C9:  lda     ($AE),y
L91CB:  inc     $AE
        bne     L91D1
        inc     $AF
L91D1:  cmp     #0
        beq     L91FB
stack_selfmod1:
        sta     $1000,x
        inx
        bne     L91DE
        inc     stack_selfmod1 - stack_code + 2 + $0100
L91DE:  lda     $AE
        ora     $AF
        bne     L91C9
        lda     #0
        sta     $2D
        sta     $AE
        lda     #0
        sta     $2E
        sta     $AF
        lda     #$37
        sta     $01
        cli
pack_run:
        jsr     $A659 ; CLR
        jmp     $A7AE ; next statement

L91FB:  lda     ($AE),y
        inc     $AE
        bne     L9203
        inc     $AF
L9203:  cmp     #0
        bne     L920B
        lda     #0
        bne     stack_selfmod1
L920B:  sta     $FF
        lda     ($AE),y
        ldy     stack_selfmod1 - stack_code + 2 + $0100
        sty     stack_selfmod2 - stack_code + 2 + $0100
        ldy     $FF
        iny
L9218:  dey
        beq     L91CB
stack_selfmod2:
        sta     $1000,x
        inx
        bne     L9218
        inc     stack_selfmod1 - stack_code + 2 + $0100
        inc     $0156
        bne     L9218
stack_code_end:
unpack_header_end:
pack_data:

; ----------------------------------------------------------------

.segment "basic_vectors"

; these have to be at $A000
        .addr   go_basic          ; BASIC cold start entry point
        .addr   _basic_warm_start ; BASIC warm start entry point

; ----------------------------------------------------------------


.ifdef INTERNAL_KEYWORDS
.segment "basic_keywords"

; This is a redundant copy of the BASIC keywords in ROM.
; They are probably here for speed reasons, so the tokenizer doesn't
; have to switch banks.
basic_keywords:
        .byte   "EN", 'D' + $80
        .byte   "FO", 'R' + $80
        .byte   "NEX", 'T' + $80
        .byte   "DAT", 'A' + $80
        .byte   "INPUT", '#' + $80
        .byte   "INPU", 'T' + $80
        .byte   "DI", 'M' + $80
        .byte   "REA", 'D' + $80
        .byte   "LE", 'T' + $80
        .byte   "GOT", 'O' + $80
        .byte   "RU", 'N' + $80
        .byte   "I", 'F' + $80
        .byte   "RESTOR", 'E' + $80
        .byte   "GOSU", 'B' + $80
        .byte   "RETUR", 'N' + $80
        .byte   "RE", 'M' + $80
        .byte   "STO", 'P' + $80
        .byte   "O", 'N' + $80
        .byte   "WAI", 'T' + $80
        .byte   "LOA", 'D' + $80
        .byte   "SAV", 'E' + $80
        .byte   "VERIF", 'Y' + $80
        .byte   "DE", 'F' + $80
        .byte   "POK", 'E' + $80
        .byte   "PRINT", '#' + $80
        .byte   "PRIN", 'T' + $80
        .byte   "CON", 'T' + $80
        .byte   "LIS", 'T' + $80
        .byte   "CL", 'R' + $80
        .byte   "CM", 'D' + $80
        .byte   "SY", 'S' + $80
        .byte   "OPE", 'N' + $80
        .byte   "CLOS", 'E' + $80
        .byte   "GE", 'T' + $80
        .byte   "NE", 'W' + $80
        .byte   "TAB", '(' + $80
        .byte   "T", 'O' + $80
        .byte   "F", 'N' + $80
        .byte   "SPC", '(' + $80
        .byte   "THE", 'N' + $80
        .byte   "NO", 'T' + $80
        .byte   "STE", 'P' + $80
        .byte   '+' + $80
        .byte   '-' + $80
        .byte   '*' + $80
        .byte   '/' + $80
        .byte   '^' + $80
        .byte   "AN", 'D' + $80
        .byte   "O", 'R' + $80
        .byte   '>' + $80
        .byte   '=' + $80
        .byte   '<' + $80
        .byte   "SG", 'N' + $80
        .byte   "IN", 'T' + $80
        .byte   "AB", 'S' + $80
        .byte   "US", 'R' + $80
        .byte   "FR", 'E' + $80
        .byte   "PO", 'S' + $80
        .byte   "SQ", 'R' + $80
        .byte   "RN", 'D' + $80
        .byte   "LO", 'G' + $80
        .byte   "EX", 'P' + $80
        .byte   "CO", 'S' + $80
        .byte   "SI", 'N' + $80
        .byte   "TA", 'N' + $80
        .byte   "AT", 'N' + $80
        .byte   "PEE", 'K' + $80
        .byte   "LE", 'N' + $80
        .byte   "STR", '$' + $80
        .byte   "VA", 'L' + $80
        .byte   "AS", 'C' + $80
        .byte   "CHR", '$' + $80
        .byte   "LEFT", '$' + $80
        .byte   "RIGHT", '$' + $80
        .byte   "MID", '$' + $80
        .byte   "G", 'O' + $80
        .byte   0

.else
basic_keywords = $A09E
.endif
