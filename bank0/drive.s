; ----------------------------------------------------------------
; Common drive code
; ----------------------------------------------------------------
; The BASIC extension and fast format call into this.

.include "../core/kernal.i"

; from wrapper
.import disable_rom_jmp_error

; from basic
.import set_drive

.global print_line_from_drive
.global check_iec_error
.global cmd_channel_listen
.global listen_second
.global command_channel_talk
.global talk_second
.global m_w_and_m_e
.global listen_6F_or_error
.global listen_or_error
.global device_not_present

.segment "drive"

print_line_from_drive:
        jsr     IECIN
        jsr     $E716 ; output character to the screen
        cmp     #CR
        bne     print_line_from_drive
        jmp     UNTALK

check_iec_error:
        jsr     command_channel_talk
        jsr     IECIN
        tay
:       jsr     IECIN
        cmp     #CR ; skip message
        bne     :-
        jsr     UNTALK
        ldx     #0
        cpy     #'0'
        rts

cmd_channel_listen:
        lda     #$6F
listen_second:
        pha
        jsr     set_drive
        jsr     LISTEN
        pla
        jsr     SECOND
        lda     ST
        rts

command_channel_talk:
        lda     #$6F
talk_second:
        pha
        jsr     set_drive
        jsr     TALK
        pla
        jmp     TKSA

.global transfer_code_to_drive
transfer_code_to_drive:
        sta     $C3
        sty     $C4
        ldy     #0
@1:     lda     #'W'
        jsr     send_m_dash ; send "M-W"
        tya
        jsr     IECOUT
        txa
        jsr     IECOUT
        lda     #' '
        jsr     IECOUT
:       lda     ($C3),y
        jsr     IECOUT
        iny
        tya
        and     #$1F  ; 32 bytes sent?
        bne     :-    ; if not, next byte
        jsr     UNLSTN
        dec     $93     ; decrease number of 32byte chunks to send
        beq     @ready  ; if zero we are ready
        tya
        bne     @1
        inc     $C4
        inx
        bne     @1   ; always taken
@ready: lda     #'E' ; send "M-E"

send_m_dash:
        pha
        jsr     listen_6F_or_error
        lda     #'M'
        jsr     IECOUT
        lda     #'-'
        jsr     IECOUT
        pla
        jmp     IECOUT

listen_6F_or_error:
        lda     #$6F
listen_or_error:
        jsr     listen_second
        bmi     device_not_present
        rts

device_not_present:
        ldx     #5 ; "DEVICE NOT PRESENT"
        jmp     disable_rom_jmp_error
