; ----------------------------------------------------------------
; Common drive code
; ----------------------------------------------------------------
; The BASIC extension and fast format call into this.

.include "../core/kernal.i"

; from wrapper
.import disable_rom_jmp_error

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
.global init_read_disk_name
.global init_write_bam
.global close_ch2
.global set_colon_asterisk
.global set_drive
.global digit_to_ascii
.global byte_to_hex_ascii

.segment "drive"

print_line_from_drive:
        jsr     IECIN
        jsr     LE716 ; output character to the screen
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


init_read_disk_name:
        lda     #$F2
        jsr     listen_second
        lda     #'#'
        jsr     IECOUT
        jsr     UNLSTN
        ldy     #drive_cmd_u1 - drive_cmds
        jsr     send_drive_cmd ; send "U1:2 0 18 0", block read of BAM
        jsr     check_iec_error
        bne     close_ch2 ; error
        ldy     #drive_cmd_bp - drive_cmds
        jsr     send_drive_cmd ; send "B-P 2 144", read name
        lda     #0
        rts

init_write_bam:
        ldy     #drive_cmd_u2 - drive_cmds
        jsr     send_drive_cmd ; send "U2:2 0 18 0", block write of BAM
close_ch2:
        lda     #$E2
        jsr     listen_second
        jsr     UNLSTN
        lda     #1
        rts

send_drive_cmd:
        jsr     cmd_channel_listen
:       lda     drive_cmds,y
        beq     @done
        jsr     IECOUT
        iny
        bne     :-
@done:  jmp     UNLSTN

set_colon_asterisk:
;        ldx     #<_a_colon_asterisk
;        ldy     #>_a_colon_asterisk
        ; Make DLOAD, DAPPEND etc. use * rather than :*. It should be identical, but avoids a bug in the
        ; code that loads a backup than doesn't like it when :* is used as the file name to load the
        ; backup. There are multiple * in the KERNAL, no alternative KERNAL will dare to change one at
        ; $FFE5 (hopefully).
        ldx     #<$FFE5
        ldy     #>$FFE5
        jsr     SETNAM
set_drive:
        lda     #0
        sta     ST
        lda     #8
        cmp     FA
        bcc     @hidev ; device number 9 or above
@store: sta     FA
@rts:   rts
@hidev: lda     FA
        cmp     #16
        bcc     @rts
        lda     #8 ; set drive 8
        bne     @store ; always

; convert byte into hex ASCII in A/Y
byte_to_hex_ascii:
        pha
        and     #$0F
        jsr     digit_to_ascii
        tay
        pla
        lsr     a
        lsr     a
        lsr     a
        lsr     a
digit_to_ascii:
        sed
        cmp #10
        adc #$30
        cld
        rts

drive_cmds:
drive_cmd_u1:
        .byte   "U1:2 0 18 0", 0
drive_cmd_bp:
        .byte   "B-P 2 144", 0
drive_cmd_u2:
        .byte   "U2:2 0 18 0", 0
