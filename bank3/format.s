; ----------------------------------------------------------------
; Fast Format
; ----------------------------------------------------------------

.include "../core/kernal.i"
.include "persistent.i"

; from drive
.import check_iec_error
.import cmd_channel_listen
.import listen_second
.import read_drive_identification2
.import read_drive_status
.import install_cmds_ram
.import send_partinfo
;.import send_partinfo
;.import transfer_code_to_drive
.import __diredit_cmds_LOAD__,__diredit_cmds_RUN__
.import sd2iec_createimg

.global fast_format

.segment "fast_format"

.import __fast_format_drive_LOAD__
.import __fast_format_drive_RUN__

fast_format:
        ;
        ; At this point the drive is already listening on channel 15, waiting
        ; a command. The caller of this routine has already scanned the first
        ; char of the DOS command, and return with the drive still listening
        ; it will send this can and then send the rest of the command.
        ;
        ; This fast format code is incompatible with JiffyDOS modded drives,
        ; and won't work on non-1541/1581 drives either.
        ;
        ; We will detect the drive type. If it is a 1541 or 1571 without ROM
        ; mod, we will proceed with fast format. We will send a command like
        ; M-Elh:DISKNAME,01
        ; I.e. the disk name and ID are part of the M-E command.
        ;
        ; In case of rom mods or entirely different devices, we will send N
        ; to proceed with a normal format.
        ;
        ; A 1571 will return to 1 MHz mode due to the UI command, so 2MHz
        ; mode does not need to be taken into account. The normal command
        ; N to format a dual sided disk in 2MHz mode.
        ;
        jsr     read_drive_identification2
        lda     $02C3 ; 'C'
        eor     $02C7 ; 'D'
        eor     #$07  ; 'C' eor 'D'
        bne     @nocbm
        lda     $02D2
        cmp     #'4'
        beq     @1541
        cmp     #'7'
        beq     @1541
        bne     @no1541

        ;
        ; Test for SD2IEC
        ;
@nocbm:
        lda     $02C3
        eor     $02C5
        eor     #'S' ^ '2'
        bne     @no1541

        ;
        ; If we are dealing with an SD2IEC, we will create a new image
        ; if no image is mounted.
        ;
        jsr     send_partinfo

        jsr     read_drive_status
        ldx     #0
        lda     $02C0
        cmp     #1
        bne     @no1541

        ; Create image
        jsr     sd2iec_createimg
        jsr     start_format_cmd
        ; Skip file extension
        ldy     #0
:       jsr     lda_txtptr_indy
        cmp     #'.'
        beq     @s
        jsr     out_inc_txtptr
        bne     :- ; always
@s:     jsr     inc_txtptr
        jsr     lda_txtptr_indy
        cmp     #','
        beq     @r
        cmp     #0
        beq     @r
        bne     @s

@no1541:
        jsr     start_format_cmd
:       bne     @r ; always
@1541:
        lda     #8
        sta     $93 ; times $20 bytes
        lda     #<__fast_format_drive_LOAD__
        ldy     #>__fast_format_drive_LOAD__
        ldx     #>__fast_format_drive_RUN__
        jsr     transfer_code_to_drive
        lda     #<fast_format_drive_code_entry
        jsr     IECOUT
        lda     #>fast_format_drive_code_entry
        jsr     IECOUT
@r:     lda     #$40
        jmp     _jmp_bank


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
        lda     #$6F
        jsr     listen_second
        lda     #'M'
        jsr     IECOUT
        lda     #'-'
        jsr     IECOUT
        pla
        jmp     IECOUT

start_format_cmd:
        lda     #$6F                         ; Listen channel 15
        jsr     listen_second
        lda     #'N'
        jsr     out_inc_txtptr
        rts

; ----------------------------------------------------------------

.segment "fast_format_drive"

ram_code := $0630

; this lives at $0400
fast_format_drive_code:
        ; Drive controller jumps here when we execute the buffer
        jmp perform_buffer_code

fast_format_drive_code_entry:
        jsr     $C1E5 ; Search for : in command string
        bne     :+
        jmp     $C1F3 ; 34, SYNTAX ERROR
:       sty     $027A
        lda     #$A0
        jsr     $C268  ; Search for char
        jsr     $C100  ; Turn led on
        ldy     $027B  ; Position of ID in command string
        cpy     $0274  ; Check for end of command
        bne     :+
        jmp     $EE46  ; Do format in ROM

:       lda     $0200,y
        sta     $12
        lda     $0201,y
        sta     $13

        ; Copy some ROM code to RAM
        ldx     #$78
:       lda     $FC36 - 1,x
        sta     ram_code - 1,x
        dex
        bne     :-
        ; Patch it to become a subroutine
        lda     #$60 ; add RTS at the end
        sta     ram_code + $78
        lda     #1
        sta     $80 ; Track for operation
        sta     $51 ; Track during format
        jsr     $D6D3 ; Set track and sector for buffer

        lda     $22    ; Current track number
        bne     :+
        lda     #$C0   ; Move head to track 0 (machinegun sound)
        jsr     do_buffer1_cmd
:       lda     #$E0   ; Exec buffer command
        jsr     do_buffer1_cmd
        cmp     #2
        bcc     :+
        jmp     $C8E8
:       jmp     $EE40 ; create a new BAM


perform_buffer_code:
        lda     $51   ; Track during format
        cmp     ($32),y ; Track correct?
        beq     :+
        sta     ($32),y
        jmp     $F99C ; motor and stepper control

        ; Determine track zone
:       ldx     #4
:       cmp     $FED7,x ; Control bytes for head position
        beq     :+
        dex
        bcs     :-
        bcc     @6
        ; Zone OK
:       jsr     $FE0E ; Track erase: Write 10240 times $55 to diskette
        ; Write 5 times $FF to diskette (sync)
        lda     #$FF
        sta     $1C01
:       bvc     :-
        clv
        inx
        cpx     #5

        bcc     :-
        jsr     $FE00 ; Disk controller in read mode
:       lda     $1C00 ; Bit 7: SYNC detect
        bpl     @2 ; Sync? Then jump
        bvc     :-
        clv
        inx
        bne     :-
        iny
        bpl     :-    ; Loop till sync

@3:     lda     #3
        jmp     $FDD3  ; decrease error counter and make other attempt in ROM

@2:     sty     $C0
        stx     $C1
        ldx     $43
        ldy     #0
        tya
@1:     clc
        adc     #$64
        bcc     :+
        iny
:       iny
        dex
        bne     @1
        eor     #$FF
        sec
        adc     $C1
        bcs     :+
        dec     $C0
:       tax
        tya
        eor     #$FF
        sec
        adc     $C0
        bcc     @3
        tay
        txa
        ldx     #0
@4:     sec
        sbc     $43
        bcs     :+
        dey
        bmi     @5
:       inx
        bne     @4
@5:     stx     $0626 ; Used in ram_code
        cpx     #4
        bcc     @3
@6:     jsr     ram_code
        lda     $1C0C
        and     #$1F
        ora     #$C0
        sta     $1C0C
        dec     $1C03
        ldx     #$55
        stx     $1C01
:       bvc     :-
        inx
        bne     :-
        jmp     $FCB1


;
; Give a command to buffer 1 ($0400..$04FF)
;
do_buffer1_cmd:
        sta     $01
:       lda     $01
        bmi     :-
        rts

