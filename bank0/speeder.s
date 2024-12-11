; ----------------------------------------------------------------
; Disk and Tape Speeder
; ----------------------------------------------------------------
; This speeds up LOAD and SAVE on both disk and tape

.include "../core/kernal.i"
.include "../core/fc3ioreg.i"
.include "persistent.i"

.global new_load
.global new_save
.global tape_write_byte
.import check_iec_error
.import transfer_code_to_drive

L0110           := $0110

.segment "speeder_serial"

send_byte:
        pha
@1:     bit     $DD00  ; Wait until DATA IN high
        bpl     @1
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        tax
@2:     lda     $D012
        cmp     #$31
        bcc     @3
        and     #$06
        cmp     #$02
        beq     @2
@3:     lda     #$07
        sta     $DD00
        lda     iec_tab,x
        nop
        nop
        sta     $DD00
        lsr     a
        lsr     a
        and     #$F7
        sta     $DD00
        pla
        and     #$0F
        tax
        lda     iec_tab,x
        sta     $DD00
        lsr     a
        lsr     a
        and     #$F7
        sta     $DD00
        lda     #$17
        nop
        nop
        sta     $DD00
.assert >* = >send_byte, error, "Page boundary!"
        rts

iec_tab:
        .byte   $07,$87,$27,$A7,$47,$C7,$67,$E7
        .byte   $17,$97,$37,$B7,$57,$D7,$77,$F7
.assert >* = >iec_tab, error, "Page boundary!"


receive_4_bytes:
        ; Note $DD00 is set to 0 before this routine is called
        lda     $02A6   ; PAL or NTSC?
        beq     @ntsc
        ; PAL
@pal:   bit     $DD00  ; Wait until clock in low
        bvs     @pal
        ldx     #3
        nop
        bit     $01    ; Consume 3 cycles
@1:     lda     $DD00
        lsr     a
        lsr     a
        nop
        nop
        ora     $DD00
        lsr     a
        lsr     a
        nop
        nop
        ora     $DD00
        lsr     a
        lsr     a
        nop
        nop
        ora     $DD00
        sta     a:$00C1,x ; 16 bit adress for timing
        dex
        bpl     @1
.assert >* = >@pal, error, "Page boundary!"
        rts

@ntsc:  ; NTSC
        bit     $DD00  ; Wait until clock in low
        bvs     @ntsc
        ldx     #3
        nop
        bit     $01    ; Consume 3 cycles
@2:     lda     $DD00
        lsr     a
        lsr     a
        nop
        nop
        nop
        ora     $DD00
        lsr     a
        lsr     a
        nop
        nop
        ora     $DD00
        lsr     a
        lsr     a
        nop
        nop
        ora     $DD00
        sta     a:$00C1,x ; 16 bit adress for timing
        dex
        bpl     @2
.assert >* = >@ntsc, error, "Page boundary!"
        rts

.segment "speeder_a"

iec_load:
        jmp     $F530 ; IEC LOAD - used in the error case

L99C9:  pla
        pla
        pla
        tay
        lda     #$F4
        pha
        lda     #$A6
        pha
        jmp     _disable_fc3rom_set_01


new_load:
        sty     $93
        tya
        ldy     FA
        cpy     #7
        bne     @1
        ; A custom kernal may not support tape. A kernal without tape will remove the
        ; tape sense check routine, do a PEEK in it for the right value:
        ldy     #$01
        cpy     $f831
        bne     L99C9 ; KERNAL does not support tape
        cpy     $f835
        bne     L99C9 ; KERNAL does not support tape
        ; tape turbo
        tax
        beq     @2     ; LOAD? Then do not install stack code.
        ldx     #$16
:       lda     L9A50,x
        sta     L0110,x
        dex
        bpl     :-
@2:     jmp     tape_load_code
@1:     cpy     #8
        bcc     L99C9
        cpy     #10
        bcs     L99C9
        tay
        lda     $B7
        beq     L99C9
        ;
        ; We need to use 8K cartridge mode, because the routines that load bytes
        ; from memory use dec $01. In 16K mode, this hides ROML but not ROMH,
        ; in other words, you would not be able to save data from $A000..$BFFF,
        ; or store a filename there. This is a bug in the original FC3 and is
        ; hereby fixed.
        ;
        lda     #fcio_bank_0|fcio_c64_8kcrtmode|fcio_nmi_line
        sta     fcio_reg
        jsr     _load_FNADR_indy
        cmp     #$24
        beq     L99C9
        ldx     SA
        cpx     #2
        beq     L99C9
        jsr     print_searching
        lda     #$60
        sta     SA
        jsr     open_file
        lda     FA
        jsr     $ED09 ; TALK
        lda     SA
        jsr     $EDC7 ; SECTLK
        jsr     $EE13 ; IECIN
        sta     $AE
        lda     ST
        lsr     a
        lsr     a
        bcs     iec_load
        jsr     $EE13 ; IECIN
        sta     $AF
        txa
        bne     L9A35
        lda     $C3
        sta     $AE
        lda     $C4
        sta     $AF
L9A35:  jsr     print_loading
        lda     $AF
        cmp     #4
        bcc     @low
        jmp     new_load_continue
        ; Low load below $0400, for compatibility with autostart programs,
        ; abort fast loading and continue with slow kernal load
@low:   pla
        pla
        pla
        tay
        lda     #$F4
        pha
        lda     #$F2
        pha
        jmp     _disable_fc3rom_set_01

; ----------------------------------------------------------------

.segment "tape_stack_code"

L9A50:  lda     #$0C
        sta     $01
        lda     ($C3),y
        cmp     $BD
        beq     :+
        stx     ST
:       eor     $D7
        sta     $D7
        lda     #$0F
        sta     $01
        jmp     LA8FF

.segment "speeder_b"

;rts_ldx0_carry_set:
;        ldx     #0  ; Transfered to A in IO1
;        sec
;        rts

original_save:
        jmp     $F5ED ; execute original SAVE routine

new_save:
        lda     FA
        cmp     #7
        bne     @1
        ; A custom kernal may not support tape. A kernal without tape will remove the
        ; tape sense check routine, do a PEEK in it for the right value:
        ldy     #$01
        cpy     $f831
        bne     original_save ; KERNAL does not support tape
        cpy     $f835
        bne     original_save ; KERNAL does not support tape
        jmp     new_save_tape ; tape turbo
@1:     cmp     #8 ; if <8 then not a drive
        bcc     original_save
        cmp     #10
        bcs     original_save ; not a drive (XXX why only support drives 8 and 9?)
        ldy     $B7   ; length of filename
        beq     original_save
        lda     #$61
        sta     SA
        ;
        ; We need to use 8K cartridge mode, because the routines that load bytes
        ; from memory use dec $01. In 16K mode, this hides ROML but not ROMH,
        ; in other words, you would not be able to save data from $A000..$BFFF,
        ; or store a filename there. This is a bug in the original FC3 and is
        ; hereby fixed.
        ;
        lda     #fcio_bank_0|fcio_c64_8kcrtmode|fcio_nmi_line
        sta     fcio_reg
        jsr     open_file
        ; Back to 16K mode
        lda     #fcio_bank_0|fcio_c64_16kcrtmode|fcio_nmi_line
        sta     fcio_reg
        jsr     print_message ; print message (unless disabled)
        jsr     fastsave_initialize
        bne     @rts_carry_set
;        ldx     #0 ; Already done via fastsave_initialize/wait_for_next_frame
        ldy     #0
        stx     ST
        stx     $A4
        jsr     $FB8E ; copy I/O start address to buffer address
        sec
        lda     $AC
        sbc     #2
        sta     $AC
        bcs     @2
        dec     $AD
@2:     jsr     L9AD0
        lda     $C1
        jsr     send_byte_and_increment
        lda     $C2
        jsr     send_byte_and_increment
@3:     lda     #$35
        jsr     _load_ac_indy
        jsr     send_byte_and_increment
        bne     @3
        lda     $A4
        bmi     @done
        jsr     L9AD0
        bne     @3     ; always because send_byte returns with Z=0

@done:  cli
        clc
        .byte $24 ; BIT $18, 2 byte nop
@rts_carry_set:
        sec
        rts

send_byte_and_increment:
        jsr     send_byte
        jsr     $FCDB ; inc $AC/$AD
        dec     $93
        rts

L9AD0:  sec
        lda     $AE
        sbc     $AC
        tax
        sta     $93
        lda     $AF
        sbc     $AD
        bne     @1
        cpx     #$FF
        beq     @1
        inx
        txa
        dec     $A4
        bne     @2
@1:     lda     #$FE
        sta     $93
        tya
@2:     jmp     send_byte



; Serial fastload code



new_load_continue:
        jsr     UNTALK
        jsr     LA691
.import __drive_code_load_LOAD__
.import __drive_code_load_RUN__
        lda     #19
        sta     $93
        lda     #<__drive_code_load_LOAD__
        ldy     #>__drive_code_load_LOAD__
        ldx     #>__drive_code_load_RUN__ ; $0400
        jsr     transfer_code_to_drive
        lda     #<drivecode_load_initialize
        jsr     IECOUT
        lda     #>drivecode_load_initialize
        jsr     IECOUT
        jsr     UNLSTN
        sei
        ; The screen can only be disabled after drive code has been uploaded
        ; because KERNAL code overwrites $95 in which we backup the screen
        ; enable status.
.ifdef use_ill
        lax     $D011
        and     #$10 ; save screen enable bit
        sta     $95
        lda     #$EF
        sax     $D011
.else
        lda     $D011
        tax
        and     #$10 ; save screen enable bit
        sta     $95
        txa
        and     #$EF
        sta     $D011
.endif
        lda     $DD00
        and     #$07
        ora     $95 ; save VIC bank (XXX #$03 would have been enough)
        sta     $95
        ; $A6 is used by kernal tape routines, code using load should be aware
        ; it can be modified during LOAD, but just in case some program is not
        ; designed to support tape and uses $A6 for its own storage, let's just
        ; save it and be compatible.
        lda     $A6
        pha
        ; Backup $c1/$c2
        lda     $C1
        pha
        lda     $C2
        pha
        ; $A3/$A4 := $AE/$AF - $0002
        sec
        lda     $AE
        sbc     #2
        sta     $A3
        lda     $AF
        sbc     #0
        sta     $A4
@back:  bit     $DD00 ; DATA IN high?
        bmi     @recv ; Then receive data
        cli
        ; Restore $C1/$C2
        pla
        sta     $C2
        pla
        sta     $C1
        pla
        sta     $A6
        php     ; Note that pla instructions above don't modify V
.ifdef use_ill
        lda     $95
        ldx     #$07
        sax     $DD00 ; restore VIC bank
        and     #$10
        ora     $D011 ; restore screen enable bit
        sta     $D011
.else
        lda     $95
        and     #$07
        sta     $DD00 ; restore VIC bank
        lda     $95
        and     #$10
        ora     $D011 ; restore screen enable bit
        sta     $D011
.endif

        lda     #0
        sta     $A3
        sta     $A4
        sta     $94
        lda     #$60
        sta     SA
        lda     #$E0
        jsr     LA612
        jsr     UNLSTN
        plp
        bvs     @done ; used to be "bcs" in 1988-05
        ldx     #$1D  ; kernal error code, transfered to A in IO1
        sec
        rts

@done:  lda     #$40
        sta     ST
        jsr     LA694
        jmp     $F5A9 ; LOAD done

@recv:  bvs     @back ; CLOCK IN high? Then back
        lda     #$20  ; DATA OUT high, CLOCK OUT 0
        sta     $DD00
@1:     bit     $DD00 ; Wait until CLOCK IN is high
        bvc     @1
        lda     #0    ; Clear $DD00 to simply receive algorithm
        sta     $DD00
        jsr     receive_4_bytes
        lda     $C3   ; Contains block number (determines memory location to write to)
        php           ; Need to know whether block 0 later

        ; Compute memory address to write to.
        ;    Addr:= base + blockno * 254
        ; -> Addr:= base + blockno shl 8 - 2 * blockno
;        clc          ; Carry already cleared by receive_4_bytes
        adc     $A4
        asl     $C3
        tax
        bcc     :+
        dex
:       sec
        lda     $A3
        sbc     $C3
        sta     $93
        bcs     @2
        dex
@2:
@3:     stx     $94

        ldx     $C2  ; Contains number of bytes in block to use (0 if all)
        stx     $A6
        bne     :+
        dex
:       dex
        stx     $A5
        ldy     #0
        plp      ; Block 0??
        bne     @5
        ; First block, skip load address
        jsr     receive_4_bytes ; in $C1..$C4
        ldy     #2
        bne     @9              ; always taken

@b:     lda     $A6
        beq     :+
        clc
        adc     $93
        sta     $AE
        lda     $94
        adc     #0
        sta     $AF
:       jmp     @back

@6:     jsr     receive_4_bytes ; in $C1..C4  (A,X modified, Y untouched)
        cpy     $A5
        bcs     @8
        lda     $C4             ; copy byte ...
        sta     ($93),y         ; ...to target memory
@8:     iny
        cpy     #$FE            ; End of block?
        bcs     @b              ; If yes? Back to next block
@9:
        cpy     $A5
        bcs     @10
        lda     $C3             ; copy byte ...
        sta     ($93),y         ; ...to target memory
@10:    iny
;        cpy     #$FE           ; Never happens because block size is constant
;        bcs     @b
        cpy     $A5
        bcs     @12
        lda     $C2             ; copy byte ...
        sta     ($93),y         ; ...to target memory
@12:    iny
;        cpy     #$FE           ; Never happens because block size is constant
;        bcs     @b
        cpy     $A5
        bcs     @14
@5:     lda     $C1             ; copy byte ...
        sta     ($93),y         ; ...to target memory
@14:    iny
;        cpy     #$FE           ; Never happens because block size is constant
;        bcs     @b
        bne     @6              ; always taken

; ----------------------------------------------------------------

.segment "drive_code_load" ; $0400

sector_not_needed = $FF

drive_code_load:
        lda     $43        ; Number of sectors on current track
        sta     $C1
L9BFE:
        ;
        ; Here we wait for a sector header and read it
        ;
        jsr     wait_for_header ; (sets Y=0)
        ; 7 more bytes to read
@1:     bvc     @1         ; Loop until byte ready
        clv
        lda     $1C01      ; Read next byte of header
        sta     $25,y      ; Store
        iny
        cpy     #7         ; Did we read 7 bytes?
        bne     @1         ; No? Read next byte
        ; Sector header has been read

        ; 
        ; Now we read 5 bytes of the sector data
        ;
        jsr     wait_for_sync ; Sets Y=0
        clv
@2:     bvc     @2         ; Loop until byte ready
        clv
        lda     $1C01
        sta     ($30),y
        iny
        cpy     #5         ; Did we read 5 bytes?
        bne     @2         ; No? Read next byte
        jsr     $F497      ; GCR decode header (not sector data) and write to $16..$1A

        ; Check checksum $1A = $16 xor $17 xor $18 xor $19
        ; Therefore xorring $16..$1A should result in 0
        ldx     #5
        lda     #0
@3:     eor     $15,x
        dex
        bne     @3
        tay
        beq     @4
@error: jmp     $F40B     ; Read error

                          ; X=0
@4:     inx               ; X=1
@6:     lda     $12,x     ; Compare expected header ID
        cmp     $16,x     ; ..  with read header ID
        bne     @error
        dex
        bpl     @6

        jsr     $F7E8     ; GCR decode first 5 bytes of sector data and write to $52..$55
        ldx     $19       ; Is the sector number that we read smaller
        cpx     $43       ; than the number of sectors on this track?
        bcs     @error
        lda     $53       ; Store next track
        sta     track_links,x
        lda     $54       ; Store next sector
        sta     sector_links,x
        lda     #sector_not_needed
        sta     sector_order,x ; initialize array
        dec     $C1
        bne     L9BFE

        ;
        ; Now build the sector_order array
        ;
        lda     #1
        sta     $C3
        ldx     $09       ; Sector last read
@7:     lda     $C2       ; Counter, initalized to 0 by drivecode_load_initialize
        sta     sector_order,x
        inc     $C2
        lda     track_links,x
        cmp     $08       ; Next sector on the same track as last?
        bne     @8        ; Then sector_order array is finished
        lda     sector_links,x ; Chain to
        tax                    ; next sector of file
        inc     $C3
        bne     @7
        beq     @error         ; If $C3 hits 0 (255 iterations), then there must be a cycle in the sector chain
        ;
        ; When we arrive here we either need to continue on a different track, or
        ; we hit the final sector of the file (A=0). Either way the sector_order array
        ; is complete.
        ;
@8:     cmp     $02AC          ; Track beyond end of disk?
        bcs     @error         ; Then a problem. NOTE: This is incompatible with dual sided disks on 1571.
        sta     $08
        lda     sector_links,x
        sta     $09

        ;
        ; Wait for a sector header and read it
        ;
@9:     jsr     wait_for_header ; (sets Y=0)
        iny
        ; 3 more bytes to read
@10:    bvc     @10       ; Loop until byte ready
        clv
        lda     $1C01
        sta     ($30),y
        iny
        cpy     #4
        bne     @10
        ldy     #0
        jsr     $F7E8     ; GCR decode the bytes
        ldx     $54       ; If sector number
        cpx     $43       ; >= number of sectors on track
        bcs     @error    ; then there is a problem
        lda     sector_order,x
        cmp     #sector_not_needed   ; If we don't need to read this sector,
        beq     @9                   ; Wait for the next one
        stx     $C0

        ;
        ; This is a sector we need. so read its contents
        ;
        jsr     wait_for_sync ; Sets Y=0
        clv
        ; Read 256 bytes in the buffer
@11:    bvc     @11       ; Loop until byte ready
        clv
        lda     $1C01
        sta     ($30),y
        iny
        bne     @11
        ; Read another 70 bytes in the auxiliary buffer at end of the stack
        ldy     #$BA
@12:    bvc     @12      ; Loop until byte ready
        clv
        lda     $1C01
        sta     $0100,y
        iny
        bne     @12
        ; GCR decode bytes
        jsr     $F7E8
        lda     $53      ; Get link to next track ???
        beq     @13      ; 0? Then skip
        lda     #0       ; Clear link to next sector ???
        sta     $54
@13:    sta     $34
        sta     $C1
        ldx     $C0
        lda     sector_order,x
        sta     $53
        lda     #sector_not_needed   ; We won't need this sector anymore
        sta     sector_order,x
        jsr     $F6D0    ; Encode 4 bytes to 5 GCR bytes
        lda     #$42
        sta     $36

        ; Signal C64 that we want to transmit
        ldy     #$08     ; Clock out high, data out low
        sty     $1800
        ; C64 will set DATA IN high if it is ready to receive
@14:    lda     $1800
        lsr     a
        bcc     @14
        ldy     #0
@next:
        sty     $1800     ; Y=0 also when entering via branch -> DATA OUT low, CLOCK OUT low
        dec     $36
        bne     @transmit_buffer
        dec     $C3       ; Did we read all blocks?
        bne     @9
        jmp     $F418     ; Set buffer status at $0001 to 01 (succesfull completion)

@transmit_buffer:
        ; 5 bytes of GCR data become 4 bytes of decoded data. But the GCR data will not be decoded
        ; into raw data, but directly decoded into values that can be written to VIA register $1800.
        ; In order to convert to register values, we will convert the 5 bytes GCR into 8 "quintets"
        ; of 5 bits that we will store at $55..$5D.
        ;
        ; A "quintet" can be convert to 4 bits of decoded data by a lookup table, but we are not
        ; going to generate decoded data, but VIA register values. We transmit two bytes at a time
        ; over the serial bus via the clock and data lines. Thus in order to transmit 4 bits, we need
        ; two VIA register values that will be transmited after each other.
        ;
        ; In other words, we need two 32 byte lookup tables. However, because in a GCR code, no two
        ; zeros can occur in a row, the first 8 values will never occur and we can limit ourselves
        ; to 24 byte lookup tables.
        ;
        ; Convert to nibbles:
        ldy     $C1
.ifdef use_ill
        lax     ($30),y
.else
        lda     ($30),y
        tax
.endif
        lsr     a
        lsr     a
        lsr     a
        sta     $5C
.ifdef use_ill
        lda     #$07
        sax     $5D
.else
        txa
        and     #$07
        sta     $5D
.endif
        iny
        bne     @16      ; Not end of regular buffer?
        iny              ; End of register buffer
        sty     $31      ; Y=1
        ldy     #$BA     ; Continue from auxiliary buffer at $01BA
@16:
.ifdef use_ill
        lax     ($30),y
.else
        lda     ($30),y
        tax
.endif
        asl     a
        rol     $5D
        asl     a
        rol     $5D
        lsr     a
        lsr     a
        lsr     a
        sta     $5A
        txa
        lsr     a
        iny
.ifdef use_ill
        lax     ($30),y
.else
        lda     ($30),y
        tax
.endif
        rol     a
        rol     a
        rol     a
        rol     a
        rol     a
        and     #$1F
        sta     $5B
.ifdef use_ill
        lda     #$0F
        sax     $58
.else
        txa
        and     #$0F
        sta     $58
.endif
        iny
.ifdef use_ill
        lax     ($30),y
.else
        lda     ($30),y
        tax
.endif
        asl     a
        rol     $58
        lsr     a
        lsr     a
        lsr     a
        sta     $59
        txa
        asl     a
        asl     a
        asl     a
        and     #$18
        sta     $56
        iny
.ifdef use_ill
        lax     ($30),y
.else
        lda     ($30),y
        tax
.endif
        rol     a
        rol     a
        rol     a
        rol     a
        and     #$07
        ora     $56
        sta     $56
.ifdef use_ill
        lda     #$1F
        sax     $57
.else
        txa
        and     #$1F
        sta     $57
.endif
        iny
        sty     $C1
@transmit_tuple:
        ; Transmit the 4-byte tuple to the C64
        ; $55..5D contain indexes into the tables with CIA register values
        ldy     #$08      ; Signal C64 with CLOCK OUT high, DATA OUT low
        sty     $1800
        ldx     $55,y     ; Replaced by bne @transmit_tuple_2mhz in 2MHz mode
        ; Transmit bits 0-1 of the 4 bits of decoded data
@15:    lda     regvalue_lookup_01 - 8,x  ; - 8 because the table is only 24 rather than 32 bytes
        sta     $1800
        ; Transmit bits 2-3 of the 4 bits of decoded data
        lda     regvalue_lookup_23 - 8,x  ; - 8 because the table is only 24 rather than 32 bytes
        ldx     $54,y     ; This ldx might look illogicallly placed but is here also for timing reasons!
        sta     $1800
        dey
        bne     @15
.assert >* = >@transmit_tuple, error, "Page boundary!"
        jmp     @next

@transmit_tuple_2mhz:
        ; Transmit the 4-byte tuple to the C64
        ; $55..5D contain indexes into the tables with CIA register values
        ldx     $55,y
        nop
@17:
        nop
        ; Transmit bits 0-1 of the 4 bits of decoded data
        lda     regvalue_lookup_01 - 8,x  ; - 8 because the table is only 24 rather than 32 bytes
        nop
        nop
        sta     $1800
        nop
        nop
        ; Transmit bits 2-3 of the 4 bits of decoded data
        lda     regvalue_lookup_23 - 8,x  ; - 8 because the table is only 24 rather than 32 bytes
        nop
        nop
        ldx     $54,y     ; This ldx might look illogicallly placed but is here also for timing reasons!
        nop
        nop
        sta     $1800
        nop
        bit     $00       ; Waste 3 cycles
        dey
        nop
        bne     @17
.assert >* = >@transmit_tuple, error, "Page boundary!"
        ; Because we can convert GCR to kwintets much faster in 2MHz mode, we need a little delay,
        ; otherwise the C64 can't write the transmitted bytes to destination memory fast enough
.ifdef use_ill
        ldy     #8
.else
        ldy     #5
.endif
@18:
        dey
        bne     @18
        jmp     @next

modpoint = @transmit_tuple+5



wait_for_sync:
        ; This jmp ismodified by drive_code_initialize to jump to $9754 in case
        ;of a 1571 in 2MHz mode.
        jmp     $F556     ; Wait for SYNC on disk (sets Y=0)

wait_for_header:
        ldx     #3
        stx     $31
        ldx     #90      ; The 1541 ROM also tries 90 times (see $F3B1 onwards)
        stx     $C4
@try_again:
        dec     $C4
        bne     @try
        jmp     $F40B    ; Read error

@try:   jsr     wait_for_sync ; Sets Y=0
        clv
@1:     bvc     @1       ; Loop until byte ready
        clv
        lda     $1C01
        cmp     $24      ; Header block ID as expected?
        bne     @try_again
        rts

drivecode_load_initialize:
        lda     $02AC    ; 1571 stores number of tracks on current disk here (either $24 or $71)
        bne     @3
        lda     #$24     ; 1541 has a 0 there, that's quickly fixed
        sta     $02AC
@3:
        lda     #$37
        cmp     $E5C6    ; Skip 2MHz check for 1541. Not needed on real hardware, but on the 1541
        bne     @4       ; Ultimate you would falsely detect 2MHz. Don't check too naive :)
        lda     $180F
        and     #$20     ; Check for 2MHz mode
        beq     @4
        ; 1571 in 2MHz mode. Adjust wait_for_sync to jump to $9754 instead
        lda     #$54
        sta     wait_for_sync+1
        lda     #$97
        sta     wait_for_sync+2
        lda     #$D0
        sta     modpoint
        lda     #$14
        sta     modpoint+1
@4:
        ldx     #$00     ; CLOCK OUT low, DATA OUT low
        stx     $1800
        stx     $C2
        lda     $19      ; Sector number last read sector (first sector of program file)
        sta     $09      ; Buffer 1 sector
        lda     $18      ; Track of last read reactor  (first sector of program file)
        sta     $08      ; Buffer 1 track
        ; The drive code is in memory at $400, the address of buffer 1.
        ; So we want to send an execute command for buffer 1.
@2:     lda     #$E0     ; $E0 = read sector header and then execute code in buffer
        sta     $01
@1:     lda     $01      ; Wait until command has completed
        bmi     @1
        ;
        ; If the command has completed, it means the load has completed.
        ;
        cmp     #2       ; >=2 means error
        bcs     @error
        lda     $08
        bne     @2
        lda     #$02     ; DATA OUT high, CLOCK OUT low 
        sta     $1800
        jmp     $C194    ; Prepare status message

@error: inx
        ldy     #$0A     ; DATA out high, lock out high
        sty     $1800
        jmp     $E60A    ; 21, 'read error'

; These tables cannot cross a page boundary because lda absolute,x then costs an extra cycle
regvalue_lookup_01:
        .byte   0, 10, 10, 2
        .byte   0, 10, 10, 2
        .byte   0, 0, 8, 0
        .byte   0, 0, 8, 0
        .byte   0, 2, 8, 0
        .byte   0, 2, 8, 0
.assert >* = >(regvalue_lookup_01-8), error, "Page boundary!"

regvalue_lookup_23:
        .byte   0, 8, 10, 10
        .byte   0, 0, 2, 2
        .byte   0, 0, 10, 10
        .byte   0, 0, 2, 2
        .byte   0, 8, 8, 8
        .byte   0, 0, 0, 0
.assert >* = >(regvalue_lookup_23-8), error, "Page boundary!"

sector_links:
track_links := sector_links + 21
sector_order := track_links + 21

crc_correction_l:
.ifdef use_ill
        .word $74c3
.else
        .word $65f7
.endif
        .align 32

; ----------------------------------------------------------------
; drive code $0500
; ----------------------------------------------------------------
.segment "drive_code_save"

ram_code := $0680

buffer_to_use_times2 = drive_code_save + 1

drive_code_save:
        lda     #0    ; This is replaced at runtime by the buffer number * 2
        tax
        lsr     a     ; Divide by 2 to get actual buffer number
        adc     #3    ; Add 3 to get high byte of memory location of buffer
        sta     $95
        sta     $31
        txa
        adc     #6
        sta     $32
LA510:
        jsr     receive_byte  ; Receive number of bytes that C64 has to send
        beq     :+            ; 0 means 256
        sta     $81
        tax
        inx
        stx     end_position
        lda     #0
        sta     $80
        beq     LA534

:       lda     $02FC         ; Nr. of free blocks (high)
        ora     $02FA         ; Nr. or free blocks (low)
        bne     :+
        lda     #$72
        jmp     $F969 ; DISK FULL

:       jsr     $F11E ; find and allocate free block
LA534:  ldy     #0
        sty     $94
        lda     $80
        sta     ($94),y
        iny
        lda     $81
        sta     ($94),y
        iny
LA542:  jsr     receive_byte
        sta     ($30),y
        iny
end_position = * + 1
        cpy     #0    ; #0 is modified to position where receive should end
        bne     LA542
        jsr     ram_code
        inc     $B6
        ldx     buffer_to_use_times2
        lda     $81
        sta     $07,x
        lda     $80
        cmp     $06,x
        beq     LA510
        sta     $06,x
        jmp     $F418 ; set OK code

receive_byte:
        lda     #$00
        sta     $1800
        lda     #$04
@1:     bit     $1800
        bne     @1
        sta     $C0     ; Replaced by beq @receive_byte_2mhz in 2MHz mode
drive_code_save_timing_selfmod1:
        sta     $C0
        lda     $1800
        asl     a
        nop
        nop
        ora     $1800
        asl     a
        asl     a
        asl     a
        asl     a
        sta     a:$C0  ; 16 bit address for timing!
        lda     $1800
        asl     a
;drive_code_save_timing_selfmod2:
        nop
drive_code_save_timing_selfmod2:
        nop
        ora     $1800
drive_code_save_receive_end:
        pha
        lda     #$02
        sta     $1800
        pla
        and     #$0F
        ora     $C0
        sta     $C0
;        lda     $C0
        rts
drive_code_save_timing_selfmod2_end:
        nop ; filler, gets overwritten when L0589 gets copied down by 1 byte


drive_code_save_timing_selfmod5 = drive_code_save_timing_selfmod1-2


receive_byte_2mhz:
        sta     $C0
drive_code_save_timing_selfmod3:
        sta     $C0
        sta     $C0
        sta     $C0
        nop
        lda     $1800
        nop
        nop
        asl     a
        nop

        nop
        nop
        nop
        bit     $C0
        ora     $1800
        nop
        nop

        asl     a
        nop
        asl     a
        nop
        asl     a
        nop
        asl     a
        nop
        sta     a:$C0 ; 16 bit address for timing!
        nop
        nop
        lda     $1800
        nop
        nop
        asl     a
        nop
        nop
        nop
drive_code_save_timing_selfmod4:
        nop
        nop
        nop
        ora     $1800
        jmp     drive_code_save_receive_end
;        and     #$0F
;        ora     $C0
;        sta     $C0
;        lda     #$02
;        sta     $1800
;        lda     $C0
;        rts
drive_code_save_timing_selfmod4_end:
        nop ; filler, gets overwritten when L0589 gets copied down by 1 byte
;        nop


L059C:
        ; PAL entry
;        lda     #$85
;        sta     drive_code_save_timing_selfmod2
;        lda     #$EA
;        sta     drive_code_save_timing_selfmod2+2
        ldx     #drive_code_save_timing_selfmod2_end - drive_code_save_timing_selfmod2 - 1
LA5A6:  lda     drive_code_save_timing_selfmod2,x
        sta     drive_code_save_timing_selfmod2+1,x ; insert 2 cycles into code
        dex
        bpl     LA5A6

        ldx     #drive_code_save_timing_selfmod4_end - drive_code_save_timing_selfmod4 - 1
LA5A62:  lda     drive_code_save_timing_selfmod4,x
        sta     drive_code_save_timing_selfmod4+1,x ; insert 2 cycles into code
        dex
        bpl     LA5A62

;        ldx     #$02
;        stx     $1800
        lda     #$EA
        sta     drive_code_save_timing_selfmod2
;        lda     #$EA
        sta     drive_code_save_timing_selfmod1
        sta     drive_code_save_timing_selfmod1 + 1 ; insert 1 cycle into code
        sta     drive_code_save_timing_selfmod3
        sta     drive_code_save_timing_selfmod3 + 1 ; insert 1 cycle into code
        sta     drive_code_save_timing_selfmod3 + 2 ; insert 1 cycle into code
        sta     drive_code_save_timing_selfmod3 + 3 ; insert 1 cycle into code

L05AF:
        ldx     #$65
        ; NTSC entry
        ; 9775 + $74
        lda     #$37
        cmp     $E5C6    ; Skip 2MHz check for 1541. Not needed on real hardware, but on the 1541
        bne     @1541    ; Ultimate you would falsely detect 2MHz. Don't check too naive :)
        lda     $180F
        and     #$20     ; Check for 2MHz mode
        beq     @1541
        ; 1571 in 2MHz mode
        lda     #$F0
        sta     drive_code_save_timing_selfmod5
        ;lda     #$2A
        lda     #receive_byte_2mhz - drive_code_save_timing_selfmod5 - 2
        sta     drive_code_save_timing_selfmod5 + 1
        lda     #$73
        ldx     #$75
        sta     @mod1571 + 1
        lda     #$97
        sta     @mod1571 + 2
@1541:
        lda     #$60     ; Opcode for RTS
@1:
        sta     ram_code - 1,x
@mod1571:
        lda     $F575 - 2,x; copy "write data block to disk" to RAM
        dex
        bne     @1
        inx
        stx     $82      ; X=1, channel number
        stx     $83
        jsr     $DF95    ; Get active buffer into A (preserves X,Y)
        inx
        stx     $1800    ; X=2
@2:     inx
        bne     @2
        sta     buffer_to_use
        asl     a
        sta     buffer_to_use_times2
        tax
        lda     #$40
        sta     $02F9
@next:  lda     $06,x
        beq     @done
        sta     $0A
        lda     #$E0     ; $E0 = read sector header and execute buffer
        sta     $02
@wait:  lda     $02
        bmi     @wait
        cmp     #2       ; Smaller than 2 means no error
        bcc     @next    ; Next sector if no error
        cmp     #$72     ; Disk full?
        bne     @error
        jmp     $C1C8    ; set error message

@error: ldx     #0    ; #0 is replaced by buffer nummer
        jmp     $E60A ; Error handling - A: error code, X: buffer number
@done:
        jsr     $DBA5 ; write directory entry
        jsr     $EEF4 ; write BAM
        jmp     $D227 ; close channel

buffer_to_use = @error + 1

crc_correction_s:
        .word $9148
        .align 32

; ----------------------------------------------------------------
; C64 IEC code
; ----------------------------------------------------------------
.segment "speeder_c"

LA612:  pha
        lda     FA
        jsr     LISTEN
        pla
        jmp     SECOND

LA628:  jsr     LA632
        jsr     $E716 ; KERNAL: output character to screen
        tya
        jmp     $E716 ; KERNAL: output character to screen

LA632:  pha
        and     #$0F
        jsr     LA63E
        tay
        pla
        lsr     a
        lsr     a
        lsr     a
        lsr     a
LA63E:  clc
        adc     #$F6
        bcc     LA645
        adc     #$06
LA645:  adc     #$3A
rts_01: rts

fastsave_initialize:
        jsr     check_iec_error
        bne     rts_01
        lda     #12
        sta     $93
.import __drive_code_save_LOAD__
.import __drive_code_save_RUN__
        lda     #<__drive_code_save_LOAD__
        ldy     #>__drive_code_save_LOAD__
        ldx     #>__drive_code_save_RUN__
        jsr     transfer_code_to_drive
        lda     $02A6
        beq     @ntsc
        ; PAL
        lda     #<L059C
        jsr     IECOUT
        lda     #>L059C
        bne     LA671  ; Always taken

@ntsc:  ;NTSC
        lda     #<L05AF
        jsr     IECOUT
        lda     #>L05AF
LA671:  jsr     IECOUT
        jsr     UNLSTN
        sei
        lda     $D015  ; Save sprite enable reg
        sta     $93
        lda     #0     ; Hide sprites
        sta     $D015
        lda     $DD00
        and     #$07
        sta     $A4
        ora     #$10
        sta     $A5
        sta     $DD00
        jmp     wait_for_next_frame

LA691:
        ldy     #0
        .byte   $2C    ; opcode for bit $ABCD
LA694:
        ldy     #8
        bit     $9D
        bpl     LA6A7
        jsr     LA6A8
        lda     $AF
        jsr     LA628
        lda     $AE
        jmp     LA628

LA6A7:  rts

LA6A8:  lda     s_from,y
        beq     LA6A7
        jsr     $E716 ; KERNAL: output character to screen
        iny
        bne     LA6A8

s_from: .byte   " FROM $", 0
        .byte   " TO $", 0

open_file:
        ldy     #0
        sty     ST
        lda     FA
        jsr     $ED0C ; LISTEN
        lda     SA
        ora     #$F0  ; $F0 means OPEN
        jsr     $EDB9 ; SECLST
        lda     ST
        bpl     @1
        pla
        pla
        jmp     $F707 ; DEVICE NOT PRESENT ERROR

@1:     jsr     _load_FNADR_indy
        jsr     $EDDD ; KERNAL IECOUT
        iny
        cpy     $B7
        bne     @1
        jmp     $F654 ; UNLISTEN


tape_wait_play:
         ; if already pressed, no need to display messages
        jsr     $F82E ; cassette sense
        beq     rts_  ; $F82E clears carry
        ldy     #$1B  ; print PRESS PLAY ON TAPE
print:  jsr     maybe_print_kernal_string
        ; Wait for key on tape, but allow run/stop to abort.
        ; Run/stop is column 7, row 7
        ; $DC00 = $7f, = row 7 selected. Therefore test bit 7 of $DC01:
:       bit     $DC01
        bpl     rts_carry_set
        jsr     $F82E ; cassette sense
        bne     :-
        ldy     #$6A  ; Offset to OK
        bne     maybe_print_kernal_string ; (always) print OK

tape_wait_record:
        jsr     $F82E ; cassette sense
        beq     rts_  ; $F82E clears carry
        ldy     #$2E  ; print PRESS RECORD & PLAY ON TAPE
        bne     print ; always
;rts_lda0_carry_set:
;        lda     #$00
rts_carry_set:
        sec
rts_:
        rts

print_found:
        lda     $9D
        bpl     LA7A7
        ldy     #$63 ; "FOUND"
        jsr     print_kernal_string
        ldy     #5
:       lda     ($B2),y
        jsr     $E716 ; KERNAL: output character to screen
        iny
        cpy     #$15
        bne     :-
        rts

print_message:
        jsr     print_saving
        bmi     LA796
        rts

print_searching:
        lda     $9D
        bpl     LA7A7
        ldy     #$0C ; "SEARCHING"
        jsr     print_kernal_string
        lda     $B7
        beq     LA7A7
        ldy     #$17 ; "FOR"
        jsr     print_kernal_string
LA796:  ldy     $B7
        beq     LA7A7
        ldy     #0
LA79C:  jsr     _load_FNADR_indy
        jsr     $E716 ; KERNAL: output character to screen
        iny
        cpy     $B7
        bne     LA79C
LA7A7:  rts

print_loading:
        ldy     #$49 ; "LOADING"
        lda     $93
        beq     maybe_print_kernal_string
print_verifying:
        ldy     #$59 ; "VERIFYING"
        .byte   $2C  ; BIT $abcd
print_saving:
        ldy     #$51 ; "SAVING"
maybe_print_kernal_string:
        bit     $9D  ; Display messages?
        bpl     LA7C4 ; return if no
print_kernal_string:
        lda     $F0BD,y ; KERNAL strings
        php
        and     #$7F
        jsr     $E716 ; KERNAL: output character to screen
        iny
        plp
        bpl     print_kernal_string ; until MSB set
LA7C4:  clc
        rts

turn_screen_off:
        ldy     #0
        sty     $C0
        lda     $D011
        and     #$EF
        sta     $D011 ; turn screen off
wait_for_next_frame:
        ; It would be tempting to optimize this wait loop, but it creates too
        ; much problems. The delay loop is necessary to ensure the VIC-II
        ; has stopped generating bad lines, to give the floppy drive enough
        ; time to executing the drive code and start receiving bytes, and
        ; to give the C64s irq handler enough time to start the tape motor
        ; when a tape button is pressed.
:       dex
        bne :-
        dey
        bne :-
        sei
        rts

; ----------------------------------------------------------------
; tape related

.segment "tape"

new_save_tape:
        ldx     #5
        stx     $AB
        jsr     $FB8E ; copy I/O start address to buffer address
        jsr     tape_wait_record
        bcc     :+
        lda     #0
        jmp     _disable_fc3rom
:       jsr     print_message  ; print mesaage on screen (unless disabled)
        jsr     turn_screen_off

        ; Start writing file metadata in turbotape format
        jsr     tape_write_header
        lda     SA
        clc
        adc     #1
        dex
        jsr     tape_write_byte ; write SA+1 (file header)
        ldx     #8
:       lda     $AC,y
        jsr     tape_write_byte
        ldx     #6
        iny
        cpy     #5
        nop
        bne     :-
        ldy     #0
        ldx     #2
@1:     jsr     _load_FNADR_indy
        cpy     $B7
        bcc     :+
        lda     #$20
        dex
:       jsr     tape_write_byte
        ldx     #3
        iny
        cpy     #$BB
        bne     @1
        lda     #2
        sta     $AB

        ; Start writing file data in turbotape format
        jsr     tape_write_header
        tya     ; Write a 0 (file data)
        jsr     tape_write_byte
        sty     $D7
        ldx     #5
LA82B:  jsr     tape_write_byte_from_ram
        ldx     #3 ; used to be "#2" in 1988-05
        inc     $AC
        bne     :+
        inc     $AD
        dex
:       lda     $AC
        cmp     $AE
        lda     $AD
        sbc     $AF
        bcc     LA82B
LA841:  lda     $D7
        jsr     tape_write_byte
        ldx     #7
        dey
        bne     LA841
        jsr     LA912
        jmp     _disable_fc3rom

tape_load_code:
        jsr     LA8C9
        lda     $AB
        cmp     #2
        beq     @1
        cmp     #1
        bne     tape_load_code
        lda     SA
        beq     @2 ; "LOAD"[...]",n,0" -> skip load address
@1:     lda     $033C
        sta     $C3
        lda     $033D
        sta     $C4
@2:     jsr     print_found
        cli
        lda     $A1
        jsr     $E4E0 ; wait for CBM key
        sei
        lda     $01
        and     #$1F
        sta     $01
        ldy     $B7  ; File name specified for load?
        beq     @nofn
:       dey
        jsr     _load_FNADR_indy
        cmp     $0341,y
        bne     tape_load_code ; file name not matched, restart
        tya
        bne     :-
@nofn:  sty     ST
        jsr     print_loading
        lda     $C3
        sta     $AC
        lda     $C4
        sta     $AD
        sec
        lda     $033E
        sbc     $033C
        php
        clc
        adc     $C3
        sta     $AE
        lda     $033F
        adc     $C4
        plp
        sbc     $033D
        sta     $AF
        jsr     LA8E5
        lda     $BD
        eor     $D7
        ora     ST
        clc
        beq     LA8C2
        sec
        lda     #$FF
        sta     ST
LA8C2:  ldx     $AE
        ldy     $AF
        jmp     _disable_fc3rom

LA8C9:  jsr     tape_search_header ; sets Y=0
        lda     $BD
        beq     LA8C9              ; if it is file data, look for next header
        sta     $AB
:       jsr     tape_read_byte
        lda     $BD
        sta     ($B2),y
        iny
        cpy     #$C0
        bne     :-
        beq     LA913 ; always

LA8E2:  jmp     L0110

LA8E5:  jsr     tape_search_header
LA8E8:  jsr     tape_read_byte
        cpy     $93
        bne     LA8E2
        lda     #$0B
        sta     $01
        lda     $BD
        sta     ($C3),y
        eor     $D7
        sta     $D7
        lda     #$0F
        sta     $01
LA8FF:
        inc     $C3
        bne     LA905
        inc     $C4
LA905:  lda     $C3
        cmp     $AE
        lda     $C4
        sbc     $AF
        bcc     LA8E8
        jsr     tape_read_byte
LA912:  iny
LA913:  sty     $C0
        lda     #0
        sta     $02A0
        lda     $D011
        ora     #$10
        sta     $D011 ; turn screen on
        lda     $01
        ora     #$20
        sta     $01
        cli
        clc
        rts

tape_search_header:
        jsr     tape_wait_play
        bcc     :+
        ; Abort because run/stop pressed
        pla
        pla
        pla
        pla
        lda     #0
        jmp     _disable_fc3rom
:       jsr     turn_screen_off
        sty     $D7
        lda     #$07
        sta     $DD06
        ;
        ; A turbo tape header starts with 256 times a byte with value 2 and then
        ; a countdown form 9 to 1. Read a bits until we have a 2 and are possibly
        ; synced, then check for the countdown.
        ;
        ; The loader has been coded so that besides the magical 2, $f2 is also
        ; accepted and 2 and $f2 can be mixed in the header without limitation.
        ; The reason is unknown but likely compatibility with other implementations
        ; of turbotape.
        ;
        ldx     #1
@nsync: jsr     tape_read_bit
        rol     $BD
        lda     $BD
        cmp     #2
        beq     @sync
        cmp     #$F2
        bne     @nsync
@sync:  ldy     #9
:       jsr     tape_read_byte
        lda     $BD
        cmp     #2
        beq     :-
        cmp     #$F2
        beq     :-
:       cpy     $BD
        bne     @nsync
        jsr     tape_read_byte
        dey
        bne     :-
        rts

tape_read_byte:
        lda     #8
        sta     $A3
:       jsr     tape_read_bit
        rol     $BD
        nop
        nop
        dec     $A3
        bne     :-
        rts

tape_read_bit:
        lda     #$10
:       bit     $DC0D
        beq     :-
        lda     $DD0D ; Timer b underflow determines 0 or 1
        stx     $DD07 ; Set timer B
        pha
        lda     #$19
        sta     $DD0F ; Start timer B
        pla
        lsr     a
        lsr     a
        rts

        lda     #4
        sta     $AB

tape_write_header:
        ldy     #0
:       lda     #2
        jsr     tape_write_byte
        ldx     #7
        dey
        cpy     #9
        bne     :-
        ldx     #5
        dec     $AB
        bne     tape_write_byte
:       tya
        jsr     tape_write_byte
        ldx     #7
        dey
        bne     :-
        dex
        dex
        sty     $D7
        rts

tape_write_byte:
        sta     $BD
        eor     $D7
        sta     $D7
        lda     #8
        sta     $A3
LA9C5:  asl     $BD
        lda     $01
        and     #$F7
        jsr     LA9DD
        ldx     #$11
        nop
        ora     #8
        jsr     LA9DD
        ldx     #14
        dec     $A3
        bne     LA9C5
        rts

LA9DD:  dex
        bne     LA9DD
        bcc     LA9E7
        ldx     #11
LA9E4:  dex
        bne     LA9E4
LA9E7:  sta     $01
        rts

