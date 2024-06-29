; ----------------------------------------------------------------
; Centronics and RS-232 printer drivers
; ----------------------------------------------------------------
; This hooks CKOUT, BSOUT, CLRCH and CLALL to support Centronics
; and RS-232 printers as device #4.

.include "kernal.i"
.include "persistent.i"

.global set_io_vectors_with_hidden_rom
.global set_io_vectors
.global cent_rs232_or_cbm
.global new_ckout
.global new_bsout
.global new_clall
.global new_clrch

.segment "printer"

;
; The Centronics interface uses user port signals PB0..PB7 to transmit data.
; PA2 is used as the Centronics STROBE signal and FLAG as the BUSY signal.
;
; To send a byte to a Centronics printer, the data must be presented at the data
; lines, and then STROBE must be toggled. The printer responds by raising BUSY,
; when BUSY becomes low again, the printer is ready to receive the next byte.
;

;$DD0C:
; bit 7:   Current printing mode (0=CBM bus, 1=centronics/rs232)
; bit 5:   MPS-803 temporary charset 0=graphic, 1=business
; bit 4:   temporary charset switch active
; bit 3:   1 = cbm reversed mode
; bit 2:   1 = cbm mode
; bit 1:   1 = business mode (global)
; bit 0:   1 = graphics mode (global)
; if bit7 set:
;    $c0 = send characters untranslated to printer

set_io_vectors_with_hidden_rom:
        jmp     set_io_vectors_with_hidden_rom2

set_io_vectors:
        jmp     set_io_vectors2

printer_send_byte:
        pha
        lda     $DC0C
        cmp     #$FE
        beq     rs232_send_byte ; RS-232
        pla
        jsr     centronics_send_byte
        lda     #$10
:       bit     $DD0D ; Wait for flag
        beq     :-
        rts

centronics_send_byte:
        ; Write byte to user port PB0..PB7
        sta     $DD01
        lda     $DD0D
        lda     $DD00
        ; Toggle the STROBE line to tell the printer there is new data.
        and     #$FB
        sta     $DD00
        ora     #$04
        sta     $DD00
        rts

; RS232 transfer, send
rs232_send_byte:
        pla
        sta     $A5
        txa
        pha
LA03A:  lda     $DD01
        asl     a
        asl     a
        bcc     LA03A
        lda     #$10
        sta     $DD0E
        lda     #$64
        sta     $DD04
        lda     #$00
        sta     $DD05
        lda     $DD0D
        bit     $D011
        bmi     LA065
LA058:  lda     $D012
        and     #$0F
        cmp     #$02
        beq     LA065
        cmp     #$0A
        bne     LA058
LA065:  lda     #$11
        sta     $DD0E
        ldx     #10
        clc
        bcc     LA077
LA06F:  lda     $DD0D
        lsr     a
        bcc     LA06F
        lsr     $A5
LA077:  lda     $DD00
        and     #$FB
        bcc     LA080
        ora     #$04
LA080:  sta     $DD00
        dex
        bne     LA06F
LA086:  lda     $DD0D
        lsr     a
        bcc     LA086
        lda     $DD00
        and     #$FB
        ora     #$04
        sta     $DD00
LA096:  lda     $DD0D
        lsr     a
        bcc     LA096
        pla
        tax
        rts

cia2_reset:
        lda     $DD0C
        and     #$7F    ; Indicate printing via cbm serial by defaut
        sta     $DD0C
        lda     #$3F    ; Reset DDR A
        sta     $DD02
        lda     $DD00
        ora     #$04    ; PA2 high
        sta     $DD00
        lda     #$10
        sta     $DD0E
        lda     #$FF    ; Reset timer
        sta     $DD04
        sta     $DD05
        lda     #$00    ; Reset DDR B
        sta     $DD03
        rts

centronics_or_rs232:
        lda     $DC0C
        cmp     #$FE
        bne     @no_rs232 ; not RS-232
        ; Print via RS-232 has been forced
        lda     #$7F      ; Setup DDR for RS232 communication
        sta     $DD03
        sta     $DD0D     ; Set interrupt mask
        lda     #$3F      ; Setup DDR for RS232 communication
        sta     $DD02
        lda     #$04      ; Set TXD line high.
        ora     $DD00
        sta     $DD00
@clc_rts:
        clc
        rts
@no_rs232:
        dec     $DD03      ; DDR contains 0 (input), set it to output ($ff)
        ; This check is super weird. When this is called the CIA has been reset by cia2_reset.
        ; which doesn't test bit 6. Bit 6 is set by a previous call to the printer interface.
        ; This means that 
        bit     $DD0C     ; ?? Serial shift register CIA2
        bvs     @clc_rts
        ;
        ; Centronics check. The presence of a Centronics printer is detected
        ; by sending an ASCII XON byte, and then checking if the printer
        ; will raise BUSY.
        ;
        lda     #$11      ; XON = resume transmission??
        jsr     centronics_send_byte
        lda     #$FF      ; Initialize timer B
        sta     $DC07
        lda     #$19      ; Start timer B
        sta     $DC0F
        lda     $DC0D
:       lda     $DD0D
        and     #$10      ; Test for FLAG
        bne     @clc_rts  ; FLAG set, then centronics printer detected
        lda     $DC0D
        and     #$02     ; Timer underflow?
        beq     :-       ; No, then loop.
        sec              ; Timeout, no centronics printer
        rts
; ----------------------------------------------------------------

; these routines turn the cartridge ROM on before,
; and turn it back off afterwards
set_io_vectors_with_hidden_rom2:
        lda     #<_new_ckout
        ldy     #>_new_ckout
        sta     $0320 ; CKOUT
        sty     $0321
        lda     #<_new_bsout
        ldy     #>_new_bsout
        sta     $0326 ; BSOUT
        sty     $0327
        lda     #<_new_clrch
        ldy     #>_new_clrch
        sta     $0322 ; CLRCH
        sty     $0323
        lda     #<_new_clall
        ldy     #>_new_clall
        sta     $032C ; CLALL
        sty     $032D
        rts

; these routines assume the cartridge ROM is mapped
set_io_vectors2:
        lda     #<new_ckout
        ldy     #>new_ckout
        sta     $0320 ; CKOUT
        sty     $0321
        lda     #<new_bsout2
        ldy     #>new_bsout2
        sta     $0326 ; BSOUT
        sty     $0327
        lda     #<new_clrch2
        ldy     #>new_clrch2
        sta     $0322 ; CLRCH
        sty     $0323
        lda     #<new_clall2
        ldy     #>new_clall2
        sta     $032C ; CLALL
        sty     $032D
        rts

; ----------------------------------------------------------------
new_ckout:
        txa
        pha
        jsr     $F30F  ; find file number in file open table
        beq     @found ; found, then jump
@noprn: pla
        tax
        jmp     $F250 ; KERNAL CKOUT

@cbm:   pla
        lda     #4
        jmp     $F279 ; set output to IEC bus

@found: jsr     $F31F ; set file par from table ($B8/$B9/$BA LA/SA/FA)
        lda     FA
        cmp     #4 ; printer
        bne     @noprn
        jsr     cent_rs232_or_cbm
        bcs     @cbm
        pla
        rts

cent_rs232_or_cbm:
        ; Check whether we will print via centronics/rs232 or cbm serial
        ;
        ; Returns:
        ;  C=0    Centonics or RS232
        ;  C=1    CBM
        ;
        ; RS232 will be used if $DC0C contains $FE, Centronics otherwise.
        jsr     cia2_reset ; Setup for CBM serial by default
        lda     $DC0C  ; Centronics check van be disabled with a POKE $DC0C,$ff
        cmp     #$FF
        beq     @rts   ; "no centronics check"
        sei
        jsr     centronics_or_rs232
        bcs     @rts  ; Return if neither Centronics nor RS232
        lda     #4
        sta     $9A
        jsr     setup_dd0c
        clc
@rts :  rts

new_bsout:
        jsr     new_bsout2
        jmp     _disable_fc3rom

new_bsout2:
        pha
        lda     $9A
        cmp     #4
        beq     @dev4
@kernal_bsout:
        pla
        jmp     $F1CA ; KERNAL BSOUT
@dev4:  bit     $DD0C         ; Printing via Centronics or RS232?
        bpl     @kernal_bsout  ; Jump to KERNAL if not.
        pla
        sta     $95
        sei
        jsr     maybe_printer_bsout_petscii
        bcs     @skip
        lda     $95
        jsr     printer_send_byte
@skip:  lda     $95
        cli
        clc
        rts

new_clall:
        jsr     new_clall2
        jmp     _disable_fc3rom

new_clrch:
        jsr     new_clrch2
        jmp     _disable_fc3rom

new_clall2:
        lda     #0
        sta     $98
new_clrch2:
        lda     #4
        ldx     #3
        cmp     $9A         ; Check if current output device is printer
        bne     @cbm
        bit     $DD0C       ; Are we printing to Centronics/RS232 or CBM serial?
        bpl     @cbm
        jsr     cia2_reset  ; Return CIA2 to normal state
        beq     @1          ; Always
@cbm:   cpx     $9A         ; Current output device
        bcs     @1
        jsr     $EDFE ; UNLISTEN
@1:     cpx     $99         ; Current input device
        bcs     @2
        jsr     $EDEF ; UNTALK
@2:     stx     $9A      ; Set current output device to screen
        lda     #0       ; Set current input device to keyboard
        sta     $99
        rts

        ; $DD0C is used as some memory location for printing mode depending
        ; on secondary address. SA 7 and 8 are not documented in the FC3 manual,
        ; SA 2 and 3 are documented, but missing here.
setup_dd0c:
        lda     SA
        cmp     #$FF
        beq     @gra
        and     #$0F
        beq     @gra
        cmp     #7
        beq     @bus
        cmp     #9		; CBM charset
        beq     @cbm
        cmp     #10     ; CBM charset reversed
        beq     @cbmr
        cmp     #8
        beq     @u8
        lda     #$C0
        .byte   $2C
@gra:   lda     #$C1    ; MPS-803 graphics mode (default)
        .byte   $2C
@bus:   lda     #$C2    ; MPS-803 business mode
        .byte   $2C
@cbm:   lda     #$C4
        .byte   $2C
@cbmr:  lda     #$C8
        .byte   $2C
@u8:    lda     #$D0
        sta     $DD0C
        rts

; PETSCII/ASCII conversion
petscii_to_ascii:
        lda     $95
        cmp     #$C0
        bcc     @1
        cmp     #$E0
        bcc     @2
        cmp     #$FF
        bne     @3
        lda     #$7E
        bne     @1
@2:     and     #$7F
        bcc     @5
@3:     and     #$BF
        bcc     @4
@1:     cmp     #$40
        bcc     @6
        cmp     #$60
        bcc     @7
        cmp     #$80
        bcc     @6
        cmp     #$A0
        bcc     @8
@4:     and     #$7F
@8:     ora     #$40
        bne     @exit
@5:     and     #$DF
        bcc     @exit
@7:     and     #$BF
        bcc     @exit
@6:     cmp     #$20
        bcs     @exit
        ora     #$80
@exit:  sta     $95
        rts

maybe_printer_bsout_petscii:
        lda     $DD0C
        cmp     #$C1   ; $C0 = no PETSCII conversion
        bcc     @rts
        lda     $DD0C
        lsr     a
        bcs     LA2D1
        lsr     a
        bcs     LA2D2
        lsr     a
        bcc     @2
@1:     jmp     LA39A
@2:     lsr     a
        bcs     @1
        lsr     a
        bcs     LA282
@rts:   rts

LA282:  lda     $95
        cmp     #$0A ; LF
        beq     LA29A
        cmp     #CR
        beq     LA29A
        cmp     #' '
        bcc     LA298
        cmp     #$80
        bcc     LA29A
        cmp     #$A0
        bcs     LA29A
LA298:  sec
        rts

LA29A:  lda     $D018
        and     #$02 ; lowercase font enabled?
        beq     :+
        jsr     to_lower
:       clc
        rts

LA2A6:  cmp     #$80
        bcc     LA2B0
        cmp     #$A0
        bcs     LA2B0
        sec
        rts

LA2B0:  lda     $DD0C
        lsr     a
        and     #$18
        bne     LA2BC
        bcc     LA2C0
        clc
        rts

LA2BC:  and     #$10
        beq     LA2C3
LA2C0:  jsr     to_lower
LA2C3:  clc
        rts

dd0c_clear_bit45:
        pha
        lda     $DD0C
        and     #$CF
        sta     $DD0C
        pla
        clc
        rts

LA2D1:  lsr     a
LA2D2:  lsr     a
        bcc     :+
        lsr     a
        lda     $95
        and     #$0F
        sta     $95
        bcc     LA319
        bcs     LA327
:       lda     $95
        cmp     #$91  ; MPS-803 temporary switch to graphics mode
        beq     @1
        cmp     #$20  ; ASCII control char<$20
        bcs     LA2A6 ; If not then jump
        cmp     #$0A  ; Line feed
        beq     dd0c_clear_bit45
        cmp     #$0C  ; MPS-803 line-feed, ends quote mode,
                      ; reverse & temporary charset switch
        beq     dd0c_clear_bit45
        cmp     #$0D  ; MPS-803 carriage return, ends quote mode,
        beq     dd0c_clear_bit45
        cmp     #$11  ; MPS-803 temporary switch to business mode
        beq     @2
        cmp     #$10  ; Set print start position
        bne     @sec_rts
        lda     $DC0C
        cmp     #$FE
        beq     @sec_rts ; RS-232
        lda     #$04  ; CBM mode?
        bne     @3 ; always
@1:     lda     #$10
        .byte   $2C
@2:     lda     #$30
        jsr     dd0c_clear_bit45
@3:     ora     $DD0C
        sta     $DD0C
@sec_rts:
        sec
        rts

LA319:  asl     a
        asl     a
        adc     $95
        asl     a
        sta     $A4
        lda     #$08
        ora     $DD0C
        bne     LA34A ;always
LA327:  clc
        adc     $A4
        sta     $95
        lda     #$1B
        jsr     printer_send_byte
        lda     #'D' ; Set horizontal tabs
        jsr     printer_send_byte
        lda     $95
        jsr     printer_send_byte
        lda     #0
        jsr     printer_send_byte
        lda     #9
        jsr     printer_send_byte
        lda     $DD0C
        and     #$F3
LA34A:  sta     $DD0C
        sec
        rts

to_lower:
        lda     $95
        cmp     #$41
        bcc     LA373
        cmp     #$5B
        bcs     LA35D
        ora     #$20
        bne     LA373
LA35D:  cmp     #$61
        bcc     LA373
        cmp     #$7B
        bcs     LA369
        and     #$DF
        bcc     LA373
LA369:  cmp     #$C1
        bcc     LA373
        cmp     #$DB
        bcs     LA373
        and     #$7F
LA373:  sta     $95
        rts

LA376:  ldy     #3
:       asl     a
        rol     $FC
        dey
        bne     :-
        rts

LA37F:  sta     $A4
        tya
        pha
        lda     $D018
        lsr     a
        and     #$01
        ora     #$1A
        sta     $FC
        lda     $A4
        jsr     LA376
        sta     $FB
        jsr     print_char_from_charrom
        pla
        tay
        rts

LA39A:  lda     $95
        cmp     #10   ; LF? print buffer contents
        beq     LA3BF
        cmp     #13   ; CR? print buffer contents
        beq     LA3BF
        jsr     petscii_to_ascii
        tya
        pha
        ldy     $033C
        lda     $95
        sta     $033D,y
        inc     $033C
        cpy     #$1D  ; buffer full?
        bne     LA3BB
        jsr     LA3BF ; print buffer_contents
LA3BB:  pla
        tay
        sec
        rts

LA3BF:  pha
        lda     $033C
        beq     LA43C
        jsr     printer_send_graphics_cmd
        tya
        pha
        lda     #0
        sta     $FC
        lda     $033C
        jsr     LA376
        sta     $FB
        lda     $DC0C
        cmp     #$FE
        bne     LA41D ; not RS-232
        txa
        pha
        ldx     #$30
LA3E1:  lda     $FB
        sec
        sbc     #$64
        tay
        lda     $FC
        sbc     #0
        bcc     LA3F4
        sta     $FC
        sty     $FB
        inx
        bne     LA3E1
LA3F4:  txa
        jsr     printer_send_byte
        ldx     #$30
LA3FA:  lda     $FB
        sec
        sbc     #10
        tay
        lda     $FC
        sbc     #0
        bcc     LA40D
        sta     $FC
        sty     $FB
        inx
        bne     LA3FA
LA40D:  txa
        jsr     printer_send_byte
        lda     $FB
        ora     #$30
        jsr     printer_send_byte
        pla
        tax
        jmp     LA427

LA41D:  lda     $FB
        jsr     printer_send_byte
        lda     $FC
        jsr     printer_send_byte
LA427:  ldy     #0
LA429:  lda     $033D,y
        jsr     LA37F
        iny
        cpy     $033C
        bne     LA429
        lda     #0
        sta     $033C
        pla
        tay
LA43C:  pla
        sta     $95
        clc
        rts

print_char_from_charrom:
        lda     #$80
        sta     $A4
@2:     lda     #0
        sta     $A5
        ldy     #7
        jsr     get_char_column
        lda     $DD0C
        lsr     a
        lsr     a
        lsr     a
        lda     $A5
        bcs     :+
        eor     #$FF    ; Reversed CBM charset
:       sta     $A5
        lda     $DC0C
        cmp     #$FE
        bne     @1 ; not RS-232
        txa
        ; Reverse the bits in $A5.
        pha
        ldx     #8
        lda     $A5
:       asl     a
        ror     $A5
        dex
        bne     :-
        pla
        tax
@1:     lda     $A5
        jsr     printer_send_byte
        lsr     $A4
        bcc     @2
        rts

pow2:  .byte   $80,$40,$20,$10,$08,$04,$02,$01

;
; Characters are stored in charrom one row at a time, but for printing we need
; them one column at a time. This routine retrieves column y of the characer
; pointed to by ($fb) and stores the result in $a5.
;
;
get_char_column:
        lda     #$33
        sta     $01
@1:     lda     ($FB),y
        and     $A4
        beq     :+
        lda     $A5
        ora     pow2,y
        sta     $A5
:       dey
        bpl     @1
        lda     #$37
        sta     $01
        rts

;
; This routine sends the command to print graphics to the printer.
;
; If RS232 is selected, the original FC3 would switch the printer language to
; that of the Apple Image Writer printers. This is not documented in the manual
; at all, so no FC3 owner could have known that RS232 support only works with
; Apple Image Writer printers.
;
; This doesn't make any sense: There exist far more RS232 printers that use the
; Epson command set than the 3 models of Image Writer printers. This was so in
; 1986 when the FC3 was released, but it is still true today, as one can still
; buy brand new dot matrix printers with RS232 interface and Epson command set
; in 2024.
;
; Therefore the imagewriter support has been placed between ifdefs. Removing it
; actually improves compatibility.
;
printer_send_graphics_cmd:
        jsr     printer_send_esc
        lda     $DC0C
        beq     esc_k
.ifdef imagewriter
        cmp     #$FE
        beq     apple_send_graphics_cmd ; RS-232
.endif

        cmp     #'0'
        bcc     esc_k
        cmp     #'['
        bcs     esc_k
        cmp     #$37 ; Graphics printing mode?
        bcc     esc_star
        cmp     #'K'
        bcc     esc_k
        bcs     sendb ; always
esc_star:
        pha
        lda     #'*' ; Print graphics command
        jsr     printer_send_byte
        pla
        and     #$0F ; Printing density
        .byte   $2C
printer_send_esc:
         lda     #$1B
        .byte   $2C
esc_k:  lda     #'K'
sendb:  jmp     printer_send_byte

.ifdef imagewriter
apple_send_graphics_cmd:
        lda     #'N'  ; Sets printing density to 80 dpi on Image Writer.
        jsr     printer_send_byte
        jsr     printer_send_esc
        lda     #'G' ;  Enters graphics mode on Image Writer.
        jsr     printer_send_byte
        lda     #'0'
        jmp     printer_send_byte
.endif

; ----------------------------------------------------------------
