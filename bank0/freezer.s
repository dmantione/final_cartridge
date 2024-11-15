; ----------------------------------------------------------------
; Freezer Entry
; ----------------------------------------------------------------
; In Ultimax mode, we have the following memory layout:
; $8000-$9FFF: bank 0 lo
; $E000-$FFFF: bank 0 hi
; This code is mapped into bank 0 hi, and the vectors appear
; at the very end of this bank.
; The code here only does some minimal saving of state, then
; jumps to a different bank.

.include "persistent.i"
.include "../core/fc3ioreg.i"

.segment "freezer"

;
; The freeze NMI handler starts execution in bank 0, then continues in bank 3.
;
freezer:
;        sei ; not necessary, already cleared by cpu

        ; it's rare, but you should be able to freeze a program that uses decimal mode
        cld

        pha
        lda     $00
        pha
        lda     #$2F
        sta     $00 ; default value of processor port DDR
        lda     $01
        ora     #$20 ; cassette motor off - but don't store
        pha
        lda     #$37
        sta     $01 ; processor port defaut value


        ; Activate Ultimax mode and bank 3, NMI line stays active
        lda     #fcio_bank_3|fcio_c64_ultimaxmode
        sta     fcio_reg ; NMI = 1, GAME = 1, EXROM = 0
        ; Code execution now continues in bank 3!

.assert *= $FFF8 , error, "CPU vectors not at correct memory location!"
        ; This is to make space in bank 3 for a bne freezer_nmi_handler
        nop
        nop
; catch IRQ, NMI, RESET
        .word freezer ; NMI
        .word freezer ; RESET
        .word freezer ; IRQ

