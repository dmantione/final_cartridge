;*****************************************************************************
;  Final Cartridge III reconstructed source code
;
;  This file implements the functions of the reset menu of the freezer
;*****************************************************************************

      .setcpu "6502x"

.include "../core/kernal.i"
.include "../core/fc3ioreg.i"
.include "persistent.i"

.import _jmp_bank,_enable_fcbank0,_disable_fc3rom_set_01
.import monitor_frozen
.import freezer_screenshot_prepare
.importzp freezer_mem_a

.importzp __FREEZERZP_SIZE__
.importzp __freezer_restore_1_SIZE__

.importzp tmpvar1,tmpptr_a

.segment "freezer_monitor"

init_load_and_basic_vectors = $8021

.global freezer_goto_monitor
freezer_goto_monitor:
      jsr  detect_c128
      bcc  :+
      ldx  #0
      stx  tmpvar1
      jsr  backup_to_vdc
:     ldx  #$FF
      txs
      jsr  IOINIT_direct

      ; Avoid use of RESTOR since it writes to RAM under ROM
      ;jsr  RESTOR_direct
      ldy  #$1F
:     lda  $FD30,y
      sta  $0314,y
      dey
      bpl :-

      lda  #$00
      tay
:     sta  $0002,y                      ; Clear zeropage
      sta  $0200,y                      ; Clear $02xx
      iny
      bne  :-
      ldx  #<$A000
      ldy  #>$A000
      jsr  $FD8D                        ; Set top, bottom of memory and screen base
      jsr  CINT_direct
      jsr  detect_c128
      bcs  @1                           ; Monitor wille exit to freezer
      ; Only initialize BASIC if the monitor will exit to BASIC
      jsr  $E453                        ; Routine: Set BASIC vectors (case 0x300..case 0x309)
      jsr  $E3BF                        ; Routine: Set USR instruction and memory for BASIC
      lda  #$01                         ; Monitor entry reason
      bne  @2
@1:   jsr  mem_ab_for_monitor
      lda  #$41
@2:   pha
      lda  #>(monitor_frozen-1)
      pha
      lda  #<(monitor_frozen-1)
      pha
      jmp  _enable_fcbank0


detect_c128:
      clc
      lda  #$fe
      sta  $d02f
      sta  $d030
      eor  $d02f
      eor  $d030
      eor  #$fe
      bne  @noc128
      ; A=0
      sta  $d02f
      sta  $d030
      eor  $d02f
      eor  $d030
      eor  #$04
      bne  @noc128
      sec
@noc128:
      rts

; stores a byte in A into VDC register X
vdc_reg_store:
      ldy     #63 ; VDC should have time for processing at least once per
                  ; scanline, this is multiple scanlines in cycles, so
                  ; should be enough.
      stx     $d600
:     dey
      beq     @error
      bit     $d600
      bpl     :-
      sta     $d601
@error:
      rts

backup_to_vdc:
        ; Backup $D000..$D02E to $F3D1..$F3FF in VDC
      lda     #$F3
      ldx     #$12
      jsr     vdc_reg_store
      lda     #$D1
      inx
      jsr     vdc_reg_store
      lda     #$00
      sta     tmpptr_a
      lda     #$D0
      sta     tmpptr_a+1
      ldx     #$1F
:     ldy     #$00
      lda     (tmpptr_a),y
      jsr     vdc_reg_store
      inc     tmpptr_a
      lda     tmpptr_a
      cmp     #$2F
      bne     :-
        ; Backup $D800..$DBFF to $F400..$F7FF in VDC
;      lda     #$F4
;      ldx     #$12
;      jsr     vdc_reg_store
;      lda     #$00
;      inx
;      jsr     vdc_reg_store
      lda     #$00
      sta     tmpptr_a
      lda     #$D8
      sta     tmpptr_a+1
      ldx     #$1F
:     ldy     #$00
      lda     (tmpptr_a),y
      jsr     vdc_reg_store
      inc    tmpptr_a
      bne     :-
      inc    tmpptr_a+1
      lda    tmpptr_a+1
      cmp    #$DC
      bne    :-

      ; Backup $0000..$07FF to $F800..$FFFF in VDC
      lda     #$00
      sta     tmpptr_a+1
      ldx     #$1F
:     ldy     #$00
      lda     (tmpptr_a),y
      jsr     vdc_reg_store
      inc    tmpptr_a
      bne     :-
      inc    tmpptr_a+1
      lda    tmpptr_a+1
      cmp    #$08
      bne    :-
      rts

mem_ab_for_monitor:
      ; Get freezer mem a/b locations
      lda     #$F8
      ldx     #$12
      jsr     vdc_reg_store
      inx
      lda     #freezer_mem_a
      jsr     vdc_reg_store
      ldy     #0
      ldx     #$1F
      stx     $D600
      ldx     #$00
:     bit     $D600   ; No point for a timeout, all is lost if VDC fails
      bpl     :-
      lda     $D601
      sta     $70,x
      inx
      cpx     #6
      bne     :-
      lda     #__FREEZERZP_SIZE__
      sta     $76
      lda     #__freezer_restore_1_SIZE__
      sta     $79
      rts

.segment "freezer_reset"

.global freezer_zero_fill
freezer_zero_fill:
      ldy  #$00
      sty  $AC
      lda  #$08
      sta  $AD
      lda  #$33
      sei
      sta  $01
      tya
:     sta  ($AC),y
      iny
      bne  :-
      inc  $AD
      bne  :-
c64_reset:
      lda  #>(START-1)
      pha
      lda  #<(START-1)
      pha
      lda  #$37
      sta  $01
      jmp  _enable_fcbank0

.global write_mg87_and_reset
write_mg87_and_reset:
      ldx  #sizeof_MG87 - 1
:     lda  MG87,x
      sta  $CFFC,x
      dex
      bpl  :-
      bmi  c64_reset ; always

MG87: .byte "MG87"
sizeof_MG87 = .sizeof(MG87)

