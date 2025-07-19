;*****************************************************************************
;  Final Cartridge III reconstructed source code
;
;  This file implements the functions of the reset menu of the freezer
;*****************************************************************************

      .setcpu "6502x"

.include "../core/kernal.i"
.include "../core/fc3ioreg.i"
.include "../core/macros.i"
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
      ; Interrupts are off
      jsr  detect_c128
      bne  :+
      ldx  #0
      stx  tmpvar1
      jsr  backup_to_vdc
      lda  #$41
      jmp  @4
:     jsr  detect_reu
      lda  #$01
      bcc  @4
      ldx  #0
      stx  tmpvar1
      jsr  backup_to_reu
      lda  #$81
@4:   ldx  #$FF
      txs
      pha
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
      bit  $01FF
      bmi  @3
      bvs  @1                           ; Monitor wille exit to freezer
      ; Only initialize BASIC if the monitor will exit to BASIC
      jsr  $E453                        ; Routine: Set BASIC vectors (case 0x300..case 0x309)
      pla                               ; totally crazy, but $E3BF requires empty stack
      jsr  $E3BF                        ; Routine: Set USR instruction and memory for BASIC
      lda  #$01
      pha
      bne  @2
@1:   jsr  mem_ab_for_monitor_vdc       ; Clears Z
      bne  @2                           ; always
@3:   jsr  mem_ab_for_monitor_reu
@2:   lda  #>(monitor_frozen-1)
      pha
      lda  #<(monitor_frozen-1)
      pha
      jmp  _enable_fcbank0


;
; Detects whether we run on a C128 in C64 mode
;
; Returns:
;  A<>0 - Commodore 64
;  A=0  - Commodore 128
;  Z=0  - Commodore 64
;  Z=1  - Commodore 128
;
detect_c128:
      lda  #$fe
      sta  $d02f
      sta  $d030
      eor  $d02f
      eor  $d030
      eor  #$fe
      bne  @x
      ; A=0
      sta  $d02f
      sta  $d030
      eor  $d02f
      eor  $d030
      eor  #$04
@x:   rts

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


vdc_memory_store:
      ldy     #$00
      lda     (tmpptr_a),y
      jsr     vdc_reg_store
      inc     tmpptr_a
      rts

backup_to_vdc:
        ; Backup $D000..$D02E to $F3D1..$F3FF in VDC
      lda     #>$F3D1
      ldx     #$12
      jsr     vdc_reg_store
      lda     #<$F3D1
      inx
      jsr     vdc_reg_store
      lda     #$00
      sta     tmpptr_a
      lda     #$D0
      sta     tmpptr_a+1
      ldx     #$1F
:     jsr     vdc_memory_store
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
:     jsr     vdc_memory_store
      bne     :-
      inc    tmpptr_a+1
      lda    tmpptr_a+1
      cmp    #$DC
      bne    :-

      ; Backup $0000..$07FF to $F800..$FFFF in VDC
      lda     #$00
      sta     tmpptr_a+1
      ldx     #$1F
:     jsr     vdc_memory_store
      bne     :-
      inc    tmpptr_a+1
      lda    tmpptr_a+1
      cmp    #$08
      bne    :-
      rts

mem_ab_for_monitor_vdc:
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
      beq     mem_ab_size
mem_ab_for_monitor_reu:
      jsr     reu_memab_setup
mem_ab_size:
      lda     #__FREEZERZP_SIZE__
      sta     $76
      lda     #__freezer_restore_1_SIZE__
      sta     $79
      rts


detect_reu:
      ; Exchange 255 bytes of c64 and reu memory
      jsr @xchg
      ; D012 == 0?
      lda $D012
      clc
      beq :+
      sec ; REU found
:
      ; Exchange back
@xchg:
      jsr reu_detect_setup
:     ldx $D012
      bne :-
      ldx #%10010010
      stx $DF01
      rts

reu_memab_setup:
       ldy  #reu_memab_command-reu_commands
       skip_2b_instr

reu_detect_setup:
       ldy  #reu_detect_command-reu_commands
       skip_2b_instr

backup_to_reu:
       ldy   #0
:      ldx   reu_commands,y
       bmi   @x
       iny
       lda   reu_commands,y
       iny
       sta   $DF00,x
       bpl   :- ; always
@x:    rts

reu_commands:
        ; Restore $D000..$D02E from $FFF3D1..$FFF3FF in REU
        .byte   $02,<$D000
        .byte   $03,>$D000
        .byte   $04,<$FFF3D1
        .byte   $05,>$FFF3D1
        .byte   $06,^$FFF3D1
        .byte   $07,<$002F
        .byte   $08,>$002F
        .byte   $01,$90        ; start immediate transfer from C64 to reu
        ; Restore $D800..$DBFF
        .byte   $02,<$D800
        .byte   $03,>$D800
        .byte   $07,<$0400
        .byte   $08,>$0400
        .byte   $01,$90        ; start immediate transfer from C64 to reu
        ; Restore $0000..$07FF
        .byte   $02,<$0000
        .byte   $03,>$0000
        .byte   $07,<$0800
        .byte   $08,>$0800
        .byte   $01,$90        ; start immediate transfer from C64 to reu
        .byte   $FF
reu_detect_command:
        .byte   $02,<$0000
        .byte   $03,>$0400
        .byte   $04,>$000000
        .byte   $05,>$000000
        .byte   $06,<$000000
        .byte   $07,<$00FF
        .byte   $08,>$00FF
        .byte   $FF
reu_memab_command:
        .byte   $02,<$0070
        .byte   $03,>$0070
        .byte   $04,<freezer_mem_a
        .byte   $05,$F8
        .byte   $06,$FF
        .byte   $07,<6
        .byte   $08,>6
        .byte   $01,$91
        .byte   $ff


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

