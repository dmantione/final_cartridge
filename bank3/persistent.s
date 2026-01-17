; --------ø--------------------------------------------------------
; I/O Area ROM
; ----------------------------------------------------------------
; This is a max. 512 byte section that lives at $1E00-$1FFF of
; bank 0 of the ROM, and will also be mapped into the I/O extension
; area at $DE00-$DFFF, so it's always visible.

; It contains soms trampolines to be able to switch from/to Ultimax
; mode for the freezer and contains the autofire code for the joystick

      .setcpu "6502x"

.include "../core/fc3ioreg.i"
.include "../core/kernal.i"

.import freezer_init
.import freezer_exec_menu
.import freezer_update_spritepointers
.import show_view_menu
.import draw_menu
.import highlight_selected_menu
.import pset
.import listen_second
;.import freezer_exec_bank

.segment "romio1"
;
;
; ROMIO1 area ($DE00)
;
;

fc_bank_id:
      .byte fcio_bank_3|fcio_nmi_line

;
; Jump into a bank of the FC3 ROM
;
; Jumps to a routine in the FC3 ROM of which the address is on the stack
; and the bank number in A.
;

.global _jmp_bank
_jmp_bank:
        sta  fcio_reg
        rts

.global _enable_fcbank0
_enable_fcbank0: ; $DE05
        pha
        lda     #fcio_bank_0|fcio_c64_16kcrtmode|fcio_nmi_line
a_to_fcio_pla:
        sta     fcio_reg
        pla
        rts

; _disable_fc3rom:        Hides the FC3 ROMS from memory
; _disable_fc3rom_set_01: Stores Y into $01 and hides the FC3 ROMS from memory
;

.global _disable_fc3rom_set_01
_disable_fc3rom_set_01:; $DE0D
        sty     $01
.global _disable_fc3rom
_disable_fc3rom: ; $DE0F
        pha
        lda     #fcio_bank_0|fcio_c64_crtrom_off|fcio_nmi_line
        bne     a_to_fcio_pla			; always taken

.global _freezer_pset
_freezer_pset:
      jsr  call_pset_in_bank0
      ; fall through to freezer_run
.global _freezer_run
_freezer_run:
      ldy #$35
      bne _disable_fc3rom_set_01 ; always taken

call_pset_in_bank0:
      lda  #>pset
      pha
      lda  #<pset
      pha
      bne  _enable_fcbank0 ; always taken

;
; Do an "lda($AE),y" with ROMs disabled
; We have to use #$35, because in 16K cartridge mode #$36 keeps ROM at $A000
; enabled. Because #$35 disables the KERNAL, interrupts must be off.
;

.global _load_ptr2_rom_hidden
_load_ptr2_rom_hidden:
        sei
        lda  #$35
        sta  $01
        lda  ($AE),y
        inc  $01
        inc  $01
        cli
        ora  #$00 ; set flags
        rts

.global _swap_ptr2_ptr3_rom_hidden
_swap_ptr2_ptr3_rom_hidden:
        sei
        lda  #$35
        sta  $01
        lda  ($AE),y
        pha
        lda  ($C1),y
        sta  ($AE),y
        pla
        sta  ($C1),y
        lda  #$37
        sta  $01
        cli
        rts


.global _freezer_upd_sprptr_16k
_freezer_upd_sprptr_16k:
      lda  #fcio_bank_3|fcio_c64_16kcrtmode
      sta  fcio_reg
      jsr  freezer_update_spritepointers  ; jump into bank 3
ultimax_bank3_rts:
      lda  #fcio_bank_3|fcio_c64_ultimaxmode
      sta  fcio_reg
      rts

;
; Go to ultimax mode, execute the freezer menu and return to 16K mode
;

.global freezer_ultimax_exec_menu
freezer_ultimax_exec_menu:
      jsr  ultimax_bank3_rts
      jsr  freezer_exec_menu
bank3_16kmode:
      ldx  #fcio_bank_3|fcio_c64_16kcrtmode
      stx  fcio_reg
      rts

;
; Go to 16k mode, execute $bc5b and return to ultimax mode
;
.global _show_view_menu
_show_view_menu:
      jsr  bank3_16kmode
      jsr  show_view_menu
      jsr  ultimax_bank3_rts
      jmp  freezer_exec_menu

;
; Go to ultimax mode, execute $fbe4 and return to 16K mode
;

.global ultimax_draw_menu
ultimax_draw_menu:
      jsr  ultimax_bank3_rts
      jsr  draw_menu
      jmp  bank3_16kmode



.global autofire_ldy_dc01
autofire_ldy_dc01:
      pha
      tya
      jsr  autofire_lda_dc01
autofire_ldy_exit:
      tay
      pla
      cpy  #0
      rts

.global autofire_ldx_dc01
autofire_ldx_dc01:
      pha
      txa
      jsr  autofire_lda_dc01
autofire_ldx_exit:
      tax
      pla
      cpx  #0
      rts

.global autofire_ldy_dc00
autofire_ldy_dc00:
      pha
      tya
      jsr  autofire_lda_dc00
      jmp  autofire_ldy_exit

.global autofire_ldx_dc00
autofire_ldx_dc00:
      pha
      txa
      jsr  autofire_lda_dc00
      jmp  autofire_ldx_exit

      cpy  #$01
      beq  autofire_lda_dc01
      bne  autofire_lda_dc00
      cpx  #$01
      beq  autofire_lda_dc01
.global autofire_lda_dc00
autofire_lda_dc00:
      lda  $DC00                        ; Data port A #1: keyboard, joystick, paddle, optical pencil
      jmp  autofire_chkbutton

.global autofire_lda_dc01
autofire_lda_dc01:
      lda  $DC02                        ; Data direction register port A #1
      pha
      lda  #$00
      sta  $DC02                        ; Data direction register port A #1
      lda  $DC01                        ; Data port B #1: keyboard, joystick, paddle
      sta  $0122                        ; Save to tmp location in stack memory
      pla
      sta  $DC02                        ; Data direction register port A #1
      lda  $0122                        ; Load from tmp location
      pha
      and  #$10                         ; Fire button pressed?
      beq autofire_button_pressed
      pla
      lda  $DC01                        ; Data port B #1: keyboard, joystick, paddle
      rts

      lda  $0122
autofire_chkbutton:
      pha
      and  #$10                         ; Fire button pressed?
      beq  autofire_button_pressed
pla_rts:
      pla
      rts

autofire_button_pressed:
      lda  $0120
      bne  autofire_signal
      dec  $0121
      bne  pla_rts
      lda  #$02 
      sta  $0120
      sta  $0121
      bne  pla_rts                      ; Always
autofire_signal:
      dec  $0121
      beq  autofire_signal_press
      pla
      ora  #$10                         ; Unpress the button
      rts

autofire_signal_press:
      lda  #$00
      sta  $0120
      lda  #$01
      sta  $0121
      pla
      rts


.segment "romio2"
      .byte "REU REU REU REU REU REU REU U2CI"


;
; Go to ultimax mode, execute $fb98 and return to 16K mode
;

.global ultimax_highlight_selected_menu
ultimax_highlight_selected_menu:
      jsr  ultimax_bank3_rts
      jsr  highlight_selected_menu
      jmp  bank3_16kmode

.global freezer_set_c64and_fc3_rts
freezer_set_c64and_fc3_rts:
      sta  fcio_reg
      sty  $01
      rts

.global lda_txtptr_indy
lda_txtptr_indy:
      dec $01
      lda (TXTPTR),y
      inc $01
      rts

.global out_inc_txtptr
.global inc_txtptr

out_inc_txtptr:
        jsr     IECOUT
inc_txtptr:
        inc     TXTPTR
        bne     :+
        inc     TXTPTR+1
:       rts

.global sd2iec_createimg
sd2iec_createimg:
      ldy #1
      jsr lda_txtptr_indy
      cmp #':'
      beq @1
@e:   jmp @err
@1:   iny
      jsr lda_txtptr_indy
      beq @e
      cmp #'.'
      bne @1
      iny
      jsr lda_txtptr_indy
      cmp #'D'
      bne @e
      iny
      jsr lda_txtptr_indy
      ldx #strpd64 - strings
      cmp #'6'
      beq @ok
      ldx #strpd71 - strings
      cmp #'7'
      beq @ok
      ldx #strpd81 - strings
      cmp #'8'
      bne @e
@ok:  ; Send filename, including image extension and comma, i.e. image.d64,
      txa
      pha
      lda #$F2 ; Open channel 2
      jsr listen_second
      ldy #1
:     jsr lda_txtptr_indy
      jsr IECOUT
      iny
      cmp #','
      bne :-
      ldx #strpw - strings
      jsr outstr
      jsr UNLSTN
      lda #$6F
      jsr listen_second
      pla
      tax
      jsr outstr
      jsr UNLSTN
      lda #$62
      jsr listen_second
      ; Write a byte
      lda #0
      jsr IECOUT
      jsr UNLSTN
      lda #$E2 ; Close channel 2
      jsr listen_second
      jsr UNLSTN
      ; Chdir to image
      lda #$6F
      jsr listen_second
      lda #'C'
      jsr IECOUT
      lda #'D'
      jsr IECOUT
      ldy #1
:     jsr lda_txtptr_indy
      cmp #','
      beq :+
      jsr IECOUT
      iny
      jmp :-
:     jsr UNLSTN
      clc
      rts
;      jmp @r
@err:
      sec
;@r:
      rts

out:  jsr IECOUT
      inx
outstr:
      lda strings,x
      bne out
      rts

strings:
strpw:      .asciiz "P,W"
strpd64:    .byte 'P',2,$ff,$aa,$02,0
strpd71:    .byte 'P',2,$55,$5b,$05,0
strpd81:    .byte 'P',2,$ff,$7f,$0c,0
strcd:      .asciiz "CD"

