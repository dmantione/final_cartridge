; ----------------------------------------------------------------
; Monitor
; ----------------------------------------------------------------
;
; Data input and dumping:
;
; | out | in  | description
; +-----+-----+---------------
; |  M  |  :  | 8 hex bytes
; |  I  |  '  | 32 PETSCII characters
; |  EC |  [  | 1 binary byte (character data)
; |  ES |  ]  | 3 binary bytes (sprite data)
; |  D  |  ,  | disassemble
; |  R  |  ;  | registers
;
; Other commands:
;
; "F"/"H"/"C"/"T" - find, hunt, compare, transfer
; "A" - assemble
; "G" - run code
; "$" - convert hex to decimal
; "#" - convert decimal to hex
; "X" - exit monitor
; "B" - set cartridge bank (0-3) to be visible at $8000-$BFFF
; "O" - set bank
; "L"/"S" - load/save file
; "@" - send drive command
; "*R"/"*W" - read/write sector
; "P" - set output to printer
;
; Unique features of this monitor include:
; * "I" command to dump 32 PETSCII characters, which even renders
;   control characters correctly.
; * F3/F5 scroll more lines in (disassembly, dump, ...) on either
;   the top or the bottom of the screen. This includes backwards
;   disassembly.
; * "OD" switches all memory dumps/input to the drive's memory.
; * "B" command to introspect cartridge ROM

.linecont +

.include "../core/kernal.i"
.include "../core/fc3ioreg.i"

.ifdef CART_FC3
.include "persistent.i"
.else
.ifdef MACHINE_C64
_basic_warm_start := $E37B
.elseif .defined(MACHINE_TED)
_basic_warm_start := $800A
.endif
.endif

; from init
.import init_load_and_basic_vectors

; from vectors
.import jfast_format
.import print_dir

; from printer
.import set_io_vectors
.import set_io_vectors_with_hidden_rom

; from basic
.import set_irq_and_kbd_handlers
.import uninstall_kbd_handler
.import print_dir

; from editor
.import scroll_screen_up

; from drive
.import cmd_channel_listen
.import listen_second
.import command_channel_talk
.import talk_second
.import print_line_from_drive
.import close_ch2

; from constants
.import pow10lo
.import pow10hi

; from linker
.import restart_freezer

.import new_load

.global monitor
.global monitor_frozen

.ifdef MACHINE_C64
zp1             := $C1
zp2             := $C3
zp3             := $FF
CHARS_PER_LINE := 40
DEFAULT_BANK := $37
.endif

.ifdef MACHINE_TED
zp1             := $60
zp2             := $62
zp3             := $64
CHARS_PER_LINE := 40
DEFAULT_BANK := 0
.endif

tmp3            := BUF + 3
tmp4            := BUF + 4
num_asm_bytes   := BUF + 5
tmp6            := BUF + 6
prefix_suffix_bitfield := BUF + 7
tmp8            := BUF + 8
tmp9            := BUF + 9
tmp10           := BUF + 10
tmp11           := BUF + 11
tmp12           := BUF + 12
tmp13           := BUF + 13
tmp14           := BUF + 14
tmp16           := BUF + 16
tmp17           := BUF + 17
tmp_opcode      := BUF + 18

reg_pc_hi       := ram_code_end + 5
reg_pc_lo       := ram_code_end + 6
reg_p           := ram_code_end + 7

registers       := ram_code_end + 8
reg_a           := ram_code_end + 8
reg_x           := ram_code_end + 9
reg_y           := ram_code_end + 10
reg_s           := ram_code_end + 11

irq_lo          := ram_code_end + 12
irq_hi          := ram_code_end + 13

entry_type      := ram_code_end + 14
command_index   := ram_code_end + 15 ; index from "command_names", or 'C'/'S' in EC/ES case
bank            := ram_code_end + 16
disable_f_keys  := ram_code_end + 17
tmp1            := ram_code_end + 18
tmp2            := ram_code_end + 19
cartridge_bank  := ram_code_end + 20

.global et_brk,et_call,et_monitor_vdc,et_monitor_reu

et_brk          := $00
et_call         := $01
et_monitor_vdc  := $40
et_monitor_reu  := $80

; Freezer variables
tmpvar1         := $90
tmpptr_a        := $91
tmpvar2         := $93
freezer_mem_a   := $d1
freezer_mem_a_val := $d3
freezer_mem_a_size := $67
freezer_mem_b   := $d4
freezer_mem_b_val := $d3
freezer_mem_b_size := $57
freezer_vicii_backup := $F3D1

.segment "monitor_a"

.import __monitor_ram_code_LOAD__
.import __monitor_ram_code_RUN__

.import __mnemos1_RUN__
.import __mnemos2_RUN__

.import __asmchars1_RUN__
.import __asmchars2_RUN__

monitor_frozen:
        jsr init_load_and_basic_vectors
        pla

monitor:
        sta entry_type
.ifdef MACHINE_TED
; change F keys to return their code, like on the C64
; http://plus4world.powweb.com/software/Club_Info_53
        ldx #7
:       lda #1
        sta $055f,x ; set length of string to 1
        lda $dc41,x ; table of F key codes
        sta $0567,x ; set as strings
        dex
        bpl :-
.endif

        lda     #<brk_entry
        sta     CBINV
        lda     #>brk_entry
        sta     CBINV + 1 ; BRK vector
        lda     #DEFAULT_BANK
        sta     bank
.ifdef CART_FC3
        lda     #fcio_nmi_line|fcio_c64_crtrom_off
        sta     cartridge_bank ; by default, hide cartridge
.endif
        ldx     #ram_code_end - ram_code - 1
:       lda     __monitor_ram_code_LOAD__,x
        sta     __monitor_ram_code_RUN__,x
        dex
        bpl     :-
        brk ; <- nice!

.segment "monitor_ram_code"
; code that will be copied to $0220
ram_code:

.ifndef MACHINE_TED
load_byte_ram:
; read from memory with a specific ROM and cartridge config
.ifdef CART_FC3
        sta     fcio_reg ; set cartridge config
        pla
.endif
        sta     R6510 ; set ROM config
        lda     (zp1),y ; read
enable_all_roms:
        pha
        lda     #DEFAULT_BANK
        sta     R6510 ; restore ROM config
.ifdef CART_FC3
        lda     #fcio_nmi_line | fcio_c64_16kcrtmode | fcio_bank_0
        sta     fcio_reg
.endif
        pla
        rts
.endif

disable_rom_rti:
.ifdef CART_FC3
        jsr     _disable_fc3rom
.endif
.ifdef MACHINE_C64
        sta     R6510
.endif
.ifdef MACHINE_TED
        stx     tmp1
        tax
        sta     $fdd0,x
        ldx     tmp1
.endif
        lda     reg_a
_rti:   rti

brk_entry:
.ifdef MACHINE_TED
        sta $fdd0
.else
        jsr     enable_all_roms
.endif
        jmp     brk_entry2
ram_code_end:

.segment "monitor_b"

;frozen_regtable: .byte <reg_x,$40,$80,<reg_a,<reg_p,<reg_pc_lo,<reg_pc_hi

frozen_regtable: .byte <reg_pc_hi,<reg_pc_lo,<reg_p,<reg_a,$80,<bank,<reg_x

brk_entry2:
        cld ; <- important :)
        pla
        sta     reg_y
        pla
        sta     reg_x
        pla
        sta     reg_a
        pla
        sta     reg_p
        pla
        sta     reg_pc_lo
        pla
        sta     reg_pc_hi
        tsx
        stx     reg_s
        lda     CINV
        sta     irq_lo
        lda     CINV+1
        sta     irq_hi
        jsr     set_irq_and_kbd_handlers
.ifdef CART_FC3
        jsr     set_io_vectors
.endif
        jsr     print_cr
        lda     #$3F
        bit     entry_type
        bmi     @reu
        bvs     @vdc
        beq     :+
        jmp     @c
:       lda     #'B'
        .byte   $2C ; skip
@reu:   lda     #'R'
        jmp     @c
@vdc:
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
:       bit     $D600   ; No point for a timeout, all is lost if VDC fails
        bpl     :-
        lda     $D601
        sta     $70,x
        inx
        cpx     #6
        bne     :-
        lda     #freezer_mem_a_size
        sta     $76
        lda     #freezer_mem_b_size
        sta     $79

        ; Get original y register and stack pointer
        lda     #78
        clc
        adc     $73
        ldx     #$13
        jsr     vdc_reg_store  ; A,X,C preserved
        lda     #$F8
        adc     $74
        dex
        jsr     vdc_reg_store
        ldx     #$1F
        jsr     vdc_reg_load
        sta     reg_y
        jsr     vdc_reg_load
        jsr     vdc_reg_load
        sta     reg_s

        ; Get original bank, accumulator and flags
        adc     #3
        ldx     #$13
        jsr     vdc_reg_store
        lda     #$F9
        dex
        jsr     vdc_reg_store
        ldx     #$1F
        jsr     vdc_set_addreg
        ldy     #.sizeof(frozen_regtable)
@l:     bit     $d600
        bpl     @l
        lda     $d601
        ldx     frozen_regtable-1,y
        bmi     @d
        cpx     #<bank
        bne     :+
        and     #$07
        ora     #$40
:       sta     $0200,x
@d:     dey
        bne     @l

        lda     #'V'
        .byte   $2C ; skip
@c:     lda     #'C'
        ldx     #'*'
        jsr     print_a_x
        clc
        lda     reg_pc_lo
        adc     #$FF
        sta     reg_pc_lo
        lda     reg_pc_hi
        adc     #$FF
        sta     reg_pc_hi ; decrement PC
        lda     FA
        and     #$FB
        sta     FA
        lda     entry_type
        and     #$C0
        sta     entry_type
.ifdef MACHINE_C64
        lda     #$80
        sta     RPTFLG ; enable key repeat for all keys
.endif
        bne     dump_registers ; always


; ----------------------------------------------------------------
; "R" - dump registers
; ----------------------------------------------------------------
cmd_r:
        jsr     basin_cmp_cr
        bne     syntax_error
dump_registers:
        ldx     #0
        beq     @2
@1:     inx
        jsr     BSOUT
@2:     lda     s_regs,x ; "PC  IRQ  BK AC XR YR SP NV#BDIZC"
        bne     @1

dump_registers2:
        ldx     #';'
        jsr     print_dot_x
        lda     reg_pc_hi
        jsr     print_hex_byte2 ; address hi
        lda     reg_pc_lo
        jsr     print_hex_byte2 ; address lo
        jsr     print_space
        lda     irq_hi
        jsr     print_hex_byte2 ; IRQ hi
        lda     irq_lo
        jsr     print_hex_byte2 ; IRQ lo
        jsr     print_space
        lda     bank
        bpl     @3
.ifdef MACHINE_C64
        cmp     #$81
        beq     @vdc
.endif
        ; drive
        lda     #'D'
        ldx     #'R'
        bne     @2
@vdc:   lda     #'V'
        ldx     #'D'
@2:     jsr     print_a_x
        bne     @1 ; negative bank means drive ("DR")
@3:
.ifdef MACHINE_C64
        tax
        and     #$40
        beq     :+
        lda     #'F'
        jsr     BSOUT
        txa
        and     #$F
        jsr     digit_to_ascii
        jsr     BSOUT
        bcc     @1 ; always
:
.endif
        txa
        and     #$0F
        jsr     print_hex_byte2 ; bank
@1:     ldy     #$100 - 4
:       jsr     print_space
        lda     registers - ($100 - 4),y
        jsr     print_hex_byte2 ; registers...
        iny
        bne     :-
        jsr     print_space
        lda     reg_p
        jsr     print_bin
        beq     input_loop ; always


syntax_error:
        lda     #'?'
        .byte   $2C
print_cr_then_input_loop:
        lda     #CR
        jsr     BSOUT
input_loop:
        ldx     reg_s
        txs
        lda     #0
        sta     disable_f_keys
        jsr     print_cr_dot
input_loop2:
        jsr     basin_if_more
        cmp     #'.'
        beq     input_loop2 ; skip dots
        cmp     #' '
        beq     input_loop2 ; skip spaces
        ldx     #command_names_end - command_names - 1
:       cmp     command_names,x
        beq     :+
        dex
        bpl     :-
        bmi     syntax_error ; always
:       stx     command_index
        lda     function_table_h,x
        pha
        lda     function_table_l,x
        pha
        rts

; ----------------------------------------------------------------
; "EC"/"ES"/"D" - dump character or sprite data
; ----------------------------------------------------------------
cmd_e:
        jsr     BASIN
        cmp     #'C'
        beq     cmd_mid2
        cmp     #'S'
        beq     cmd_mid2
        bmi     syntax_error

fill_kbd_buffer_with_csr_right:
        lda     #CSR_UP
        ldx     #CR
        jsr     print_a_x
        lda     #CSR_RIGHT
        ldx     #0
:       sta     KEYD,x ; fill kbd buffer with 7 CSR RIGHT characters
        inx
        cpx     #7
        bne     :-
        stx     NDX ; 7
        jmp     input_loop2

cmd_mid2:
        sta     command_index ; write 'C' or 'S'

; ----------------------------------------------------------------
; "M"/"I"/"D" - dump 8 hex byes, 32 ASCII bytes, or disassemble
;               ("EC" and "ES" also end up here)
; ----------------------------------------------------------------
cmd_mid:
        jsr     get_hex_word
        jsr     basin_cmp_cr
        bne     LAC80 ; second argument
        jsr     copy_zp2_to_zp1
        jmp     LAC86

is_h:   jmp     LAEAC

; ----------------------------------------------------------------
; "F"/"H"/"C"/"T" - find, hunt, compare, transfer
; ----------------------------------------------------------------
cmd_fhct:
        jsr     get_hex_word
        jsr     basin_if_more
LAC80:  jsr     swap_zp1_and_zp2
        jsr     get_hex_word3
LAC86:  lda     command_index
        beq     is_mie ; 'M' (hex dump)
        cmp     #command_index_i
        beq     is_mie ; 'I' (ASCII dump)
        cmp     #command_index_d
        beq     is_d ; 'D' (disassemble)
        cmp     #command_index_f
        beq     is_f ; 'F' (fill)
        cmp     #command_index_h
        beq     is_h ; 'H' (hunt)
        cmp     #'C'
        beq     is_mie ; 'EC'
        cmp     #'S'
        beq     is_mie ; 'ES'

; ----------------------------------------------------------------
; "C" - compare
; ----------------------------------------------------------------
        jsr     check_end
        bcs     :+
syn_err0:
        jmp     syntax_error
:       sty     tmp10
        jsr     basin_if_more
        jsr     get_hex_word3
        lda     command_index
        cmp     #command_index_c
        beq     LAEA6
        jsr     LB1CB
        jmp     print_cr_then_input_loop

LAEA6:  jsr     LB245
        jmp     input_loop

LACA6:  jsr     LB64D
        bcs     is_mie
LACAB:  jmp     fill_kbd_buffer_with_csr_right

is_mie:
        jsr     print_cr
        lda     command_index
        beq     LACC4 ; 'M'
        cmp     #'S'
        beq     LACD0
        cmp     #'C'
        beq     LACCA
        jsr     dump_ascii_line
        jmp     LACA6

LACC4:  jsr     dump_hex_line
        jmp     LACA6

; EC
LACCA:  jsr     dump_char_line
        jmp     LACA6

; ES
LACD0:  jsr     dump_sprite_line
        jmp     LACA6

LACD6:  jsr     LB64D
        bcc     LACAB
is_d:   jsr     print_cr
        jsr     dump_assembly_line
        jmp     LACD6

is_f:   jsr     basin_if_more
        jsr     get_hex_byte
        jsr     LB22E
        jmp     print_cr_then_input_loop

dump_sprite_line:
        ldx     #']'
        jsr     print_dot_x
        jsr     print_hex_16
        jsr     print_space
        ldy     #0
:       jsr     load_byte
        jsr     print_bin
        iny
        cpy     #3
        bne     :-
        jsr     print_8_spaces
        lda     #2
        jmp     sadd_a_to_zp1

dump_char_line:
        ldx     #'['
        jsr     print_dot_x
        jsr     print_hex_16
        jsr     print_space
        ldy     #0
        jsr     load_byte
        jsr     print_bin
        jsr     print_8_spaces
        jmp     inc_zp1

dump_hex_line:
        ldx     #':'
        jsr     print_dot_x
        jsr     print_hex_16
        jsr     dump_8_hex_bytes
        jsr     print_space
        jmp     dump_8_ascii_characters

dump_ascii_line:
        ldx     #$27  ; "'"
        jsr     print_dot_x
        jsr     print_hex_16
        jsr     print_space
        ldx     #$20
        jmp     dump_ascii_characters

dump_assembly_line:
        ldx     #','
LAD4B:  jsr     print_dot_x
        jsr     print_hex_16
        jsr     print_space
        jsr     decode_mnemo
        jsr     print_asm_bytes
        jsr     print_mnemo
        jsr     print_operand
        jsr     print_8_spaces
        lda     num_asm_bytes
        jmp     sadd_a_to_zp1


; ----------------------------------------------------------------
; "[" - input character data
; ----------------------------------------------------------------
cmd_leftbracket:
        jsr     get_hex_word
        jsr     copy_zp2_to_zp1
        jsr     basin_skip_spaces_if_more
        jsr     get_bin_byte_char_in_a
        ldy     #0
        jsr     store_byte
        jsr     print_up
        jsr     dump_char_line
        jsr     print_cr_dot
        jsr     fill_kbd_buffer_leftbracket
        jmp     input_loop2

; ----------------------------------------------------------------
; "]" - input sprite data
; ----------------------------------------------------------------
cmd_rightbracket:
        jsr     get_hex_word
        jsr     copy_zp2_to_zp1
        jsr     basin_skip_spaces_if_more
        jsr     get_bin_byte_char_in_a
        ldy     #0
        beq     LAD9F
LAD9C:  jsr     get_bin_byte
LAD9F:  jsr     store_byte
        iny
        cpy     #3
        bne     LAD9C
        jsr     print_up
        jsr     dump_sprite_line
        jsr     print_cr_dot
        jsr     fill_kbd_buffer_rightbracket
        jmp     input_loop2

; ----------------------------------------------------------------
; "'" - input 32 ASCII characters
; ----------------------------------------------------------------
cmd_singlequote:
        jsr     get_hex_word
        jsr     read_ascii
        jsr     print_up
        jsr     dump_ascii_line
        jsr     print_cr_dot
        jsr     fill_kbd_buffer_singlequote
        jmp     input_loop2

; ----------------------------------------------------------------
; ":" - input 8 hex bytes
; ----------------------------------------------------------------
cmd_colon:
        jsr     get_hex_word
        jsr     read_8_bytes
        jsr     print_up
        jsr     dump_hex_line
        jsr     print_cr_dot
        jsr     fill_kbd_buffer_semicolon
        jmp     input_loop2

; ----------------------------------------------------------------
; ";" - set registers
; ----------------------------------------------------------------
cmd_semicolon:
        jsr     get_hex_word
        lda     zp2 + 1
        sta     reg_pc_hi
        lda     zp2
        sta     reg_pc_lo
        jsr     basin_if_more
        jsr     get_hex_word3
        lda     zp2
        sta     irq_lo
        lda     zp2 + 1
        sta     irq_hi
        jsr     basin_if_more ; skip upper nybble of bank
        jsr     basin_if_more
        cmp     #'D' ; "drive"
        bne     LAE12
        jsr     basin_if_more
        cmp     #'R'
        bne     syn_err1
        ora     #$80 ; XXX why not lda #$80?
        bmi     LAE1B ; always
LAE12:  jsr     get_hex_byte2
        cmp     #8
        bcs     syn_err1
.ifdef MACHINE_C64
        ora     #$30
.endif
LAE1B:  sta     bank
        ldx     #0
:       jsr     basin_if_more
        jsr     get_hex_byte
        sta     registers,x ; registers
        inx
        cpx     #4
        bne     :-
        jsr     basin_if_more
        jsr     get_bin_byte
        sta     reg_p
        jsr     print_up
        jmp     dump_registers2

syn_err1:
        jmp     syntax_error

; ----------------------------------------------------------------
; "," - input up to three hex values
; ----------------------------------------------------------------
cmd_comma:
        jsr     get_hex_word3
        ldx     #3
        jsr     read_x_bytes
        lda     #$2C
        jsr     LAE7C
        jsr     fill_kbd_buffer_comma
        jmp     input_loop2

; ----------------------------------------------------------------
; "A" - assemble
; ----------------------------------------------------------------
cmd_a:
        jsr     get_hex_word
        jsr     get_mnemonic
        jsr     get_operand
        ldx     #0
        stx     tmp6
LAE61:  ldx     reg_s
        txs
        jsr     LB08D
        jsr     LB0AB
        jsr     swap_zp1_and_zp2
        jsr     LB0EF
        lda     #'A'
        jsr     LAE7C
        jsr     fill_kbd_buffer_a
        jmp     input_loop2

LAE7C:  pha
        jsr     print_up
        pla
        tax
        jsr     LAD4B
        jmp     print_cr_dot


LAEAC:  jsr     basin_if_more
        jsr     basin_if_more
        cmp     #$22
        bne     LAECF
LAEBB:  jsr     basin_cmp_cr
        beq     LAEE7
        cmp     #$22
        beq     LAEE7
        sta     BUF,x
        inx
        cpx     #$20
        bne     LAEBB
        beq     syn_err2 ; always

LAECF:  jsr     get_hex_byte2 ; sets C
        bcs     LAEDC ; always
LAED4:  jsr     basin_cmp_cr
        beq     LAEE7
        jsr     get_hex_byte
LAEDC:  sta     BUF,x
        inx
        cpx     #$20
        bne     LAED4
syn_err2:
        jmp     syntax_error

LAEE7:  stx     command_index
        txa
        beq     syn_err2
        jsr     LB293
        jmp     input_loop

; ----------------------------------------------------------------
; "G" - run code
; ----------------------------------------------------------------
cmd_g:
        jsr     basin_cmp_cr
        beq     LAF03
        jsr     get_hex_word2
        jsr     basin_cmp_cr
        beq     LAF06
        bne     syn_err2 ; always

LAF03:  bit     entry_type
        bvc     :+
        lda     #$F8
        ldx     #$12
        jsr     vdc_reg_store
        lda     #tmpvar1
        inx
        jsr     vdc_reg_store
        ldx     #$1F
        ; A unmodified, tmpvar1 = $90
        jsr     vdc_reg_store
        jmp     vdcxit
:       jsr     copy_pc_to_zp2_and_zp1
LAF06:  lda     bank
        bmi     go_drive ; drive
        jsr     uninstall_kbd_handler
        lda     irq_lo
        sta     CINV
        lda     irq_hi
        sta     CINV+1
.ifdef CART_FC3
        jsr     set_io_vectors_with_hidden_rom
.endif
        ldx     reg_s
        txs
        lda     zp2 + 1
        pha
        lda     zp2
        pha
        lda     reg_p
        pha
        ldx     reg_x
        ldy     reg_y
        lda     bank
        jmp     disable_rom_rti
go_drive:
        lda     #'E' ; send M-E to drive
        jsr     send_m_dash2
        lda     zp2
        jsr     IECOUT
        lda     zp2 + 1
        jsr     IECOUT
        jsr     UNLSTN
        jmp     print_cr_then_input_loop

; ----------------------------------------------------------------
; assembler/disassembler
; ----------------------------------------------------------------
; prints the hex bytes consumed by an asm instruction
print_asm_bytes:
        pha
        ldy     #0
@3:     cpy     num_asm_bytes
        beq     @2
        bcc     @2
        jsr     print_space
        jsr     print_space
        bcc     @1
@2:     jsr     load_byte
        jsr     print_hex_byte2
@1:     jsr     print_space
        iny
        cpy     #3
        bne     @3
        pla
        rts

; returns mnemo index in A
decode_mnemo:
        ldy     #0
        jsr     load_byte; opcode
decode_mnemo_2:
.if .defined(CPU_65C02)
        sta     tmp_opcode
.endif
.if .defined(CPU_6502)
        tay
        lsr     a
        bcc     @1 ; skip if opcodes $x0, $x2, $x4, $x6, $x8, $xA, $xC, $xE
        ; continue for opcodes $x1, $x3, $x5, $x7, $x9, $xB, $xD, $xF
        lsr     a
        bcs     @3 ; branch for opcodes $x3, $x7, $xC, $xF
        ; continue for opcodes $x1, $x5, $x9, $xB
        cmp     #$22
        beq     @3 ; opcodes $89 of $8D?
        and     #$07 ; opcode bits 4,3,2
        ora     #$80 ; use special bytes past first 64
@1:     lsr     a ; opcode bit 2 into carry
        tax
        lda     addmode_table,x
        bcs     @2 ; opcode bit 2 set, then use low nybble
        lsr     a
        lsr     a
        lsr     a
        lsr     a ; otherwise get hi nybble
@2:     and     #$0F
        bne     @4 ; if nybble is 0, Y = $80
@3:     ldy     #$80
        lda     #0
@4:     tax
        lda     addmode_detail_table,x ; X = 0..13
        sta     prefix_suffix_bitfield
        and     #3
        sta     num_asm_bytes
; mnemo: convert opcode in A to mnemo index (0-64)
        tya     ; opcode
        and     #%10001111
        tax
        tya     ; opcode
        ldy     #3
        cpx     #%10001010 ; $8A/$9A/.../$FA?
        beq     @7
@5:     lsr     a
        bcc     @7
        lsr     a
@6:     lsr     a
        ora     #%00100000
        dey
        bne     @6
        iny
@7:     dey
        bne     @5
        rts
.elseif .defined(CPU_6502ILL) || .defined(CPU_65C02)
        tay
        lsr
        tax
        lda     addmode_table,x
        bcs     @1
        lsr
        lsr
        lsr
        lsr
@1:     and     #$0f
        tax
        lda     addmode_detail_table,x ; X = 0..13
        sta     prefix_suffix_bitfield
        and     #3
        sta     num_asm_bytes
        lda     mnemotab,y
        rts
.if .defined(CPU_6502ILL)
mnemotab:
        .byte 15, 44, 36, 62, 43, 44, 6, 62, 46, 44, 6, 3, 43, 44, 6, 62, 14, 44, 36, 62, 43, 44, 6, 62, 18, 44, 43, 62, 43, 44, 6, 62, 35, 4, 36, 49, 11, 4, 50, 49, 48, 4, 50, 3, 11, 4, 50, 49, 12, 4, 36, 49, 43, 4, 50, 49, 57, 4, 43, 49, 43, 4, 50, 49, 53, 29, 36, 63, 43, 29, 42, 63, 45, 29, 42, 2, 34, 29, 42, 63, 16, 29, 36, 63, 43, 29, 42, 63, 20, 29, 43, 63, 43, 29, 42, 63, 54, 0, 36, 52, 43, 0, 51, 52, 47, 0, 51, 5, 34, 0, 51, 52, 17, 0, 36, 52, 43, 0, 51, 52, 59, 0, 43, 52, 43, 0, 51, 52, 43, 64, 43, 55, 66, 64, 65, 55, 28, 43, 71, 74, 66, 64, 65, 55, 8, 64, 36, 1, 66, 64, 65, 55, 73, 64, 72, 67, 61, 64, 60, 1, 41, 39, 40, 38, 41, 39, 40, 38, 69, 39, 68, 38, 41, 39, 40, 38, 9, 39, 36, 38, 41, 39, 40, 38, 21, 39, 70, 37, 41, 39, 40, 38, 24, 22, 43, 25, 24, 22, 26, 25, 32, 22, 27, 7, 24, 22, 26, 25, 13, 22, 36, 25, 43, 22, 26, 25, 19, 22, 43, 25, 43, 22, 26, 25, 23, 56, 43, 33, 23, 56, 30, 33, 31, 56, 43, 56, 23, 56, 30, 33, 10, 56, 36, 33, 43, 56, 30, 33, 58, 56, 43, 33, 43, 56, 30, 33
.elseif .defined(CPU_65C02)
mnemotab:
        .byte 13, 37, 36, 36, 64, 37, 2, 46, 39, 37, 2, 36, 64, 37, 2, 3, 11, 37, 37, 36, 63, 37, 2, 46, 16, 37, 27, 36, 63, 37, 2, 3, 31, 1, 36, 36, 8, 1, 47, 46, 43, 1, 47, 36, 8, 1, 47, 3, 9, 1, 1, 36, 8, 1, 47, 46, 52, 1, 23, 36, 8, 1, 47, 3, 49, 26, 36, 36, 36, 26, 35, 46, 38, 26, 35, 36, 30, 26, 35, 3, 14, 26, 26, 36, 36, 26, 35, 46, 18, 26, 41, 36, 36, 26, 35, 3, 50, 0, 36, 36, 60, 0, 48, 46, 42, 0, 48, 36, 30, 0, 48, 3, 15, 0, 0, 36, 60, 0, 48, 46, 54, 0, 45, 36, 30, 0, 48, 3, 12, 56, 36, 36, 59, 56, 58, 55, 25, 8, 66, 36, 59, 56, 58, 4, 5, 56, 56, 36, 59, 56, 58, 55, 68, 56, 67, 36, 60, 56, 60, 4, 34, 32, 33, 36, 34, 32, 33, 55, 62, 32, 61, 36, 34, 32, 33, 4, 6, 32, 32, 36, 34, 32, 33, 55, 19, 32, 65, 36, 34, 32, 33, 4, 22, 20, 36, 36, 22, 20, 23, 55, 29, 20, 24, 69, 22, 20, 23, 4, 10, 20, 20, 36, 36, 20, 23, 55, 17, 20, 40, 57, 36, 20, 23, 4, 21, 51, 36, 36, 21, 51, 27, 55, 28, 51, 36, 36, 21, 51, 27, 4, 7, 51, 51, 36, 36, 51, 27, 55, 53, 51, 44, 36, 36, 51, 27, 4
.endif
.else
.error "No CPU type specified!"
.endif

; prints name of mnemo in A
print_mnemo:
        tay
        lda     __mnemos1_RUN__,y
        sta     tmp10
        lda     __mnemos2_RUN__,y
        sta     tmp8
        ldx     #3
@1:     lda     #0
        ldy     #5
:       asl     tmp8
        rol     tmp10
        rol     a
        dey
        bne     :-
        adc     #$3F
        jsr     BSOUT
        dex
        bne     @1
.ifdef CPU_65C02
        ; add numeric suffix to RMB/SMB/BBR/BBS
        lda     tmp_opcode
        and     #$07
        cmp     #$07
        bne     :+
        lda     tmp_opcode
        lsr
        lsr
        lsr
        lsr
        and     #$07
        ora     #'0'
        jsr     BSOUT
.endif
        jmp     print_space

; Go through the list of prefixes (3) and suffixes (3),
; and if the corresponding one of six bits is set in
; prefix_suffix_bitfield, print it.
; Between the prefixes and the suffixes, print the one
; or two byte operand
print_operand:
        ldx     #6 ; start with last prefix
LAFD9:  cpx     #3
        bne     LAFF4 ; between prefixes and suffixes?, print operand
        ldy     num_asm_bytes
        beq     LAFF4 ; no operands
:       lda     prefix_suffix_bitfield
.ifdef CPU_65C02
        cmp     #<(S_ZPREL | 2) << 3 ; zp, relative addressing mode
        beq     print_zprel
.endif
        cmp     #<(S_RELATIVE | 1) << 3 ; relative addressing mode
        php
        jsr     load_byte
        plp
        bcs     print_branch_target
        jsr     print_hex_byte2
        dey
        bne     :-
LAFF4:  asl     prefix_suffix_bitfield
        bcc     :+ ; nothing to print
        lda     __asmchars1_RUN__ - 1,x
        jsr     BSOUT
        lda     __asmchars2_RUN__ - 1,x
        beq     :+ ; no second character
        jsr     BSOUT
:       dex
        bne     LAFD9
        rts

print_branch_target:
        jsr     zp1_plus_a_2
        tax
        inx
        bne     :+
        iny
:       tya
        jsr     print_hex_byte2
        txa
        jmp     print_hex_byte2

.ifdef CPU_65C02
print_zprel:
        dey
        jsr     load_byte
        jsr     print_hex_byte2
        lda     #','
        jsr     BSOUT
        lda     #'$'
        jsr     BSOUT
        iny
        jsr     load_byte
        tax
        lda zp1
        pha
        lda zp1+1
        pha
        inc zp1
        bne :+
        inc zp1+1
:       txa
        sec
        jsr     print_branch_target
        pla
        sta zp1+1
        pla
        sta zp1
        rts
.endif

; adds signed to 16 bit zp1 and increases by 1
zp1_plus_a:
        sec
zp1_plus_a_2:
        ldy     zp1 + 1
        tax
        bpl     :+
        dey
:       adc     zp1
        bcc     :+
        iny
:       rts

sadd_a_to_zp1:
        jsr     zp1_plus_a
        sta     zp1
        sty     zp1 + 1
        rts

.proc get_mnemonic
        ldx     #0
        stx     tmp17

        ; Get 3 characters
:       jsr     basin_if_more
        cmp     #' '
        beq     get_mnemonic
        sta     BUF,x
        inx
        cpx     #3
        bne     :-

        ; Convert the characters into a 16 bit number
@1:     lda     BUF-1,x
        sec
        sbc     #$3F
        ldy     #5
:       lsr     a
        ror     tmp17
        ror     tmp16
        dey
        bne     :-
        dex
        bne     @1
        rts
.endproc



.proc get_operand
        lda     #0
        sta     zp3
        sta     zp1
        ; Buffer is from $0210 to $0227. $0210/$0211 contain mnemonic, start at $0212.
        ; We count up til 0, so compute correct start
        ldx     #$100 + 2 - $17
@1:     jsr     BASIN
@2:     cmp     #CR
        beq     @done
        cmp     #':'
        beq     @done
        cmp     #' '
        beq     @1
        lsr     zp3
        bcs     @thex
        jsr     is_dec_character
        bcs     @nodgt
        stx     tmp10
        jsr     get_dec_word3 ; (returns next char in A)
        ldx     tmp10
        tay
        lda     #'$'
        sta     tmp16 - ($100 - $17),x
        inx
        lda     #'1'
        sta     tmp16 - ($100 - $17),x
;        inx
;        sta     tmp16 - ($100 - $17),x
        tya
        inx
        bne     @2
@thex:  jsr     is_hex_character
        bcs     @nodgt
        jsr     get_hex_byte3 ; (doesn't BASIN next char)
        ldy     zp1
        sty     zp1 + 1
        sta     zp1
        inc     zp3  ; Allow another hex byte to follow
        lda     #'0'
@stdig: sta     tmp16 - ($100 - $17),x
        inx
@nodgt: cmp     #'$'
        bne     :+
        inc     zp3
:       sta     tmp16 - ($100 - $17),x
        inx
        bne     @1
@done:  stx     tmp10
        rts
.endproc


LB08D:  ldx     #$100 - $17
        stx     tmp4
        lda     tmp6 ; opcode
        jsr     decode_mnemo_2
        ldx     prefix_suffix_bitfield
        stx     tmp8
        tax
        lda     __mnemos2_RUN__,x
        jsr     verify_char
        lda     __mnemos1_RUN__,x
        jmp     verify_char

LB0AB:  ldx     #6
LB0AD:  cpx     #3
        bne     LB0C5
        lda     num_asm_bytes
        beq     LB0C5
        asl
        tay
        lda     prefix_suffix_bitfield
        cmp     #<(S_RELATIVE | 1) << 3 ; relative addressing mode
        bcs     decode_rel
        jsr     verify_numconst
LB0C5:  asl     prefix_suffix_bitfield
        bcc     LB0D8
        lda     __asmchars1_RUN__ - 1,x
        jsr     verify_char
        lda     __asmchars2_RUN__ - 1,x
        beq     LB0D8
        jsr     verify_char
LB0D8:  dex
        bne     LB0AD
        beq     LB0E3

decode_rel:
        ldy     #4
        jsr     verify_numconst
LB0E3:  lda     tmp10
        cmp     tmp4
        bne     LB13B
        rts

LB0EF:  ldy     num_asm_bytes
        beq     LB123
        lda     tmp8
        cmp     #$9D
        bne     LB11A
        jsr     check_end
        bcc     LB10A
        tya
        bne     bad_operand
        ldx     tmp9
        bmi     bad_operand
        bpl     LB112
LB10A:  iny
        bne     bad_operand
        ldx     tmp9
        bpl     bad_operand
LB112:  dex
        dex
        txa
        ldy     num_asm_bytes
        bne     LB11D
LB11A:  lda     zp1 + 1,y
LB11D:  jsr     store_byte
        dey
        bne     LB11A
LB123:  lda     tmp6
        jsr     store_byte
        rts

bad_operand:
        jmp     input_loop

verify_char:
        stx     tmp3
        ldx     tmp4
        cmp     tmp16 - ($100 - $17),x
        beq     LB146
LB13B:  inc     tmp6
        beq     bad_operand
next_opcode:
        jmp     LAE61  ; no match, try another opcode

verify_numconst:
        stx     tmp3
        ldx     tmp4
        lda     tmp16 - ($100 - $17),x
        dey
        cmp     #'0'
        beq     @num
        cmp     #'1'
        bne     @nonum
        cpy     #1
        bne     LB146
        lda     zp1+1
        beq     LB146
@nonum:
        inc     tmp6
        beq     bad_operand
        bne     next_opcode
@num:   inx
        cmp     tmp16 - ($100 - $17),x
        bne     @nonum
        dey
        bne     @num
LB146:  inx
        stx     tmp4
        ldx     tmp3
rts_01: rts


; ----------------------------------------------------------------
; "$" - convert hex to decimal
; ----------------------------------------------------------------
cmd_dollar:
        jsr     get_hex_word
        jsr     print_up_dot
        jsr     copy_zp2_to_zp1
        jsr     print_dollar_hex_16
        jsr     LB48E
        jsr     print_hash
        jsr     LBC50
        jmp     input_loop

; ----------------------------------------------------------------
; "#" - convert decimal to hex
; ----------------------------------------------------------------
cmd_hash:
        jsr     get_dec_word
        jsr     print_up_dot
        jsr     print_hash
        lda     zp1
        pha
        lda     zp1 + 1
        pha
        jsr     LBC50
        pla
        sta     zp1 + 1
        pla
        sta     zp1
        jsr     LB48E
        jsr     print_dollar_hex_16
        jmp     input_loop

get_dec_word:
        jsr     basin_skip_spaces_if_more
get_dec_word3:
        ; zp1 = intermediate
        ldy     #0
        sty     zp1
        sty     zp1 + 1
@1:     and     #$0F
        ; Add digit to zp1
        clc
        adc     zp1
        sta     zp1
        bcc     :+
        inc     zp1 + 1
:       jsr     BASIN
        cmp     #'0'
        bcc     rts_01
        cmp     #':'
        bcs     rts_01
        ; Multiply zp1 by 10
        tax
        lda     zp1
        ldy     zp1 + 1
        asl     a
        rol     zp1 + 1
        asl     a
        rol     zp1 + 1
        adc     zp1
        sta     zp1
        tya
        adc     zp1 + 1
        asl     zp1
        rol     a
        sta     zp1 + 1
        txa
        bcc     @1       ; Next digit (alwaysa)

; ----------------------------------------------------------------
; "X" - exit monitor
; ----------------------------------------------------------------


cmd_x:
        jsr     uninstall_kbd_handler
        bit     entry_type
        bvs     vdcxit
.ifdef CART_FC3
        jsr     set_io_vectors_with_hidden_rom
.endif
.ifdef MACHINE_C64
        lda     #0
        sta     RPTFLG
.endif
.ifdef MACHINE_TED
        jsr     $F39C; restore F keys
.endif
        lda     #<kernal_brk_handler
        sta     CBINV
        lda     #>kernal_brk_handler
        sta     CBINV + 1 ; BRK vector

        ldx     reg_s
        txs
        jmp     _basic_warm_start


vdcxit:
        ; NMI is continuously low inside monitor, in order to safely re-enter monitor,
        ; we need to generate a harmless NMI to avoid monitor code to trigger an undesired NMI.
        sei
        lda     #<_rti
        sta     $0318
        lda     #>_rti
        sta     $0319
        ; Pull NMI low
        lda     #fcio_bank_0|fcio_c64_16kcrtmode
        sta     fcio_reg

        ; Restore $D000..$D02E from $F3D1..$F3FF in VDC
        lda     #>freezer_vicii_backup
        ldx     #$12
        jsr     vdc_reg_store
        lda     #<freezer_vicii_backup
        inx
        jsr     vdc_reg_store
        lda     #$00
        sta     tmpptr_a
        lda     #$D0
        sta     tmpptr_a+1
        ldx     #$1F
:       jsr     vdc_reg_load
        ldy     #$00
        sta     (tmpptr_a),y
        inc     tmpptr_a
        lda     tmpptr_a
        cmp     #$2F
        bne     :-

        ; Restore $D800..$DBFF
        lda     #<$D800
        sta     tmpptr_a
        lda     #>$D800
        sta     tmpptr_a+1
        ldx     #$1F
:       jsr     vdc_reg_load
        ldy     #$00
        sta     (tmpptr_a),y
        inc     tmpptr_a
        bne     :-
        inc     tmpptr_a+1
        lda     tmpptr_a+1
        cmp     #>$DC00
        bne     :-

        ; Restore $0000..$0090
        lda     #$00
        sta     tmpptr_a
        sta     tmpptr_a+1
        ldx     #$1F
:       jsr     vdc_reg_load
        ldy     #$00
        sta     (tmpptr_a),y
        inc     tmpptr_a
        lda     tmpptr_a
        cmp     #$91
        bne     :-

        ; Restore $0093..$07FF. We will overwrite the stack, so cannot
        ; use subroutines, read from VDC directly.
        lda     #$93
        ldx     #$13
        jsr     vdc_reg_store
        sta     tmpptr_a
        ldx     #$1F
        stx     $D600
:       bit     $D600   ; No point for a timeout, all is lost if VDC fails
        bpl     :-
        lda     $D601
        ldy     #$00
        sta     (tmpptr_a),y
        inc     tmpptr_a
        bne     :-
        inc     tmpptr_a+1
        lda     tmpptr_a+1
        cmp     #>$0800
        bne     :-

        ; Make the stack function again.
        ldx     tmpvar2
        txs

        bit     tmpvar1
        bmi     @run
        lda     #>(restart_freezer - 1)
        pha
        lda     #<(restart_freezer - 1)
        pha
        lda     #fcio_bank_3|fcio_c64_16kcrtmode
        jmp     _jmp_bank
@run:   ldy     #$35
        jmp     _disable_fc3rom_set_01

;---------------------------------------------------------

LB1CB:  lda     zp2
        cmp     zp1
        lda     zp2 + 1
        sbc     zp1 + 1
        bcs     LB1FC
        ldy     #0
        ldx     #0
@1:     jsr     load_byte
        pha
        jsr     swap_zp1_and_zp2
        pla
        jsr     store_byte
        jsr     swap_zp1_and_zp2
        cpx     tmp10
        bne     :+
        cpy     tmp9
        beq     @x
:       iny
        bne     @1
        inc     zp1 + 1
        inc     zp2 + 1
        inx
        bne     @1
@x:     rts

LB1FC:  clc
        ldx     tmp10
        txa
        adc     zp1 + 1
        sta     zp1 + 1
        clc
        txa
        adc     zp2 + 1
        sta     zp2 + 1
        ldy     tmp9
LB20E:  jsr     load_byte
        pha
        jsr     swap_zp1_and_zp2
        pla
        jsr     store_byte
        jsr     swap_zp1_and_zp2
        cpy     #0
        bne     LB229
        cpx     #0
        beq     LB22D
        dec     zp1 + 1
        dec     zp2 + 1
        dex
LB229:  dey
        bcs     LB20E ; always because x > 0

LB22D:  rts

LB22E:  ldy     #0
LB230:  jsr     store_byte
        ldx     zp1
        cpx     zp2
        bne     LB23F
        ldx     zp1 + 1
        cpx     zp2 + 1
        beq     LB244
LB23F:  jsr     inc_zp1
        bne     LB230
LB244:  rts

LB245:  jsr     print_cr
        clc
        lda     zp1
        adc     tmp9
        sta     tmp9
        lda     zp1 + 1
        adc     tmp10
        sta     tmp10
        ldy     #0
LB25B:  jsr     load_byte
        sta     command_index
        jsr     swap_zp1_and_zp2
        jsr     load_byte
        pha
        jsr     swap_zp1_and_zp2
        pla
        cmp     command_index
        beq     LB274
        jsr     print_space_hex_16
LB274:  jsr     STOP
        beq     LB292
        lda     zp1 + 1
        cmp     tmp10
        bne     LB287
        lda     zp1
        cmp     tmp9
        beq     LB292
LB287:  inc     zp2
        bne     LB28D
        inc     zp2 + 1
LB28D:  jsr     inc_zp1
        bne     LB25B
LB292:  rts

LB293:  jsr     print_cr
@1:     jsr     check_end
        bcc     @x
        ldy     #0
:       jsr     load_byte
        cmp     BUF,y
        bne     :+
        iny
        cpy     command_index
        bne     :-
        jsr     print_space_hex_16
:       jsr     inc_zp1
        bne     @1
@x:     rts

; ----------------------------------------------------------------
; memory load/store
; ----------------------------------------------------------------

add_y_to_zp1:
        tya
        clc
        adc     zp1
        sta     zp1
        bcc     :+
        inc     zp1+1

vdc_set_addreg:
        ldy     #63 ; VDC should have time for processing at least once per
                    ; scanline, this is multiple scanlines in cycles, so
                    ; should be enough.
        stx     $d600
:       dey
        beq     @error
        bit     $d600
        bpl     :-
        .byte   $24 ; Skip next instruction
@error: tya         ; Return 0 if time-out
        rts


; stores a byte in A into VDC register X
vdc_reg_store:
        jsr     vdc_set_addreg
        sta     $d601
        rts

; loads a byte in A from VDC register X
vdc_reg_load:
        jsr     vdc_set_addreg
        lda     $d601
        rts

; loads a byte at (zp1),y from VDC RAM
load_byte_vdc:
        tya
        clc
        adc     zp1
;        php
        ldx     #$13
        jsr     vdc_reg_store
;        plp
        lda     #0
        adc     zp1+1
;        lda     zp1+1
        dex
        jsr     vdc_reg_store
        ldx     #$1f
        jsr     vdc_reg_load
        ldx     tmp1
        ldy     tmp2
        rts

; loads a byte at (zp1),y from drive RAM
load_byte_drive:
        lda     #'R' ; send M-R to drive
        jsr     send_m_dash2
        jsr     iec_send_zp1_plus_y
        jsr     UNLSTN
        jsr     command_channel_talk
        jsr     IECIN ; read byte
        pha
        jsr     UNTALK
        pla
        rts


; loads a byte at (zp1),y from RAM with the correct ROM config
load_byte:
        stx     tmp1
        sty     tmp2
        lda     bank
        cmp     #$80
        beq     load_byte_drive ; drive
        cmp     #$81
        beq     load_byte_vdc
.ifdef CART_FC3
        bit     bank
        bvs     @frozen_vdc
.endif
.ifdef MACHINE_TED
;        stx tmp1
;        sty tmp2
        lda zp1
        sta FETPTR
        lda zp1 + 1
        sta FETPTR + 1
        lda #DEFAULT_BANK
        ldx bank
        sei
        jsr FETCHL
        cli
        ldx tmp1
        ldy tmp2
        rts
.else
@r:     clc
.ifdef CART_FC3
        pha
        lda     cartridge_bank
.endif
        sei
        jmp     load_byte_ram ; "lda (zp1),y" with ROM and cartridge config
.endif

.ifdef CART_FC3
@br:    lda     bank ; Bit 6 is set but doesn't hurt
        bne     @r

@frozen_vdc:
        lda     zp1
        sta     tmp3
        lda     zp1+1
        sta     tmp4
        jsr     add_y_to_zp1
        ; Check if I/O visible
:       lda     #$03
        bit     bank
        beq     @2
        lda     zp1+1
        ldx     #2
@1:     cmp     chips_base,x
        bne     @2
        clc
        lda     chips_back_lo,x
        adc     zp1
        tay
        lda     chips_back_hi,x
        bne     @6
@2:     dex
        bpl     @1

@3:     jsr     check_frz_mem
        lda     $72,x
        bcc     @7
        ldy     zp1
        lda     zp1+1
        bne     :+
        ; Zero page
        tya
        sec
        sbc     #$70
        bcc     :+
        cmp     #freezer_mem_a_size
        bcs     :+
        adc     $70
        tay
        lda     $71
        adc     #$00
:       ; Check whether < $0800, then read from backup,
        ; otherwise read from mem
        cmp     #>$0800
        bcc     :+
        lda     bank
        jsr     @r
        jmp     @7
:       ora     #>$F800
@6:     sty     zp1
        sta     zp1+1
        ldy     #0
        jsr     load_byte_vdc
@7:     pha
        lda     tmp3
        sta     zp1
        lda     tmp4
        sta     zp1+1
        pla
@x:
        ldx     tmp1
        ldy     tmp2
        rts

;
; VIC-II, CIA1, CIA2
;
chips_base:     .byte >$D000, >$DC00, >$DD00
chips_back_lo:  .byte <freezer_vicii_backup, $70, $80
chips_back_hi:  .byte >freezer_vicii_backup, $F8, $F8


check_frz_mem:
        ; Check whether address is in freezer memory area A
        ldx     #0
        jsr     check_area
        bcc     _rts
        ; Check whether address is in freezer memory area B
        ldx     #3
check_area:
        lda     zp1
        sec
        sbc     $70,x
        tay
        lda     zp1+1
        sbc     $71,x
        bcc     secrts
        bne     secrts
        tya
        cmp     $76,x
        rts
secrts: sec
_rts:   rts
.endif

; stores a byte at (zp1),y in VDC RAM
store_byte_vdc:
        stx     tmp1
        sty     tmp2
        tya
        clc
        adc     zp1
;        php
        ldx     #$13
        jsr     vdc_reg_store
;        plp
        lda     #0
        adc     zp1+1
        dex
        jsr     vdc_reg_store
        pla
        ldx     #$1f
        jsr     vdc_reg_store
        ldx     tmp1
        ldy     tmp2
        rts

; stores a byte at (zp1),y in drive RAM
store_byte_drive:
        lda     #'W' ; send M-W to drive
        jsr     send_m_dash2
        jsr     iec_send_zp1_plus_y
        lda     #1 ; count
        jsr     IECOUT
        pla
        pha
        jsr     IECOUT
        jsr     UNLSTN
        pla
        rts

; stores a byte at (zp1),y in RAM with the correct ROM config
store_byte:
.ifdef MACHINE_TED
        sta     (zp1),y ; store
        rts
.else
        sei
        pha
        lda     bank
        cmp     #$80
        beq     store_byte_drive ; drive
        cmp     #$81
        beq     store_byte_vdc ; drive
        ; This is tricky code: At this time we are in 16K cartridge mode and
        ; executing from ROMH ($A000..). Writing the current bank to $01 may
        ; hide cartridge ROM.
        ;
        ; Therefore, if IO is enabled, we skip writing to $01, taking
        ; advantage of the C64's feature that writes to ROM go to RAM below
        ; ROM. If IO is disabled, we write $33, again taking advantage that
        ; writes to to RAM below ROM.
        cmp     #$35
        bcs     :+ ; I/O on
        lda     #$33 ; ROM at $8000, $A000, $D000 and $E000
        sta     R6510
:       pla
        sta     (zp1),y ; store
        pha
        lda     #DEFAULT_BANK
        sta     R6510 ; restore ROM config
        pla
        rts
.endif

.ifdef CART_FC3
; ----------------------------------------------------------------
; "B" - set cartridge bank (0-3) to be visible at $8000-$BFFF
;       without arguments, this turns off cartridge visibility
; ----------------------------------------------------------------
cmd_b:  jsr     basin_cmp_cr
        beq     @1 ; without arguments, set $70
        jsr     is_hex_character
        bcs     syn_err3
        jsr     hex_digit_to_nybble
        ora     #$40  ; make $40 - $4f
       .byte   $2C
@1:     lda     #$70 ; by default, hide cartridge
        sta     cartridge_bank
        jmp     print_cr_then_input_loop
.endif

syn_err3:
        jmp     syntax_error

; ----------------------------------------------------------------
; "O" - set bank
;       0 to 7 map to a $01 value of $30-$37, "D" switches to drive
;       memory
; ----------------------------------------------------------------
cmd_o:
.ifdef MACHINE_C64
        lda     #0
        sta     tmp1
.endif
@nd:    jsr     basin_cmp_cr
        beq     @dflt ; without arguments: bank 7
        cmp     #' '
        beq     @nd
.ifdef MACHINE_TED
        tax
        bmi     :+ ; shifted arg skips 'D' test
.endif
        cmp     #'D'
        beq     @disk ; disk
.ifdef CART_FC3
        cmp     #'V'
        beq     @vdc ; vdc
        cmp     #'F'
        bne     :+
        ; Entry from freezer?
        lda     #$C0
        bit     entry_type
        beq     syn_err3
        ; Frozen memory
        lda     #$40
        sta     tmp1
        bne     @nd
:
.endif
.ifdef MACHINE_TED
:       jsr     hex_digit_to_nybble
.endif
        .byte   $2C
@dflt:  lda     #DEFAULT_BANK
.ifdef MACHINE_C64
        cmp     #$38
        bcs     syn_err3
        cmp     #$30
        bcc     syn_err3
        .byte   $2C
.endif
@disk:  lda     #$80 ; drive
.ifdef MACHINE_C64
        .byte   $2C
@vdc:   lda     #$81
        ora     tmp1
.endif
        sta     bank
        jmp     print_cr_then_input_loop

listen_command_channel:
        lda     #$6F
        jsr     listen_second
        lda     ST
        bmi     LB3A6
        rts

; ----------------------------------------------------------------
; "L"/"S" - load/save file
; ----------------------------------------------------------------
cmd_ls:
        ldy     #2
        sty     FNADR + 1  ; tmp vars in bank 2
        dey
        sty     SA  ; = 1
        dey
        sty     FNLEN  ; = 0
        lda     #8
        sta     FA
        lda     #<tmp16
        sta     FNADR
        jsr     basin_skip_spaces_cmp_cr
        bne     LB3B6
LB388:  lda     command_index
        cmp     #command_index_l
        bne     syn_err4
LB38F:
;.ifdef CART_FC3
;        jsr     restore_bsout_chrch
;.endif
        jsr     uninstall_kbd_handler
        ldx     zp1
        ldy     zp1 + 1
        jsr     perform_load
        php
;.ifdef CART_FC3
;        jsr     set_io_vectors
;.endif
        jsr     set_irq_and_kbd_handlers
        plp
LB3A4:  bcc     LB3B3
LB3A6:  ldx     #256-10
:       lda     LF0BD-(256-10),x ; "I/O ERROR"
        jsr     BSOUT
        inx
        bne     :-
LB3B3:  jmp     input_loop

LB3B6:  cmp     #'"'
        bne     syn_err4
LB3BA:  jsr     basin_cmp_cr
        beq     LB388
        cmp     #'"'
        beq     fn_parsed
        sta     (FNADR),y
        inc     FNLEN
        iny
        cpy     #$10
        bne     LB3BA
syn_err4:
        jmp     syntax_error

fn_parsed:
        jsr     basin_cmp_cr
        beq     LB388
        cmp     #','
LB3D6:  bne     syn_err4
        jsr     get_hex_byte
        and     #$0F
        beq     syn_err4
        cmp     #1 ; tape
        beq     :+
        cmp     #4
        bcc     syn_err4 ; illegal device number
:       sta     FA
        jsr     basin_cmp_cr
        beq     LB388
        cmp     #','
        bne     syn_err4
        jsr     get_hex_word3
        jsr     swap_zp1_and_zp2
        jsr     basin_cmp_cr
        bne     LB408
        lda     command_index
        cmp     #command_index_l
        bne     syn_err4
        dec     SA
        beq     LB38F

LB408:  cmp     #','
LB40A:  bne     syn_err4
        jsr     get_hex_word3
        jsr     basin_skip_spaces_cmp_cr
        bne     LB40A
        ldx     zp2
        ldy     zp2 + 1
        lda     command_index
        cmp     #command_index_s
        bne     LB40A
        dec     SA
;.ifdef CART_FC3
;        jsr     set_io_vectors_with_hidden_rom
;.endif
        jsr     perform_save
;.ifdef CART_FC3
;        jsr     set_io_vectors
;.endif
        jmp     LB3A4

perform_load:
.ifdef CART_FC3
        lda     #>(_enable_fcbank0 - 1)
        pha
        lda     #<(_enable_fcbank0 - 1)
        pha
.endif
        lda     #0
        jmp     LOAD

perform_save:
.ifdef CART_FC3
        lda     #>(_enable_fcbank0 - 1)
        pha
        lda     #<(_enable_fcbank0 - 1)
        pha
.endif
        lda     #zp1 ; pointer to ZP location with address
        jmp     SAVE

; ----------------------------------------------------------------
; "@" - send drive command
;       without arguments, this reads the drive status
;       $ shows the directory
;       F does a fast format
; ----------------------------------------------------------------
cmd_at: 
        jsr     basin_cmp_cr
        php
        pha
        cmp     #'#'
        beq     @change_dev
        jsr     listen_command_channel
        pla
        plp
        beq     print_drive_status
        bne     @nodevnum

@change_dev:
        ; Accept device numbers 8..15
;        cmp     #'#'
;        bne     @nodevnum
        jsr     basin_cmp_cr
        cmp     #'8'
        beq     @4
        cmp     #'9'
        beq     @4
        cmp     #'1'
        bne     @nodevnum
        jsr     basin_cmp_cr
        beq     @nodevnum
        cmp     #'0'
        bcc     @nodevnum
        cmp     #'6'
        bcs     @nodevnum

@2:     ; carry is already clear
        adc     #$0A
@4:     and     #$0F
        sta     FA
        jsr     listen_command_channel ; Clears N
        bpl     print_drive_status ; Always

@nodevnum:
        cmp     #'$'
        beq     LB475
.ifdef CART_FC3
        cmp     #'F'
        bne     @7
        jsr     jfast_format
        lda     #'F'
.endif
@7:     jsr     IECOUT
        jsr     basin_cmp_cr
        bne     @7
        jsr     UNLSTN
@5:
        jmp     print_cr_then_input_loop

; just print drive status
print_drive_status:
        jsr     print_cr
        jsr     UNLSTN
        jsr     command_channel_talk
        jsr     print_line_from_drive
        jmp     input_loop

; show directory
LB475:  jsr     UNLSTN
        jsr     print_cr
        lda     #$F0 ; sec address
        jsr     listen_second
        lda     #'$'
        jsr     IECOUT
        jsr     UNLSTN
        jsr     print_dir
        jmp     input_loop

LB48E:  jsr     print_space
        lda     #'='
        ldx     #' '
        bne     print_a_x

print_up:
        ldx     #CSR_UP
        .byte   $2C
print_cr_dot:
        ldx     #'.'
        lda     #CR
        .byte   $2C
print_dot_x:
        lda     #'.'
print_a_x:
        jsr     BSOUT
        txa
        jmp     BSOUT

print_up_dot:
        jsr     print_up
        lda     #'.'
        .byte   $2C
print_hash:
        lda     #'#'
        .byte   $2C
print_space:
        lda     #' '
        .byte   $2C
print_cr:
        lda     #CR
        jmp     BSOUT

basin_skip_spaces_if_more:
        jsr     basin_skip_spaces_cmp_cr
        jmp     LB4C5

; get a character; if it's CR, return to main input loop
basin_if_more:
        jsr     basin_cmp_cr
LB4C5:  bne     LB4CA ; rts
        jmp     input_loop

LB4CA:  rts

basin_skip_spaces_cmp_cr:
        jsr     BASIN
        cmp     #' '
        beq     basin_skip_spaces_cmp_cr ; skip spaces
        cmp     #CR
        rts

basin_cmp_cr:
        jsr     BASIN
        cmp     #CR
        rts


get_bin_byte_char_in_a:
        pha
        ldx     #8
        bne     gbb_c
.proc get_bin_byte
        ldx     #8
_1:     pha
        jsr     basin_if_more
_c:     cmp     #'*'
        beq     :+
        clc
:       pla
        rol     a
        dex
        bne     _1
        rts
.endproc
gbb_c = get_bin_byte::_c

; get a 16 bit ASCII hex number from the user, return it in zp2
get_hex_word:
        jsr     basin_if_more
get_hex_word2:
        cmp     #' ' ; skip spaces
        beq     get_hex_word
        jsr     get_hex_byte2 ; sets C
        bcs     LB500 ; always
get_hex_word3:
        jsr     get_hex_byte
LB500:  sta     zp2 + 1
        jsr     get_hex_byte
        sta     zp2
        rts

; get a 8 bit ASCII hex number from the user, return it in A
get_hex_byte:
;        lda     #0
;        sta     tmp2 ; XXX not necessary
        jsr     basin_if_more
get_hex_byte2:
        jsr     validate_hex_digit
get_hex_byte3:
        jsr     hex_digit_to_nybble
        asl     a
        asl     a
        asl     a
        asl     a
        sta     tmp2 ; low nybble
        jsr     get_hex_digit
        jsr     hex_digit_to_nybble
        ora     tmp2
        sec
        rts

.global hex_digit_to_nybble
hex_digit_to_nybble:
        cmp     #'9' + 1
        and     #$0F
        bcc     :+
        adc     #'A' - '9'
:       rts

; get character and check for legal ASCII hex digit
get_hex_digit:
        jsr     basin_if_more
validate_hex_digit:
        cmp     #'0'
        bcc     syn_err5
        cmp     #':'
        bcc     :+ ; ok
        cmp     #'A'
        bcc     syn_err5
        cmp     #'F' + 1
        bcs     syn_err5
:       rts

print_dollar_hex_16:
        lda     #'$'
        .byte   $2C
print_space_hex_16:
        lda     #' '
        jsr     BSOUT
print_hex_16:
        lda     zp1 + 1
        jsr     print_hex_byte2
        lda     zp1

print_hex_byte2:
        sty     tmp1
        jsr     print_hex_byte
        ldy     tmp1
        rts

print_bin:
        ldx     #8
@1:     rol     a
        pha
        lda     #'*'
        bcs     :+
        lda     #'.'
:       jsr     BSOUT
        pla
        dex
        bne     @1
        rts


inc_zp1:
        clc
        inc     zp1
        bne     :+
        inc     zp1 + 1
        sec
:       rts


dump_8_hex_bytes:
        ldy     #0
:       jsr     print_space
        jsr     load_byte
        jsr     print_hex_byte2
        iny
        cpy     #8
        bne     :-
        rts

syn_err5:
        jmp     syntax_error

dump_8_ascii_characters:
        ldx     #8
dump_ascii_characters:
        ldy     #$FF
@1:     iny
        jsr     load_byte
        cmp     #$20
        bcs     :+
        inc     RVS
        ora     #$40
:       cmp     #$80
        bcc     :+
        cmp     #$A0
        bcs     :+
        and     #$7F
        ora     #$60
        inc     RVS
:       jsr     BSOUT
        lda     #0
        sta     RVS
        sta     QTSW
        dex
        bne     @1
        tya ; number of bytes consumed
        jmp     sadd_a_to_zp1

read_ascii:
        ldy     #0
        jsr     copy_zp2_to_zp1
        jsr     basin_if_more
@1:     sty     tmp9
        ldy     PNTR
        lda     (PNT),y
        php
        jsr     basin_if_more
        ldy     tmp9
        plp
        bmi     :+
        cmp     #$60
        bcs     :+
        jsr     store_byte
:       iny
        cpy     #$20
        bne     @1
        rts

read_8_bytes:
        ldx     #8
read_x_bytes:
        ldy     #0
        jsr     copy_zp2_to_zp1
        jsr     basin_skip_spaces_if_more
        jsr     get_hex_byte2 ; sets C
        bcs     LB607 ; always

LB5F5:  jsr     basin_if_more_cmp_space ; ignore character where space should be
        jsr     basin_if_more_cmp_space
        bne     LB604 ; not space
        jsr     basin_if_more_cmp_space
        beq     LB60A
        bne     syn_err5 ; always

LB604:  jsr     get_hex_byte2
LB607:  jsr     store_byte
LB60A:  iny
        dex
        bne     LB5F5
        rts

basin_if_more_cmp_space:
        jsr     basin_cmp_cr
        bne     :+
        pla
        pla
:       cmp     #' '
        rts


is_dec_character:
        cmp     #'0'
        bcc     @no
        cmp     #':'
        bcc     @rts
        rts
@no:    sec
@rts:   rts

is_hex_character:
        cmp     #'0'
        bcc     @no
        cmp     #':'
        bcc     @rts
        cmp     #'A'
        bcc     @no
        cmp     #'F' + 1
        rts
@no:    sec
@rts:   rts

swap_zp1_and_zp2:
        lda     zp2 + 1
        pha
        lda     zp1 + 1
        sta     zp2 + 1
        pla
        sta     zp1 + 1
        lda     zp2
        pha
        lda     zp1
        sta     zp2
        pla
        sta     zp1
        rts

copy_pc_to_zp2_and_zp1:
        lda     reg_pc_hi
        sta     zp2 + 1
        lda     reg_pc_lo
        sta     zp2

copy_zp2_to_zp1:
        lda     zp2
        sta     zp1
        lda     zp2 + 1
        sta     zp1 + 1
        rts

LB64D:  lda     zp1 + 1
        bne     check_end
        bcc     check_end
        clc
        rts

check_end:
        jsr     STOP
        beq     :+
        lda     zp2
        ldy     zp2 + 1
        sec
        sbc     zp1
        sta     tmp9 ; zp2 - zp1
        tya
        sbc     zp1 + 1
        tay ; (zp2 + 1) - (zp1 + 1)
        ora     tmp9
        rts
:       clc
        rts

fill_kbd_buffer_comma:
        lda     #','
        .byte   $2C
fill_kbd_buffer_semicolon:
        lda     #':'
        .byte   $2C
fill_kbd_buffer_a:
        lda     #'A'
        .byte   $2C
fill_kbd_buffer_leftbracket:
        lda     #'['
        .byte   $2C
fill_kbd_buffer_rightbracket:
        lda     #']'
        .byte   $2C
fill_kbd_buffer_singlequote:
        lda     #$27 ; "'"
        sta     KEYD
        lda     zp1 + 1
        jsr     byte_to_hex_ascii
        sta     KEYD + 1
        sty     KEYD + 2
        lda     zp1
        jsr     byte_to_hex_ascii
        sta     KEYD + 3
        sty     KEYD + 4
        lda     #' '
        sta     KEYD + 5
        lda     #6 ; number of characters
        sta     NDX
        rts


; print 8 spaces - this is used to clear some leftover characters
; on the screen when re-dumping a line with proper spacing after the
; user may have entered it with condensed spacing
.proc print_8_spaces
        lda     #' '
        ldx     #8
:       jsr     BSOUT
        dex
        bne     :-
        rts
.endproc

; ----------------------------------------------------------------
; scrolling support for screen editor
; ----------------------------------------------------------------

advance_next:
        lda     #0 ; default
        ldx     tmp12
        cpx     #':'
        bne     :+
        lda     #7
:       cpx     #$1D ; screen code for ]
        bne     :+
        lda     #2
:       cpx     #$27 ; "'"
        bne     :+
        lda     #31
:       cpx     #','
        bne     :+
        jsr     decode_mnemo
        lda     num_asm_bytes
:       jsr     sadd_a_to_zp1
        rts

retreat_prev:
        lda     #$FE ; -1, default
        ldx     tmp12
        cpx     #':'
        bne     :+
        lda     #$F7 ; -8
:       cpx     #$1D ; screen code for ]
        bne     :+
        lda     #$FC ; -3
:       cpx     #$27 ; "'"
        bne     :+
        lda     #$DF ; -32
:       cpx     #','
        bne     @2
        ; Start decoding 16 bytes before current location, it will get in sync.
        ; Then go length of last instruction backwards.
        jsr     swap_zp1_and_zp2
        lda     #16 
        sta     tmp13
@1:     sec
        lda     zp2
        sbc     tmp13
        sta     zp1
        lda     zp2 + 1
        sbc     #0
        sta     zp1 + 1
:       jsr     decode_mnemo
.if .defined(CPU_6502ILL)
        cmp     #$24   ; 'KIL' mnemonic, unlikely a sign of synchronization
        beq     @3
.endif
        lda     num_asm_bytes
        jsr     sadd_a_to_zp1
        jsr     check_end ; zp1 = zp2?
        beq     :+
        bcs     :-
@3:     dec     tmp13 ; no sync, try different start
        bne     @1
:       inc     num_asm_bytes
        lda     num_asm_bytes
;        adc     #$00
        eor     #$FF
@2:     jsr     sadd_a_to_zp1
        rts


dump_monitor_line:
        ldx     tmp12
        cpx     #':'
        bne     :+
        jsr     dump_hex_line
:       cpx     #$1B ; screen code for [
        bne     :+
        jsr     dump_char_line
:       cpx     #$1D ; screen code for ]
        bne     :+
        jsr     dump_sprite_line
:       cpx     #$27 ; "'"
        bne     :+
        jsr     dump_ascii_line
:       cpx     #','
        bne     :+
        jsr     dump_assembly_line
:       rts

.global scrolldown_monitor
scrolldown_monitor:
        bit     $02AB
        bmi     @1
        ; Find a line number
:       dex
        bmi     @xcs
        lda     $D9,x ; high byte to pointer in screen ram
        bpl     :-
        jsr     read_mem_addr_monitor
        bcs     :-   ; no line numer on botton row
        jsr     advance_next

@1:     ; We have the line to be shown in ($14), now do the scroll
        lda     #$8D
        jsr     $E716 ; shift+return, scrolls screen down
        jsr     dump_monitor_line
        clc
        .byte $24 ; skip next instruction
@xcs:   sec
        rts

.global scrollup_monitor
scrollup_monitor:
        bit     $02AB
        bvs     @1
:       inx
        cpx     #25
        beq     @xcs
        lda     $D9,x
        bpl     :-
        jsr     read_mem_addr_monitor
        bcs     :-
@1:     jsr     retreat_prev
        lda     zp1
        pha
        lda     zp1+1
        pha
        jsr     scroll_screen_up
        jsr     $E566 ; cursor home
        jsr     dump_monitor_line
        pla
        sta     zp1+1
        pla
        sta     zp1
        clc
        .byte $24 ; skip next instruction
@xcs:   sec
        rts

;
; Attempt to read a memory address inside the monitor
;
read_mem_addr_monitor:
        ldy     $ECF0,x ; low bytes of screen line addresses
        sty     TXTPTR
        and     #3      ; Screen ram is only 4 pages
        ora     $0288   ; Base address of screen
        sta     TXTPTR + 1
        ldy     #1
        jsr     _lda_TXTPTR_indy
        cmp     #':'
        beq     @2
        cmp     #','
        beq     @2
        cmp     #$1B ; screen code for [
        beq     @2
        cmp     #$1D ; screen code for ]
        beq     @2
        cmp     #$27 ; "'"
        bne     @xcs
@2:     sta     tmp12
        iny
@1:     cpy     #$16
        beq     @xcs
        jsr     get_digit
        cmp     #$20
        beq     @1
        jsr     read_byte
        sta     zp1+1
        jsr     read_byte
        sta     zp1
        jsr     get_digit
        cmp     #$20
        bne     @1
        clc
        .byte $24 ; skip next instriction
@xcs:   sec
        rts

get_digit:
        jsr     _lda_TXTPTR_indy
        and     #$7F
        cmp     #$20
        bcs     :+
        ora     #$40
:       rts

read_byte:
        jsr     get_digit
        jsr     hex_digit_to_nybble
        iny
        asl     a
        asl     a
        asl     a
        asl     a
        sta     tmp11
        jsr     get_digit
        jsr     hex_digit_to_nybble
        iny
        ora     tmp11
        rts


; ----------------------------------------------------------------
; assembler tables
; ----------------------------------------------------------------
addmode_table:
.if .defined(CPU_6502)
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IM3
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABX

        .byte   ADDMODE_ABS << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IM3
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABX

        .byte   ADDMODE_IM2 << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IM3
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABX

        .byte   ADDMODE_IM2 << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IM3
        .byte   ADDMODE_IND << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABX

        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IM2
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPY
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IM2
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP

        .byte   ADDMODE_IMM << 4 | ADDMODE_IMM
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IM2
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPY
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IM2
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABY

        .byte   ADDMODE_IMM << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IM2
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABX

        .byte   ADDMODE_IMM << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IM2
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IM2 << 4 | ADDMODE_IMP
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABX

        .byte   ADDMODE_IZX << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMM << 4 | ADDMODE_ABS
        .byte   ADDMODE_IZY << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ABY << 4 | ADDMODE_ABX
.elseif .defined(CPU_6502ILL)
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZX
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZY
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABS << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZX
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZY
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZX
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZY
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZX
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IND << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZY
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZY
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPY << 4 | ADDMODE_ZPY
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABY << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZY
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPY << 4 | ADDMODE_ZPY
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABY << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZY
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZY
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
.elseif .defined(CPU_65C02)
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ZPR
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IZP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ZPR
        .byte   ADDMODE_ABS << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ZPR
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IZP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ZPR
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ZPR
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IZP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ZPR
        .byte   ADDMODE_IMP << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_IND << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ZPR
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IZP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_IAX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ZPR
        .byte   ADDMODE_REL << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ZPR
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IZP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPY << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ZPR
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ZPR
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IZP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPY << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABX << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABY << 4 | ADDMODE_ZPR
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ZPR
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IZP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ZPR
        .byte   ADDMODE_IMM << 4 | ADDMODE_IZX
        .byte   ADDMODE_IMM << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_ZPG << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMM
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABS
        .byte   ADDMODE_ABS << 4 | ADDMODE_ZPR
        .byte   ADDMODE_REL << 4 | ADDMODE_IZY
        .byte   ADDMODE_IZP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPX
        .byte   ADDMODE_ZPX << 4 | ADDMODE_ZPG
        .byte   ADDMODE_IMP << 4 | ADDMODE_ABY
        .byte   ADDMODE_IMP << 4 | ADDMODE_IMP
        .byte   ADDMODE_ABS << 4 | ADDMODE_ABX
        .byte   ADDMODE_ABX << 4 | ADDMODE_ZPR
.else
.error "No CPU type specified!"
.endif

P_NONE     = 0
P_DOLLAR   = 1 << 7
P_PAREN    = 1 << 6
P_HASH     = 1 << 5
S_X        = 1 << 4
S_PAREN    = 1 << 3
S_Y        = 1 << 2
; use otherwise illegal combinations for the special cases
S_RELATIVE = S_X | S_PAREN | S_Y
.ifdef CPU_65C02
S_ZPREL    = S_X | S_Y
.endif

.macro addmode_detail symbol, bytes, flags
		symbol = * - addmode_detail_table
        .byte flags | bytes
.endmacro

addmode_detail_table:
        addmode_detail ADDMODE_IMP, 0, P_NONE ; implied
        addmode_detail ADDMODE_IMM, 1, P_HASH ; immediate
        addmode_detail ADDMODE_ZPG, 1, P_DOLLAR ; zero page
        addmode_detail ADDMODE_ABS, 2, P_DOLLAR ; absolute
.ifdef CPU_6502
        addmode_detail ADDMODE_IM2, 0, P_NONE ; implied
        addmode_detail ADDMODE_IM3, 0, P_NONE ; implied
.endif
        addmode_detail ADDMODE_IZX, 1, P_PAREN | S_X | S_PAREN ; X indexed indirect
        addmode_detail ADDMODE_IZY, 1, P_PAREN | S_PAREN | S_Y ; indirect Y indexed
        addmode_detail ADDMODE_ZPX, 1, P_DOLLAR | S_X ; zero page X indexed
        addmode_detail ADDMODE_ABX, 2, P_DOLLAR | S_X ; absolute X indexed
        addmode_detail ADDMODE_ABY, 2, P_DOLLAR | S_Y ; absolute Y indexed
        addmode_detail ADDMODE_IND, 2, P_PAREN | S_PAREN ; absolute indirect
        addmode_detail ADDMODE_ZPY, 1, P_DOLLAR | S_Y ; zero page Y indexed
        addmode_detail ADDMODE_REL,1, P_DOLLAR | S_RELATIVE ; relative
.ifdef CPU_65C02
        addmode_detail ADDMODE_IAX, 2, P_PAREN | S_X | S_PAREN ; X indexed indirect
        addmode_detail ADDMODE_IZP, 1, P_PAREN | S_PAREN ; zp indirect
        addmode_detail ADDMODE_ZPR, 2, P_DOLLAR | S_ZPREL ; zp, relative
.endif

.macro asmchars c1, c2
.segment "asmchars1"
        .byte c1
.segment "asmchars2"
        .byte c2
.endmacro

        ; suffixes
        asmchars ',', 'Y' ; 1
        asmchars ')', 0   ; 2
        asmchars ',', 'X' ; 3
        ; prefixes
        asmchars '#', '$' ; 4
        asmchars '(', '$' ; 5
        asmchars '$', 0   ; 6

; encoded mnemos:
; every combination of a byte of mnemos1 and mnemos2
; encodes 3 ascii characters

.macro mnemo c1, c2, c3
.segment "mnemos1"
        .byte (c1 - $3F) << 3 | (c2 - $3F) >> 2
.segment "mnemos2"
        .byte <((c2 - $3F) << 6 | (c3 - $3F) << 1)
.endmacro

.if .defined(CPU_6502)
; 64 entries
        mnemo 'B','R','K'
        mnemo 'P','H','P'
        mnemo 'B','P','L'
        mnemo 'C','L','C'
        mnemo 'J','S','R'
        mnemo 'P','L','P'
        mnemo 'B','M','I'
        mnemo 'S','E','C'
        mnemo 'R','T','I'
        mnemo 'P','H','A'
        mnemo 'B','V','C'
        mnemo 'C','L','I'
        mnemo 'R','T','S'
        mnemo 'P','L','A'
        mnemo 'B','V','S'
        mnemo 'S','E','I'
        mnemo '?','?','?'
        mnemo 'D','E','Y'
        mnemo 'B','C','C'
        mnemo 'T','Y','A'
        mnemo 'L','D','Y'
        mnemo 'T','A','Y'
        mnemo 'B','C','S'
        mnemo 'C','L','V'
        mnemo 'C','P','Y'
        mnemo 'I','N','Y'
        mnemo 'B','N','E'
        mnemo 'C','L','D'
        mnemo 'C','P','X'
        mnemo 'I','N','X'
        mnemo 'B','E','Q'
        mnemo 'S','E','D'
        mnemo '?','?','?'
        mnemo 'B','I','T'
        mnemo 'J','M','P'
        mnemo 'J','M','P'
        mnemo 'S','T','Y'
        mnemo 'L','D','Y'
        mnemo 'C','P','Y'
        mnemo 'C','P','X'
        mnemo 'T','X','A'
        mnemo 'T','X','S'
        mnemo 'T','A','X'
        mnemo 'T','S','X'
        mnemo 'D','E','X'
        mnemo '?','?','?'
        mnemo 'N','O','P'
        mnemo '?','?','?'
        mnemo 'A','S','L'
        mnemo 'R','O','L'
        mnemo 'L','S','R'
        mnemo 'R','O','R'
        mnemo 'S','T','X'
        mnemo 'L','D','X'
        mnemo 'D','E','C'
        mnemo 'I','N','C'
        mnemo 'O','R','A'
        mnemo 'A','N','D'
        mnemo 'E','O','R'
        mnemo 'A','D','C'
        mnemo 'S','T','A'
        mnemo 'L','D','A'
        mnemo 'C','M','P'
        mnemo 'S','B','C'
.elseif .defined(CPU_6502ILL)
        mnemo 'A','D','C'
        mnemo 'A','H','X'
        mnemo 'A','L','R'
        mnemo 'A','N','C'
        mnemo 'A','N','D'
        mnemo 'A','R','R'
        mnemo 'A','S','L'
        mnemo 'A','X','S'
        mnemo 'B','C','C'
        mnemo 'B','C','S'
        mnemo 'B','E','Q'
        mnemo 'B','I','T'
        mnemo 'B','M','I'
        mnemo 'B','N','E'
        mnemo 'B','P','L'
        mnemo 'B','R','K'
        mnemo 'B','V','C'
        mnemo 'B','V','S'
        mnemo 'C','L','C'
        mnemo 'C','L','D'
        mnemo 'C','L','I'
        mnemo 'C','L','V'
        mnemo 'C','M','P'
        mnemo 'C','P','X'
        mnemo 'C','P','Y'
        mnemo 'D','C','P'
        mnemo 'D','E','C'
        mnemo 'D','E','X'
        mnemo 'D','E','Y'
        mnemo 'E','O','R'
        mnemo 'I','N','C'
        mnemo 'I','N','X'
        mnemo 'I','N','Y'
        mnemo 'I','S','C'
        mnemo 'J','M','P'
        mnemo 'J','S','R'
        mnemo 'K','I','L'
        mnemo 'L','A','S'
        mnemo 'L','A','X'
        mnemo 'L','D','A'
        mnemo 'L','D','X'
        mnemo 'L','D','Y'
        mnemo 'L','S','R'
        mnemo 'N','O','P'
        mnemo 'O','R','A'
        mnemo 'P','H','A'
        mnemo 'P','H','P'
        mnemo 'P','L','A'
        mnemo 'P','L','P'
        mnemo 'R','L','A'
        mnemo 'R','O','L'
        mnemo 'R','O','R'
        mnemo 'R','R','A'
        mnemo 'R','T','I'
        mnemo 'R','T','S'
        mnemo 'S','A','X'
        mnemo 'S','B','C'
        mnemo 'S','E','C'
        mnemo 'S','E','D'
        mnemo 'S','E','I'
        mnemo 'S','H','X'
        mnemo 'S','H','Y'
        mnemo 'S','L','O'
        mnemo 'S','R','E'
        mnemo 'S','T','A'
        mnemo 'S','T','X'
        mnemo 'S','T','Y'
        mnemo 'T','A','S'
        mnemo 'T','A','X'
        mnemo 'T','A','Y'
        mnemo 'T','S','X'
        mnemo 'T','X','A'
        mnemo 'T','X','S'
        mnemo 'T','Y','A'
        mnemo 'X','A','A'
.elseif .defined(CPU_65C02)
        mnemo 'A','D','C'
        mnemo 'A','N','D'
        mnemo 'A','S','L'
        mnemo 'B','B','R'
        mnemo 'B','B','S'
        mnemo 'B','C','C'
        mnemo 'B','C','S'
        mnemo 'B','E','Q'
        mnemo 'B','I','T'
        mnemo 'B','M','I'
        mnemo 'B','N','E'
        mnemo 'B','P','L'
        mnemo 'B','R','A'
        mnemo 'B','R','K'
        mnemo 'B','V','C'
        mnemo 'B','V','S'
        mnemo 'C','L','C'
        mnemo 'C','L','D'
        mnemo 'C','L','I'
        mnemo 'C','L','V'
        mnemo 'C','M','P'
        mnemo 'C','P','X'
        mnemo 'C','P','Y'
        mnemo 'D','E','C'
        mnemo 'D','E','X'
        mnemo 'D','E','Y'
        mnemo 'E','O','R'
        mnemo 'I','N','C'
        mnemo 'I','N','X'
        mnemo 'I','N','Y'
        mnemo 'J','M','P'
        mnemo 'J','S','R'
        mnemo 'L','D','A'
        mnemo 'L','D','X'
        mnemo 'L','D','Y'
        mnemo 'L','S','R'
        mnemo 'N','O','P'
        mnemo 'O','R','A'
        mnemo 'P','H','A'
        mnemo 'P','H','P'
        mnemo 'P','H','X'
        mnemo 'P','H','Y'
        mnemo 'P','L','A'
        mnemo 'P','L','P'
        mnemo 'P','L','X'
        mnemo 'P','L','Y'
        mnemo 'R','M','B'
        mnemo 'R','O','L'
        mnemo 'R','O','R'
        mnemo 'R','T','I'
        mnemo 'R','T','S'
        mnemo 'S','B','C'
        mnemo 'S','E','C'
        mnemo 'S','E','D'
        mnemo 'S','E','I'
        mnemo 'S','M','B'
        mnemo 'S','T','A'
        mnemo 'S','T','P'
        mnemo 'S','T','X'
        mnemo 'S','T','Y'
        mnemo 'S','T','Z'
        mnemo 'T','A','X'
        mnemo 'T','A','Y'
        mnemo 'T','R','B'
        mnemo 'T','S','B'
        mnemo 'T','S','X'
        mnemo 'T','X','A'
        mnemo 'T','X','S'
        mnemo 'T','Y','A'
        mnemo 'W','A','I'
.else
.error "No CPU type specified!"
.endif

.segment "monitor_c"

; ----------------------------------------------------------------

s_regs: .byte   CR, "   PC  IRQ  BK AC XR YR SP NV#BDIZC", CR, 0

; ----------------------------------------------------------------


command_names:
        .byte   "M" ; N.B.: code relies on "M" being the first entry of this table!
command_index_d = * - command_names
        .byte   "D"
        .byte   ":"
        .byte   "A"
        .byte   "G"
        .byte   "X"
command_index_f = * - command_names
        .byte   "F"
command_index_h = * - command_names
        .byte   "H"
command_index_c = * - command_names
        .byte   "C"
        .byte   "T"
        .byte   "R"
command_index_l = * - command_names
        .byte   "L"
command_index_s = * - command_names
        .byte   "S"
        .byte   ","
        .byte   "O"
        .byte   "@"
        .byte   "$"
        .byte   "#"
        .byte   "*"
        .byte   "P"
        .byte   "E"
        .byte   "["
        .byte   "]"
command_index_i = * - command_names
        .byte   "I"
        .byte   "'"
        .byte   ";"
.ifdef CART_FC3
        .byte   "B"
.endif
command_names_end:

.define function_table \
           cmd_mid-1 \
           ,cmd_mid-1 \
           ,cmd_colon-1 \
           ,cmd_a-1 \
           ,cmd_g-1 \
           ,cmd_x-1 \
           ,cmd_fhct-1 \
           ,cmd_fhct-1 \
           ,cmd_fhct-1 \
           ,cmd_fhct-1 \
           ,cmd_r-1 \
           ,cmd_ls-1 \
           ,cmd_ls-1 \
           ,cmd_comma-1 \
           ,cmd_o-1 \
           ,cmd_at-1 \
           ,cmd_dollar-1 \
           ,cmd_hash-1 \
           ,cmd_asterisk-1 \
           ,cmd_p-1 \
           ,cmd_e-1 \
           ,cmd_leftbracket-1 \
           ,cmd_rightbracket-1 \
           ,cmd_mid-1 \
           ,cmd_singlequote-1 \
           ,cmd_semicolon-1 \

.ifdef CART_FC3
.define temp function_table
.undef function_table
.define function_table temp ,cmd_b-1
.undef temp
.endif

function_table_l:   .lobytes function_table
function_table_h:   .hibytes function_table

; ----------------------------------------------------------------
; "P" - set output to printer
; ----------------------------------------------------------------
cmd_p:
        lda     bank
        bmi     syn_err7 ; drive?
        ldx     #$FF
        lda     FA
        cmp     #4
        beq     LBC11 ; printer
        jsr     basin_cmp_cr
        beq     LBC16 ; no argument
        cmp     #','
        bne     syn_err7
        jsr     get_hex_byte
        tax
LBC11:  jsr     basin_cmp_cr
        bne     syn_err7
LBC16:  sta     KEYD
        inc     NDX
        lda     #4
        cmp     FA
        beq     LBC39 ; printer
        stx     SA
        sta     FA ; set device 4
        sta     LA
        ldx     #0
        stx     FNLEN
;        jsr     CLOSE
        jsr     OPEN
        ldx     LA
        jsr     CKOUT
        jmp     input_loop2

LBC39:  lda     LA
        jsr     CLOSE
        jsr     CLRCH
        lda     #8
        sta     FA
        lda     #0
        sta     NDX
        jmp     input_loop

; ----------------------------------------------------------------

syn_err7:
        jmp     syntax_error

; ----------------------------------------------------------------
; "*R"/"*W" - read/write sector
; ----------------------------------------------------------------
cmd_asterisk:
        jsr     BASIN
        cmp     #'W'
        beq     :+
        cmp     #'R'
        bne     syn_err7
:       sta     zp2 ; save 'R'/'W' mode
        jsr     basin_skip_spaces_if_more
        jsr     get_hex_byte2
;        bcc     syn_err7
        sta     zp1
        jsr     basin_if_more
        jsr     get_hex_byte
;        bcc     syn_err7
        sta     zp1 + 1
        jsr     basin_cmp_cr
        bne     LBAC1
        lda     #>$CF00 ; default address
        sta     zp2 + 1
        bne     LBACD
LBAC1:  jsr     get_hex_byte
;        bcc     syn_err7
        sta     zp2 + 1
        jsr     basin_cmp_cr
        bne     syn_err7
LBACD:  lda     #$F2
        jsr     listen_second
        lda     #'#'
        jsr     IECOUT
        jsr     UNLSTN
        jsr     swap_zp1_and_zp2
        lda     zp1
        cmp     #'W'
        beq     LBB25
        lda     #'1' ; U1: read
        jsr     read_write_block
        jsr     command_channel_talk
        jsr     IECIN
        cmp     #'0'
        beq     LBB00 ; no error
        pha
        jsr     print_cr
        pla
LBAED:  jsr     LE716 ; KERNAL: output character to screen
        jsr     IECIN
        cmp     #CR ; print drive status until CR (XXX redundant?)
        bne     LBAED
        jsr     UNTALK
        jsr     close_ch2
        jmp     input_loop

LBB00:  jsr     IECIN
        cmp     #CR ; receive all bytes (XXX not necessary?)
        bne     LBB00
        jsr     UNTALK
        jsr     send_bp
        lda     #$62
        jsr     talk_second
        ldy     #0
        sty     zp1
:       jsr     IECIN
        jsr     store_byte ; receive block
        iny
        bne     :-
        jsr     UNTALK
        jmp     LBB42 ; close 2 and print drive status

LBB25:  jsr     send_bp
        lda     #$62
        jsr     listen_second
        ldy     #0
        sty     zp1
:       jsr     load_byte
        jsr     IECOUT ; send block
        iny
        bne     :-
        jsr     UNLSTN
        lda     #'2' ; U2: write
        jsr     read_write_block
LBB42:  jsr     close_ch2
        jmp     print_drive_status


to_dec:
        ldx     #'0' - 1
        sec
:       inx
        sbc     #10
        bcs     :-
        adc     #'9' + 1
        rts

read_write_block:
        ;
        ; The B-R and B-W commands have serious bugs. Details can be found in the book
        ;  "Die Floppy 1571" by Karsten Schramm, chapter 14 "Fehler im DOS 3.0"
        ; ( the 1541 has these bugs too).
        ;
        ; The U1/U2 commands are used instead.
        ;
        pha
        ; Copy s_u1 to temp buffer
        ldx     #256-s_u1_len
:       lda     s_u1+s_u1_len-256,x
        sta     BUF+s_u1_len-256,x
        inx
        bne     :-
        pla
        sta     BUF + 1  ; U1 or U2
        lda     zp2 ; track
        jsr     to_dec
        stx     BUF + s_u1_len - 3
        sta     BUF + s_u1_len - 2
        lda     zp2 + 1 ; sector
        jsr     to_dec
        stx     BUF + s_u1_len + 0
        sta     BUF + s_u1_len + 1
        jsr     listen_command_channel
        ldx     #256-(s_u1_len+2)
:       lda     BUF+s_u1_len+2-256,x
        jsr     IECOUT
        inx
        bne     :-
        jmp     UNLSTN

send_bp:
        jsr     listen_command_channel
        ldx     #256-s_bp_len
:       lda     s_bp+s_bp_len-256,x
        jsr     IECOUT
        inx
        bne     :-
        jmp     UNLSTN

s_u1:
        .byte   "U1:2 0 xx "
s_u1_len =  * - s_u1

s_bp:
        .byte   "B-P 2 0"
s_bp_len = * - s_bp

send_m_dash2:
        pha
        jsr     cmd_channel_listen
        lda     #'M'
        jsr     IECOUT
        lda     #'-'
        jsr     IECOUT
        pla
        jmp     IECOUT

iec_send_zp1_plus_y:
        tya
        clc
        adc     zp1
        php
        jsr     IECOUT
        plp
        lda     zp1 + 1
        adc     #0
        jmp     IECOUT


LBC4C:  stx     zp1
        sta     zp1 + 1
LBC50:  lda     #$31
        sta     zp2
        ldx     #4
LBC56:  dec     zp2
LBC58:  lda     #$2F
        sta     zp2 + 1
        sec
        ldy     zp1
        .byte   $2C
LBC60:  sta     zp1 + 1
        sty     zp1
        inc     zp2 + 1
        tya
        sbc     pow10lo,x
        tay
        lda     zp1 + 1
        sbc     pow10hi,x
        bcs     LBC60
        lda     zp2 + 1
        cmp     zp2
        beq     LBC7D
        jsr     LE716 ; KERNAL: output character to screen
        dec     zp2
LBC7D:  dex
        beq     LBC56
        bpl     LBC58
        rts

print_hex_byte:
        jsr     byte_to_hex_ascii
        jsr     BSOUT
        tya
        jmp     BSOUT

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
