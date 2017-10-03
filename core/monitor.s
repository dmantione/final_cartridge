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

.include "kernal.i"
.include "persistent.i"

; from vectors
.import jfast_format

; from printer
.import set_io_vectors
.import set_io_vectors_with_hidden_rom

.global monitor

.if 1
; C-64
LE50C := $E50C ; set cursor position
LE716 := $E716 ; screen CHROUT
LE96C := $E96C ; insert line at top of screen
LEA31 := $EA31 ; default contents of CINV vector (VIC-20: $EABF)
LF0BD := $F0BD ; string "I/O ERROR"
LF333 := $F333 ; default contents of CLRCHN vector
LF646 := $F646 ; IEC close
CHARS_PER_LINE := 40
.else
; VIC-20
LE50C := $E50C ; set cursor position
LE716 := $E742 ; screen CHROUT
LE96C := $E9F5 ; insert line at top of screen
LEA31 := $EABF ; default contents of CINV vector (VIC-20: $EABF)
LF0BD := $F174 ; string "I/O ERROR"
LF333 := $F3F3 ; default contents of CLRCHN vector
LF646 := $F6DE ; IEC close
CHARS_PER_LINE := 22
.endif

CINV   := $0314 ; IRQ vector
CBINV  := $0316 ; BRK vector
ICLRCH := $0322 ; CLRCHN vector
IBSOUT := $0326 ; CHROUT vector

FC3CFG := $DFFF ; Final Cartridge III banking config register

; variables
zp1             := $C1
zp2             := $C3
zp3             := $FF

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

.segment "monitor_a"

.import __monitor_ram_code_LOAD__
.import __monitor_ram_code_RUN__

.import __mnemos1_RUN__
.import __mnemos2_RUN__

.import __asmchars1_RUN__
.import __asmchars2_RUN__

monitor:
        lda     #<brk_entry
        sta     CBINV
        lda     #>brk_entry
        sta     CBINV + 1 ; BRK vector
        lda     #'C'
        sta     entry_type
        lda     #$37
        sta     bank ; bank 7
        lda     #$70
        sta     cartridge_bank ; by default, hide cartridge
        ldx     #ram_code_end - ram_code - 1
:       lda     __monitor_ram_code_LOAD__,x
        sta     __monitor_ram_code_RUN__,x
        dex
        bpl     :-
        brk ; <- nice!

.segment "monitor_ram_code"
; code that will be copied to $0220
ram_code:
; read from memory with a specific ROM and cartridge config
        sta     FC3CFG ; set cartridge config
        pla
        sta     R6510 ; set ROM config
        lda     (zp1),y ; read
enable_all_roms:
        pha
        lda     #$37
        sta     R6510 ; restore ROM config
        lda     #$40
        sta     FC3CFG ; resture cartridge config
        pla
        rts

disable_rom_rti:
        jsr     _disable_fc3rom
        sta     R6510
        lda     reg_a
        rti

brk_entry:
        jsr     enable_all_roms
        jmp     brk_entry2
ram_code_end:

; XXX ram_code is here - why put it between ROM code, so we have to jump over it?

.segment "monitor_b"

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
        jsr     set_irq_vector
        jsr     set_io_vectors
        jsr     print_cr
        lda     entry_type
        cmp     #'C'
        bne     :+
        .byte   $2C ; XXX bne + skip = beq + 2
:       lda     #'B'
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
        lda     #'B'
        sta     entry_type
        lda     #$80
        sta     RPTFLG ; enable key repeat for all keys
        bne     dump_registers ; always

; ----------------------------------------------------------------
; "R" - dump registers
; ----------------------------------------------------------------
cmd_r:
        jsr     basin_cmp_cr
        bne     syntax_error
dump_registers:
        ldx     #0
:       lda     s_regs,x ; "PC  IRQ  BK AC XR YR SP NV#BDIZC"
        beq     dump_registers2
        jsr     BSOUT
        inx
        bne     :-
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
        bpl     :+
        lda     #'D'
        jsr     BSOUT
        lda     #'R'
        jsr     BSOUT
        bne     LABEB ; negative bank means drive ("DR")
:       and     #$0F
        jsr     print_hex_byte2 ; bank
LABEB:  ldy     #0
:       jsr     print_space
        lda     registers,y
        jsr     print_hex_byte2 ; registers...
        iny
        cpy     #4
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
        ldx     #$1A
LAC27:  cmp     command_names,x
        bne     LAC3B
        stx     command_index
        txa
        asl     a
        tax
        lda     function_table + 1,x
        pha
        lda     function_table,x
        pha
        rts
LAC3B:  dex
        bpl     LAC27
        bmi     syntax_error ; always

; ----------------------------------------------------------------
; "EC"/"ES"/"D" - dump character or sprite data
; ----------------------------------------------------------------
cmd_e:
        jsr     BASIN
        cmp     #'C'
        beq     cmd_mid2
        cmp     #'S'
        beq     cmd_mid2
        jmp     syntax_error

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
        jmp     LAE88

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
LACFD:  jsr     load_byte
        jsr     print_bin
        iny
        cpy     #3
        bne     LACFD
        jsr     print_8_spaces
        tya ; 3
        jmp     add_a_to_zp1

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
        jsr     disassemble_line; XXX why not inline?
        jsr     print_8_spaces
        lda     num_asm_bytes
        jmp     sadd_a_to_zp1

disassemble_line:
        jsr     print_hex_16
        jsr     print_space
        jsr     decode_mnemo
        jsr     print_asm_bytes
        jsr     print_mnemo
        jmp     print_operand

; ----------------------------------------------------------------
; "[" - input character data
; ----------------------------------------------------------------
cmd_leftbracket:
        jsr     get_hex_word
        jsr     copy_zp2_to_zp1
        jsr     basin_skip_spaces_if_more
        jsr     LB4DB
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
        jsr     LB4DB
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
        ora     #$30
LAE1B:  sta     bank
        ldx     #0
LAE20:  jsr     basin_if_more
        jsr     get_hex_byte
        sta     registers,x ; registers
        inx
        cpx     #4
        bne     LAE20
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
        jsr     LB030
        jsr     LB05C
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

LAE88:  jsr     check_end
        bcs     LAE90
        jmp     syntax_error

LAE90:  sty     tmp10
        jsr     basin_if_more
        jsr     get_hex_word3
        lda     command_index
        cmp     #command_index_c
        beq     LAEA6
        jsr     LB1CB
        jmp     print_cr_then_input_loop

LAEA6:  jsr     LB245
        jmp     input_loop

LAEAC:  jsr     basin_if_more
        ldx     #0
        stx     tmp11 ; XXX unused
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
        jmp     syntax_error

LAECF:  jsr     get_hex_byte2
        bcs     LAEDC
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
        jmp     syntax_error

LAF03:  jsr     copy_pc_to_zp2_and_zp1
LAF06:  lda     bank
        bmi     LAF2B ; drive
        jsr     set_irq_vector
        jsr     set_io_vectors_with_hidden_rom
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
LAF2B:  lda     #'E' ; send M-E to drive
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
LAF43:  cpy     num_asm_bytes
        beq     LAF52
        bcc     LAF52
        jsr     print_space
        jsr     print_space
        bcc     LAF58
LAF52:  jsr     load_byte
        jsr     print_hex_byte2
LAF58:  jsr     print_space
        iny
        cpy     #3
        bne     LAF43
        pla
        rts

; returns mnemo index in A
decode_mnemo:
        ldy     #0
        jsr     load_byte; opcode
LAF67:  tay
        lsr     a
        bcc     LAF76 ; skip if bit 0 is clear
        lsr     a
        bcs     LAF85 ; skip more if bit 1 is set
        cmp     #$22
        beq     LAF85 ; code $89?
        and     #$07 ; opcode bits 4,3,2
        ora     #$80 ; use special bytes past first 64
LAF76:  lsr     a
        tax
        lda     addmode_table,x
        bcs     LAF81 ; bit 7 set; then use low nybble
        lsr     a
        lsr     a
        lsr     a
        lsr     a ; otherwise get hi nybble
LAF81:  and     #$0F
        bne     LAF89 ; if nybble is 0, Y = $80
LAF85:  ldy     #$80
        lda     #0
LAF89:  tax
        lda     addmode_detail_table,x ; X = 0..13
        sta     prefix_suffix_bitfield
        and     #$03
        sta     num_asm_bytes
        tya
        and     #$8F
        tax
        tya
        ldy     #3
        cpx     #$8A
        beq     LAFAB
LAFA0:  lsr     a
        bcc     LAFAB
        lsr     a
LAFA4:  lsr     a
        ora     #$20
        dey
        bne     LAFA4
        iny
LAFAB:  dey
        bne     LAFA0
        rts

; prints name of mnemo in A
print_mnemo:
        tay
        lda     __mnemos1_RUN__,y
        sta     tmp10
        lda     __mnemos2_RUN__,y
        sta     tmp8
        ldx     #3
LAFBE:  lda     #0
        ldy     #5
LAFC2:  asl     tmp8
        rol     tmp10
        rol     a
        dey
        bne     LAFC2
        adc     #$3F
        jsr     BSOUT
        dex
        bne     LAFBE
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
        cmp     #$E8 ; "($FF,X),Y" addressing mode? relative!
        php
        jsr     load_byte
        plp
        bcs     print_brach_target
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

print_brach_target:
        jsr     zp1_plus_a_2
        tax
        inx
        bne     :+
        iny
:       tya
        jsr     print_hex_byte2
        txa
        jmp     print_hex_byte2

; adds signed A to 16 bit zp1
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

LB030:  ldx     #0
        stx     tmp17
LB035:  jsr     basin_if_more
        cmp     #' '
        beq     LB030
        sta     BUF,x
        inx
        cpx     #3
        bne     LB035
LB044:  dex
        bmi     LB05B
        lda     BUF,x
        sec
        sbc     #$3F
        ldy     #5
LB04F:  lsr     a
        ror     tmp17
        ror     tmp16
        dey
        bne     LB04F
        beq     LB044
LB05B:  rts

LB05C:  ldx     #2
LB05E:  jsr     BASIN
        cmp     #CR
        beq     LB089
        cmp     #':'
        beq     LB089
        cmp     #' '
        beq     LB05E
        jsr     is_hex_character
        bcs     LB081
        jsr     get_hex_byte3
        ldy     zp1
        sty     zp1 + 1
        sta     zp1
        lda     #'0'
        sta     tmp16,x
        inx
LB081:  sta     tmp16,x
        inx
        cpx     #$17
        bcc     LB05E
LB089:  stx     tmp10
        rts

LB08D:  ldx     #0
        stx     tmp4
        lda     tmp6
        jsr     LAF67
        ldx     prefix_suffix_bitfield
        stx     tmp8
        tax
        lda     __mnemos2_RUN__,x
        jsr     LB130
        lda     __mnemos1_RUN__,x
        jmp     LB130

LB0AB:  ldx     #6
LB0AD:  cpx     #3
        bne     LB0C5
        ldy     num_asm_bytes
        beq     LB0C5
LB0B6:  lda     prefix_suffix_bitfield
        cmp     #$E8
        lda     #$30
        bcs     LB0DD
        jsr     LB12D
        dey
        bne     LB0B6
LB0C5:  asl     prefix_suffix_bitfield
        bcc     LB0D8
        lda     __asmchars1_RUN__ - 1,x
        jsr     LB130
        lda     __asmchars2_RUN__ - 1,x
        beq     LB0D8
        jsr     LB130
LB0D8:  dex
        bne     LB0AD
        beq     LB0E3
LB0DD:  jsr     LB12D
        jsr     LB12D
LB0E3:  lda     tmp10
        cmp     tmp4
        beq     LB0EE
        jmp     LB13B

LB0EE:  rts

LB0EF:  ldy     num_asm_bytes
        beq     LB123
        lda     tmp8
        cmp     #$9D
        bne     LB11A
        jsr     check_end
        bcc     LB10A
        tya
        bne     LB12A
        ldx     tmp9
        bmi     LB12A
        bpl     LB112
LB10A:  iny
        bne     LB12A
        ldx     tmp9
        bpl     LB12A
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

LB12A:  jmp     input_loop

LB12D:  jsr     LB130
LB130:  stx     tmp3
        ldx     tmp4
        cmp     tmp16,x
        beq     LB146
LB13B:  inc     tmp6
        beq     LB143
        jmp     LAE61

LB143:  jmp     input_loop

LB146:  inx
        stx     tmp4
        ldx     tmp3
        rts

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
        ldy     #0
        sty     zp1
        sty     zp1 + 1
        jsr     basin_skip_spaces_if_more
LB16F:  and     #$0F
        clc
        adc     zp1
        sta     zp1
        bcc     LB17A
        inc     zp1 + 1
LB17A:  jsr     BASIN
        cmp     #$30
        bcc     LB19B
        pha
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
        pla
        bcc     LB16F
LB19B:  jsr     print_up_dot
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

; ----------------------------------------------------------------
; "X" - exit monitor
; ----------------------------------------------------------------
cmd_x:
        jsr     set_irq_vector
        jsr     set_io_vectors_with_hidden_rom
        lda     #0
        sta     RPTFLG
        ldx     reg_s
        txs
        jmp     _basic_warm_start

LB1CB:  lda     zp2
        cmp     zp1
        lda     zp2 + 1
        sbc     zp1 + 1
        bcs     LB1FC
        ldy     #0
        ldx     #0
LB1D9:  jsr     load_byte
        pha
        jsr     swap_zp1_and_zp2
        pla
        jsr     store_byte
        jsr     swap_zp1_and_zp2
        cpx     tmp10
        bne     LB1F1
        cpy     tmp9
        beq     LB1FB
LB1F1:  iny
        bne     LB1D9
        inc     zp1 + 1
        inc     zp2 + 1
        inx
        bne     LB1D9
LB1FB:  rts

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
        jmp     LB20E

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
LB296:  jsr     check_end
        bcc     LB2B3
        ldy     #0
LB29D:  jsr     load_byte
        cmp     BUF,y
        bne     LB2AE
        iny
        cpy     command_index
        bne     LB29D
        jsr     print_space_hex_16
LB2AE:  jsr     inc_zp1
        bne     LB296
LB2B3:  rts

; ----------------------------------------------------------------
; memory load/store
; ----------------------------------------------------------------

; loads a byte at (zp1),y from drive RAM
LB2B4:  lda     #'R' ; send M-R to drive
        jsr     send_m_dash2
        jsr     iec_send_zp1_plus_y
        jsr     UNLSTN
        jsr     talk_cmd_channel
        jsr     IECIN ; read byte
        pha
        jsr     UNTALK
        pla
        rts

; stores a byte at (zp1),y in drive RAM
LB2CB:  lda     #'W' ; send M-W to drive
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

; ??? unreferenced?
        lda     (zp1),y
        rts

; ??? unreferenced?
        pla
        sta     (zp1),y
        rts

; loads a byte at (zp1),y from RAM with the correct ROM config
load_byte:
        sei
        lda     bank
        bmi     LB2B4 ; drive
        clc
        pha
        lda     cartridge_bank
        jmp     ram_code ; "lda (zp1),y" with ROM and cartridge config

; stores a byte at (zp1),y in RAM with the correct ROM config
store_byte:
        sei
        pha
        lda     bank
        bmi     LB2CB ; drive
        cmp     #$35
        bcs     LB306 ; I/O on
        lda     #$33 ; ROM at $A000, $D000 and $E000
        sta     R6510 ; ??? why?
LB306:  pla
        sta     (zp1),y ; store
        pha
        lda     #$37
        sta     R6510 ; restore ROM config
        pla
        rts

; ----------------------------------------------------------------
; "B" - set cartridge bank (0-3) to be visible at $8000-$BFFF
;       without arguments, this turns off cartridge visibility
; ----------------------------------------------------------------
cmd_b:  jsr     basin_cmp_cr
        beq     LB326 ; without arguments, set $70
        cmp     #' '
        beq     cmd_b ; skip spaces
        cmp     #'0'
        bcc     syn_err3
        cmp     #'4'
        bcs     syn_err3
        and     #$03 ; XXX no effect
        ora     #$40 ; make $40 - $43
        .byte   $2C
LB326:  lda     #$70 ; by default, hide cartridge
        sta     cartridge_bank
        jmp     print_cr_then_input_loop

syn_err3:
        jmp     syntax_error

; ----------------------------------------------------------------
; "O" - set bank
;       0 to 7 map to a $01 value of $30-$37, "D" switches to drive
;       memory
; ----------------------------------------------------------------
cmd_o:
        jsr     basin_cmp_cr
        beq     LB33F ; without arguments: bank 7
        cmp     #' '
        beq     cmd_o
        cmp     #'D'
        beq     LB34A ; disk
        .byte   $2C
LB33F:  lda     #$37 ; bank 7
        cmp     #$38
        bcs     syn_err3
        cmp     #$30
        bcc     syn_err3
        .byte   $2C
LB34A:  lda     #$80 ; drive
        sta     bank
        jmp     print_cr_then_input_loop

listen_command_channel:
        lda     #$6F
        jsr     init_and_listen
        lda     ST
        bmi     LB3A6
        rts

restore_bsout_chrch: ; set_io_vectors in printer.s changes these; change them back
        lda     #<LE716
        sta     IBSOUT
        lda     #>LE716
        sta     IBSOUT + 1
        lda     #<LF333
        sta     ICLRCH
        lda     #>LF333
        sta     ICLRCH + 1
        rts

; ----------------------------------------------------------------
; "L"/"S" - load/save file
; ----------------------------------------------------------------
cmd_ls:
        ldy     #>tmp16
        sty     FNADR + 1
        dey
        sty     SA  ; = 1
        dey
        sty     FNLEN  ; = 1
        lda     #8
        sta     FA
        lda     #<tmp16
        sta     FNADR
        jsr     basin_skip_spaces_cmp_cr
        bne     LB3B6
LB388:  lda     command_index
        cmp     #command_index_l
        bne     syn_err4
LB38F:  jsr     restore_bsout_chrch
        jsr     set_irq_vector
        ldx     zp1
        ldy     zp1 + 1
        jsr     LB42D
        php
        jsr     set_io_vectors
        jsr     set_irq_vector
        plp
LB3A4:  bcc     LB3B3
LB3A6:  ldx     #0
LB3A8:  lda     LF0BD,x ; "I/O ERROR"
        jsr     BSOUT
        inx
        cpx     #10
        bne     LB3A8
LB3B3:  jmp     input_loop

LB3B6:  cmp     #'"'
        bne     syn_err4
LB3BA:  jsr     basin_cmp_cr
        beq     LB388
        cmp     #'"'
        beq     LB3CF
        sta     (FNADR),y
        inc     FNLEN
        iny
        cpy     #$10
        bne     LB3BA
syn_err4:
        jmp     syntax_error

LB3CF:  jsr     basin_cmp_cr
        beq     LB388
        cmp     #','
LB3D6:  bne     syn_err4
        jsr     get_hex_byte
        and     #$0F
        beq     syn_err4
        cmp     #1 ; tape
        beq     LB3E7
        cmp     #4
        bcc     syn_err4 ; illegal device number
LB3E7:  sta     FA
        jsr     basin_cmp_cr
        beq     LB388
        cmp     #','
LB3F0:  bne     LB3D6
        jsr     get_hex_word3
        jsr     swap_zp1_and_zp2
        jsr     basin_cmp_cr
        bne     LB408
        lda     command_index
        cmp     #command_index_l
        bne     LB3F0
        dec     SA
        beq     LB38F
LB408:  cmp     #','
LB40A:  bne     LB3F0
        jsr     get_hex_word3
        jsr     basin_skip_spaces_cmp_cr
        bne     LB40A
        ldx     zp2
        ldy     zp2 + 1
        lda     command_index
        cmp     #command_index_s
        bne     LB40A
        dec     SA
        jsr     restore_bsout_chrch
        jsr     LB438
        jsr     set_io_vectors
        jmp     LB3A4

LB42D:  lda     #>(_enable_fcbank0 - 1)
        pha
        lda     #<(_enable_fcbank0 - 1)
        pha
        lda     #0
        jmp     LOAD

LB438:  lda     #>(_enable_fcbank0 - 1)
        pha
        lda     #<(_enable_fcbank0 - 1)
        pha
        lda     #zp1 ; pointer to ZP location with address
        jmp     SAVE

; ----------------------------------------------------------------
; "@" - send drive command
;       without arguments, this reads the drive status
;       $ shows the directory
;       F does a fast format
; ----------------------------------------------------------------
cmd_at: 
        jsr     listen_command_channel
        jsr     basin_cmp_cr
        beq     print_drive_status
        cmp     #'$'
        beq     LB475
        cmp     #'F'
        bne     LB458
        jsr     jfast_format
        lda     #'F'
LB458:  jsr     IECOUT
        jsr     basin_cmp_cr
        bne     LB458
        jsr     UNLSTN
        jmp     print_cr_then_input_loop

; just print drive status
print_drive_status:
        jsr     print_cr
        jsr     UNLSTN
        jsr     talk_cmd_channel
        jsr     cat_line_iec
        jmp     input_loop

; show directory
LB475:  jsr     UNLSTN
        jsr     print_cr
        lda     #$F0 ; sec address
        jsr     init_and_listen
        lda     #'$'
        jsr     IECOUT
        jsr     UNLSTN
        jsr     directory
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
; XXX unused?
        lda     #CSR_RIGHT
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

LB4DB:  pha
        ldx     #8
        bne     LB4E6

get_bin_byte:
        ldx     #8
LB4E2:  pha
        jsr     basin_if_more
LB4E6:  cmp     #'*'
        beq     LB4EB
        clc
LB4EB:  pla
        rol     a
        dex
        bne     LB4E2
        rts

; get a 16 bit ASCII hex number from the user, return it in zp2
get_hex_word:
        jsr     basin_if_more
get_hex_word2:
        cmp     #' ' ; skip spaces
        beq     get_hex_word
        jsr     get_hex_byte2
        bcs     LB500 ; ??? always
get_hex_word3:
        jsr     get_hex_byte
LB500:  sta     zp2 + 1
        jsr     get_hex_byte
        sta     zp2
        rts

; get a 8 bit ASCII hex number from the user, return it in A
get_hex_byte:
        lda     #0
        sta     tmp2 ; XXX not necessary
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

hex_digit_to_nybble:
        cmp     #'9' + 1
        and     #$0F
        bcc     LB530
        adc     #'A' - '9'
LB530:  rts

; ??? unused?
        clc
        rts

; get character and check for legal ASCII hex digit
; XXX this also allows ":;<=>?" (0x39-0x3F)!!!
get_hex_digit:
        jsr     basin_if_more
validate_hex_digit:
        cmp     #'0'
        bcc     syn_err5
        cmp     #'@' ; XXX should be: '9' + 1
        bcc     LB546 ; ok
        cmp     #'A'
        bcc     syn_err5
        cmp     #'F' + 1
        bcs     syn_err5
LB546:  rts
syn_err5:
        jmp     syntax_error

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
LB565:  rol     a
        pha
        lda     #'*'
        bcs     :+
        lda     #'.'
:       jsr     BSOUT
        pla
        dex
        bne     LB565
        rts

inc_zp1:
        clc
        inc     zp1
        bne     :+
        inc     zp1 + 1
        sec
:       rts

dump_8_hex_bytes:
        ldx     #8
        ldy     #0
:       jsr     print_space
        jsr     load_byte
        jsr     print_hex_byte2
        iny
        dex
        bne     :-
        rts

dump_8_ascii_characters:
        ldx     #8
dump_ascii_characters:
        ldy     #0
LB594:  jsr     load_byte
        cmp     #$20
        bcs     LB59F
        inc     RVS
        ora     #$40
LB59F:  cmp     #$80
        bcc     LB5AD
        cmp     #$A0
        bcs     LB5AD
        and     #$7F
        ora     #$60
        inc     RVS
LB5AD:  jsr     BSOUT
        lda     #0
        sta     RVS
        sta     QTSW
        iny
        dex
        bne     LB594
        tya ; number of bytes consumed
        jmp     add_a_to_zp1

read_ascii:
        ldx     #$20
        ldy     #0
        jsr     copy_zp2_to_zp1
        jsr     basin_if_more
LB5C8:  sty     tmp9
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
        dex
        bne     LB5C8
        rts

read_8_bytes:
        ldx     #8
read_x_bytes:
        ldy     #0
        jsr     copy_zp2_to_zp1
        jsr     basin_skip_spaces_if_more
        jsr     get_hex_byte2
        jmp     LB607

LB5F5:  jsr     basin_if_more_cmp_space ; ignore character where space should be
        jsr     basin_if_more_cmp_space
        bne     LB604 ; not space
        jsr     basin_if_more_cmp_space
        bne     syn_err6 ; not space
        beq     LB60A ; always

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

syn_err6:
        jmp     syntax_error

; XXX this detects :;<=>?@ as hex characters, see also get_hex_digit
is_hex_character:
        cmp     #'0'
        bcc     :+
        cmp     #'F' + 1
        rts
:       sec
        rts

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

; print 7x cursor right
print_7_csr_right:
        lda     #CSR_RIGHT
        ldx     #7
        bne     LB6AC ; always

; print 8 spaces - this is used to clear some leftover characters
; on the screen when re-dumping a line with proper spacing after the
; user may have entered it with condensed spacing
print_8_spaces:
        lda     #' '
        ldx     #8
LB6AC:  jsr     BSOUT
        dex
        bne     LB6AC
        rts

; ----------------------------------------------------------------
; IRQ logic to handle F keys and scrolling
; ----------------------------------------------------------------
set_irq_vector:
        lda     CINV
        cmp     #<irq_handler
        bne     LB6C1
        lda     CINV + 1
        cmp     #>irq_handler
        beq     LB6D3
LB6C1:  lda     CINV
        ldx     CINV + 1
        sta     irq_lo
        stx     irq_hi
        lda     #<irq_handler
        ldx     #>irq_handler
        bne     LB6D9 ; always
LB6D3:  lda     irq_lo
        ldx     irq_hi
LB6D9:  sei
        sta     CINV
        stx     CINV + 1
        cli
        rts

irq_handler:
        lda     #>after_irq ; XXX shouldn't this be "-1"?
        pha
        lda     #<after_irq
        pha
        lda     #0 ; fill A/X/Y/P
        pha
        pha
        pha
        pha
        jmp     LEA31 ; run normal IRQ handler, then return to this code

after_irq:
        lda     disable_f_keys
        bne     LB6FA
        lda     NDX
        bne     LB700
LB6FA:  pla ; XXX JMP $EA81
        tay
        pla
        tax
        pla
        rti

LB700:  lda     KEYD
        cmp     #KEY_F7
        bne     LB71C
        lda     #'@'
        sta     KEYD
        lda     #'$'
        sta     KEYD + 1
        lda     #CR
        sta     KEYD + 2 ; store "@$' + CR into keyboard buffer
        lda     #3
        sta     NDX
        bne     LB6FA ; always

LB71C:  cmp     #KEY_F5
        bne     LB733
        ldx     #24
        cpx     TBLX
        beq     LB72E ; already on last line
        jsr     LB8D9
        ldy     PNTR
.if 0
        clc
        jsr     $FFF0 ; KERNAL set cursor position
.else
        jsr     LE50C ; KERNAL set cursor position
.endif
LB72E:  lda     #CSR_DOWN
        sta     KEYD
LB733:  cmp     #KEY_F3
        bne     LB74A
        ldx     #0
        cpx     TBLX
        beq     LB745
        jsr     LB8D9
        ldy     PNTR
.if 0
        clc
        jsr     $FFF0 ; KERNAL set cursor position
.else
        jsr     LE50C ; KERNAL set cursor position
.endif
LB745:  lda     #CSR_UP
        sta     KEYD
LB74A:  cmp     #CSR_DOWN
        beq     LB758
        cmp     #CSR_UP
        bne     LB6FA
        lda     TBLX
        beq     LB75E ; top of screen
        bne     LB6FA
LB758:  lda     TBLX
        cmp     #24
        bne     LB6FA
LB75E:  jsr     LB838
        bcc     LB6FA
        jsr     LB897
        php
        jsr     LB8D4
        plp
        bcs     LB6FA
        lda     TBLX
        beq     LB7E1
        lda     tmp12
        cmp     #','
        beq     LB790
        cmp     #'['
        beq     LB7A2
        cmp     #']'
        beq     LB7AE
        cmp     #$27 ; "'"
        beq     LB7BC
        jsr     LB8C8
        jsr     print_cr
        jsr     dump_hex_line
        jmp     LB7C7

LB790:  jsr     decode_mnemo
        lda     num_asm_bytes
        jsr     sadd_a_to_zp1
        jsr     print_cr
        jsr     dump_assembly_line
        jmp     LB7C7

LB7A2:  jsr     inc_zp1
        jsr     print_cr
        jsr     dump_char_line
        jmp     LB7C7

LB7AE:  lda     #3
        jsr     add_a_to_zp1
        jsr     print_cr
        jsr     dump_sprite_line
        jmp     LB7C7

LB7BC:  lda     #$20
        jsr     add_a_to_zp1
        jsr     print_cr
        jsr     dump_ascii_line
LB7C7:  lda     #CSR_UP
        ldx     #CR
        bne     LB7D1
LB7CD:  lda     #CR
        ldx     #CSR_HOME
LB7D1:  ldy     #0
        sty     NDX
        sty     disable_f_keys
        jsr     print_a_x
        jsr     print_7_csr_right
        jmp     LB6FA

LB7E1:  jsr     scroll_down
        lda     tmp12
        cmp     #','
        beq     LB800
        cmp     #'['
        beq     LB817
        cmp     #']'
        beq     LB822
        cmp     #$27 ; "'"
        beq     LB82D
        jsr     LB8EC
        jsr     dump_hex_line
        jmp     LB7CD

LB800:  jsr     swap_zp1_and_zp2
        jsr     LB90E
        inc     num_asm_bytes
        lda     num_asm_bytes
        eor     #$FF
        jsr     sadd_a_to_zp1
        jsr     dump_assembly_line
        clc
        bcc     LB7CD
LB817:  lda     #1
        jsr     LB8EE
        jsr     dump_char_line
        jmp     LB7CD

LB822:  lda     #3
        jsr     LB8EE
        jsr     dump_sprite_line
        jmp     LB7CD

LB82D:  lda     #$20
        jsr     LB8EE
        jsr     dump_ascii_line
        jmp     LB7CD

LB838:  lda     PNT
        ldx     PNT + 1
        sta     zp2
        stx     zp2 + 1
        lda     #$19
        sta     tmp13
LB845:  ldy     #1
        jsr     LB88B
        cmp     #':'
        beq     LB884
        cmp     #','
        beq     LB884
        cmp     #'['
        beq     LB884
        cmp     #']'
        beq     LB884
        cmp     #$27 ; "'"
        beq     LB884
        dec     tmp13
        beq     LB889
        lda     KEYD
        cmp     #CSR_DOWN
        bne     LB877
        sec
        lda     zp2
        sbc     #CHARS_PER_LINE
        sta     zp2
        bcs     LB845
        dec     zp2 + 1
        bne     LB845
LB877:  clc
        lda     zp2
        adc     #CHARS_PER_LINE
        sta     zp2
        bcc     LB845
        inc     zp2 + 1
        bne     LB845
LB884:  sec
        sta     tmp12
        rts

LB889:  clc
        rts

LB88B:  lda     (zp2),y
        iny
        and     #$7F
        cmp     #$20
        bcs     LB896
        ora     #$40
LB896:  rts

LB897:  cpy     #$16
        bne     LB89D
        sec
        rts

LB89D:  jsr     LB88B
        cmp     #$20
        beq     LB897
        dey
        jsr     LB8B1
        sta     zp1 + 1
        jsr     LB8B1
        sta     zp1
        clc
        rts

LB8B1:  jsr     LB88B
        jsr     hex_digit_to_nybble
        asl     a
        asl     a
        asl     a
        asl     a
        sta     tmp11
        jsr     LB88B
        jsr     hex_digit_to_nybble
        ora     tmp11
        rts

LB8C8:  lda     #8
add_a_to_zp1:
        clc
        adc     zp1
        sta     zp1
        bcc     LB8D3
        inc     zp1 + 1
LB8D3:  rts

LB8D4:  lda     #$FF
        sta     disable_f_keys
LB8D9:  lda     #$FF
        sta     BLNSW
        lda     BLNON
        beq     LB8EB ; rts
        lda     GDBLN
        ldy     PNTR
        sta     (PNT),y
        lda     #0
        sta     BLNON
LB8EB:  rts

LB8EC:  lda     #8
LB8EE:  sta     tmp14
        sec
        lda     zp1
        sbc     tmp14
        sta     zp1
        bcs     LB8FD
        dec     zp1 + 1
LB8FD:  rts

scroll_down:
        ldx     #0
        jsr     LE96C ; insert line at top of screen
        lda     #$94
        sta     LDTB1
        sta     LDTB1 + 1
        lda     #CSR_HOME
        jmp     BSOUT

LB90E:  lda     #16 ; number of bytes to scan backwards
        sta     tmp13
LB913:  sec
        lda     zp2
        sbc     tmp13
        sta     zp1
        lda     zp2 + 1
        sbc     #0
        sta     zp1 + 1 ; look this many bytes back
:       jsr     decode_mnemo
        lda     num_asm_bytes
        jsr     sadd_a_to_zp1
        jsr     check_end
        beq     :+
        bcs     :-
        dec     tmp13
        bne     LB913
:       rts

; ----------------------------------------------------------------
; assembler tables
; ----------------------------------------------------------------
addmode_table:
        .byte   $40,$02,$45,$03,$D0,$08,$40,$09
        .byte   $30,$22,$45,$33,$D0,$08,$40,$09
        .byte   $40,$02,$45,$33,$D0,$08,$40,$09
        .byte   $40,$02,$45,$B3,$D0,$08,$40,$09
        .byte   $00,$22,$44,$33,$D0,$8C,$44,$00
        .byte   $11,$22,$44,$33,$D0,$8C,$44,$9A
        .byte   $10,$22,$44,$33,$D0,$08,$40,$09
        .byte   $10,$22,$44,$33,$D0,$08,$40,$09

        .byte   $62,$13,$78,$A9

addmode_detail_table:
        .byte   %000000 << 2 | 0 ; implied
        .byte   %001000 << 2 | 1 ; immediate
        .byte   %100000 << 2 | 1 ; zero page
        .byte   %100000 << 2 | 2 ; absolute
        .byte   %000000 << 2 | 0 ; implied
        .byte   %000000 << 2 | 0 ; implied
        .byte   %010110 << 2 | 1 ; X indexed indirect
        .byte   %010011 << 2 | 1 ; indirect Y indexed
        .byte   %100100 << 2 | 1 ; zero page X indexed
        .byte   %100100 << 2 | 2 ; absolute X indexed
        .byte   %100001 << 2 | 2 ; absolute Y indexed
        .byte   %010010 << 2 | 2 ; absolute indirect
        .byte   %100001 << 2 | 1 ; zero page Y indexed
        .byte   %100111 << 2 | 1 ; special case: relative

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

.segment "monitor_c"

; ----------------------------------------------------------------

s_regs: .byte   CR, "   PC  IRQ  BK AC XR YR SP NV#BDIZC", CR, 0

; ----------------------------------------------------------------

command_index_d = command_name_d - command_names
command_index_f = command_name_f - command_names
command_index_h = command_name_h - command_names
command_index_c = command_name_c - command_names
command_index_l = command_name_l - command_names
command_index_s = command_name_s - command_names
command_index_i = command_name_i - command_names

command_names:
        .byte   "M" ; N.B.: code relies on "M" being the first entry of this table!
command_name_d:
        .byte   "D"
        .byte   ":"
        .byte   "A"
        .byte   "G"
        .byte   "X"
command_name_f:
        .byte   "F"
command_name_h:
        .byte   "H"
command_name_c:
        .byte   "C"
        .byte   "T"
        .byte   "R"
command_name_l:
        .byte   "L"
command_name_s:
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
command_name_i:
        .byte   "I"
        .byte   "'"
        .byte   ";"
        .byte   "B"

function_table:
        .word   cmd_mid-1
        .word   cmd_mid-1
        .word   cmd_colon-1
        .word   cmd_a-1
        .word   cmd_g-1
        .word   cmd_x-1
        .word   cmd_fhct-1
        .word   cmd_fhct-1
        .word   cmd_fhct-1
        .word   cmd_fhct-1
        .word   cmd_r-1
        .word   cmd_ls-1
        .word   cmd_ls-1
        .word   cmd_comma-1
        .word   cmd_o-1
        .word   cmd_at-1
        .word   cmd_dollar-1
        .word   cmd_hash-1
        .word   cmd_asterisk-1
        .word   cmd_p-1
        .word   cmd_e-1
        .word   cmd_leftbracket-1
        .word   cmd_rightbracket-1
        .word   cmd_mid-1
        .word   cmd_singlequote-1
        .word   cmd_semicolon-1
        .word   cmd_b-1

; ----------------------------------------------------------------

syn_err7:
        jmp     syntax_error

; ----------------------------------------------------------------
; "*R"/"*W" - read/write sector
; ----------------------------------------------------------------
cmd_asterisk:
        jsr     listen_command_channel
        jsr     UNLSTN
        jsr     BASIN
        cmp     #'W'
        beq     LBAA0
        cmp     #'R'
        bne     syn_err7
LBAA0:  sta     zp2 ; save 'R'/'W' mode
        jsr     basin_skip_spaces_if_more
        jsr     get_hex_byte2
        bcc     syn_err7
        sta     zp1
        jsr     basin_if_more
        jsr     get_hex_byte
        bcc     syn_err7
        sta     zp1 + 1
        jsr     basin_cmp_cr
        bne     LBAC1
        lda     #>$CF00 ; default address
        sta     zp2 + 1
        bne     LBACD
LBAC1:  jsr     get_hex_byte
        bcc     syn_err7
        sta     zp2 + 1
        jsr     basin_cmp_cr
        bne     syn_err7
LBACD:  jsr     LBB48
        jsr     swap_zp1_and_zp2
        lda     zp1
        cmp     #'W'
        beq     LBB25
        lda     #'1' ; U1: read
        jsr     read_write_block
        jsr     talk_cmd_channel
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
        jsr     close_2
        jmp     input_loop

LBB00:  jsr     IECIN
        cmp     #CR ; receive all bytes (XXX not necessary?)
        bne     LBB00
        jsr     UNTALK
        jsr     send_bp
        ldx     #2
        jsr     CHKIN
        ldy     #0
        sty     zp1
LBB16:  jsr     IECIN
        jsr     store_byte ; receive block
        iny
        bne     LBB16
        jsr     CLRCH
        jmp     LBB42 ; close 2 and print drive status

LBB25:  jsr     send_bp
        ldx     #2
        jsr     CKOUT
        ldy     #0
        sty     zp1
LBB31:  jsr     load_byte
        jsr     IECOUT ; send block
        iny
        bne     LBB31
        jsr     CLRCH
        lda     #'2' ; U2: write
        jsr     read_write_block
LBB42:  jsr     close_2
        jmp     print_drive_status

LBB48:  lda     #2
        tay
        ldx     FA
        jsr     SETLFS
        lda     #1
        ldx     #<s_hash
        ldy     #>s_hash
        jsr     SETNAM
        jmp     OPEN

close_2:
        lda     #2
        jmp     CLOSE

to_dec:
        ldx     #'0'
        sec
LBB64:  sbc     #10
        bcc     LBB6B
        inx
        bcs     LBB64
LBB6B:  adc     #'9' + 1
        rts

read_write_block:
        pha
        ldx     #0
LBB71:  lda     s_u1,x
        sta     BUF,x
        inx
        cpx     #s_u1_end - s_u1
        bne     LBB71
        pla
        sta     BUF + 1
        lda     zp2 ; track
        jsr     to_dec
        stx     BUF + s_u1_end - s_u1 + 0
        sta     BUF + s_u1_end - s_u1 + 1
        lda     #' '
        sta     BUF + s_u1_end - s_u1 + 2
        lda     zp2 + 1 ; sector
        jsr     to_dec
        stx     BUF + s_u1_end - s_u1 + 3
        sta     BUF + s_u1_end - s_u1 + 4
        jsr     listen_command_channel
        ldx     #0
LBBA0:  lda     BUF,x
        jsr     IECOUT
        inx
        cpx     #s_u1_end - s_u1 + 5
        bne     LBBA0
        jmp     UNLSTN

send_bp:
        jsr     listen_command_channel
        ldx     #0
LBBB3:  lda     s_bp,x
        jsr     IECOUT
        inx
        cpx     #s_bp_end - s_bp
        bne     LBBB3
        jmp     UNLSTN

s_u1:
        .byte   "U1:2 0 "
s_u1_end:
s_bp:
        .byte   "B-P 2 0"
s_bp_end:

s_hash:
        .byte   "#"

send_m_dash2:
        pha
        lda     #$6F
        jsr     init_and_listen
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

syn_err8:
        jmp     syntax_error

; ----------------------------------------------------------------
; "P" - set output to printer
; ----------------------------------------------------------------
cmd_p:
        lda     bank
        bmi     syn_err8 ; drive?
        ldx     #$FF
        lda     FA
        cmp     #4
        beq     LBC11 ; printer
        jsr     basin_cmp_cr
        beq     LBC16 ; no argument
        cmp     #','
        bne     syn_err8
        jsr     get_hex_byte
        tax
LBC11:  jsr     basin_cmp_cr
        bne     syn_err8
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
        jsr     CLOSE
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
        sbc     pow10lo2,x
        tay
        lda     zp1 + 1
        sbc     pow10hi2,x
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

pow10lo2:
        .byte <1, <10, <100, <1000, <10000
pow10hi2:
        .byte >1, >10, >100, >1000, >10000

init_and_listen:
        pha
        jsr     init_drive
        jsr     LISTEN
        pla
        jmp     SECOND

talk_cmd_channel:
        lda     #$6F
init_and_talk:
        pha
        jsr     init_drive
        jsr     TALK
        pla
        jmp     TKSA

cat_line_iec:
        jsr     IECIN
        jsr     LE716 ; KERNAL: output character to screen
        cmp     #CR
        bne     cat_line_iec
        jmp     UNTALK

print_hex_byte:
        jsr     byte_to_hex_ascii
        jsr     BSOUT
        tya
        jmp     BSOUT

; convert byte into hex ASCII in A/Y
byte_to_hex_ascii:
        pha
        and     #$0F
        jsr     LBCC8
        tay
        pla
        lsr     a
        lsr     a
        lsr     a
        lsr     a
LBCC8:  clc
        adc     #$F6
        bcc     LBCCF
        adc     #$06
LBCCF:  adc     #$3A
        rts

directory:
        lda     #$60
        sta     SA
        jsr     init_and_talk
        jsr     IECIN
        jsr     IECIN ; skip load address
LBCDF:  jsr     IECIN
        jsr     IECIN ; skip link word
        jsr     IECIN
        tax
        jsr     IECIN ; line number (=blocks)
        ldy     ST
        bne     LBD2F ; error
        jsr     LBC4C ; print A/X decimal
        lda     #' '
        jsr     LE716 ; KERNAL: output character to screen
        ldx     #$18
LBCFA:  jsr     IECIN
LBCFD:  ldy     ST
        bne     LBD2F ; error
        cmp     #CR
        beq     LBD09 ; convert $0D to $1F
        cmp     #$8D
        bne     LBD0B ; also convert $8D to $1F
LBD09:  lda     #$1F ; ???BLUE
LBD0B:  jsr     LE716 ; KERNAL: output character to screen
        inc     INSRT
        jsr     GETIN
        cmp     #KEY_STOP
        beq     LBD2F
        cmp     #' '
        bne     LBD20
LBD1B:  jsr     GETIN
        beq     LBD1B ; space pauses until the next key press
LBD20:  dex
        bpl     LBCFA
        jsr     IECIN
        bne     LBCFD
        lda     #CR
        jsr     LE716 ; KERNAL: output character to screen
LBD2D:  bne     LBCDF ; next line
LBD2F:  jmp     LF646 ; CLOSE

init_drive:
        lda     #0
        sta     ST ; clear status
        lda     #8
        cmp     FA ; drive 8 and above ok
        bcc     LBD3F
LBD3C:  sta     FA ; otherwise set drive 8
LBD3E:  rts

LBD3F:  lda     #9
        cmp     FA
        bcs     LBD3E
        lda     #8
LBD47:
        bne     LBD3C
        lda     zp3 ; XXX ???
LBD4B:  ldy     ST
        bne     LBD7D
        cmp     #CR
        beq     LBD57
        cmp     #$8D
        bne     LBD59
LBD57:  lda     #$1F
LBD59:  jsr     LE716 ; KERNAL: output character to screen
        inc     INSRT
        jsr     GETIN
        cmp     #KEY_STOP
        beq     LBD7D
        cmp     #$20
        bne     LBD6E
LBD69:  jsr     GETIN
        beq     LBD69
LBD6E:  dex
        bpl     LBD47 + 1 ; ??? XXX
        jsr     IECIN
        bne     LBD4B
        lda     #CR
        jsr     LE716 ; KERNAL: output character to screen
        bne     LBD2D
LBD7D:  jmp     LF646 ; CLOSE

        lda     #0
        sta     ST
        lda     #8
        cmp     FA
        bcc     LBD8D
LBD8A:  sta     FA
LBD8C:  rts

LBD8D:  lda     #9
        cmp     FA
        bcs     LBD8C
        lda     #8
        bne     LBD8A ; always
