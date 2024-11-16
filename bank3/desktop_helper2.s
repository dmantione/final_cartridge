;*****************************************************************************
;  Final Cartridge III reconstructed source code
;
;  After the user has reordered the entries in a directory using the desktop,
;  this code writes back the directory to disk in the desired order.
;*****************************************************************************

      .setcpu "6502x"

.include "../core/kernal.i"
.include "../core/fc3ioreg.i"
.include "persistent.i"

.import load_ae_rom_hidden
.import __diredit_cmds_LOAD__,__diredit_cmds_RUN__,__diredit_cmds_SIZE__

.segment "desktop_helper_2"

.global write_directory_back_to_disk
write_directory_back_to_disk:
      jsr  write_directory_back_to_disk2
      lda  #fcio_nmi_line | fcio_bank_0
      jmp  _jmp_bank

;
; The desktop has created a list of directory entries to indicate
; the desired order of the files on disk. Each directory entry consists
; of two ASCIIZ strinfs. The first is the number of blocks in decimal,
; the second the file name. All strings are directly concatenated after
; each other.
;

jmp_errexit:
      jmp errexit

write_directory_back_to_disk2:
      ; Check for compatible drive
      jsr read_drive_identification
      lda #'7'  ; Status code '73'
      cmp $0202
      bne jmp_errexit
      lda #'5'  ; From 1541
      cmp $0213
      bne jmp_errexit
      lda #'4'  ; From 154x
      beq :+
      lda #'7'  ; From 157x
      bne jmp_errexit
      ; Fill $A000..$BFFF with #$00
:     lda  #>$A000
      sta  $AD
      ldx  #$20 ; Fill $2000 bytes
      lda  #$00
      tay
      sta  $AC
fill_loop:
:     sta  ($AC),y
      iny
      bne  :-
      inc  $AD
      dex
      bne  :-

      ; Copy directory editing commands to low RAM
      ldx  #<(__diredit_cmds_SIZE__-1)
:     lda  __diredit_cmds_LOAD__,x
      sta  $0202,x
      dex
      bpl  :-

      jsr  open_hash_on_chan_2
      lda  #$00
      sta  $AC
      sta  $AE
      sta  $C1
      lda  #>$A000
      sta  $AD
:     jsr  send_read_block
      jsr  send_seek_0
      jsr  read_sector_from_chan_2
      inc  $AD                          ; Increasepointer for next sector
      cpx  #$00                         ; End of directory?
      beq  directory_read_complete
      cpx  #18                          ; Not on track 18?
      bne  close                        ; Then blast off.
      cmp  #19                          ; Sector >= 19?
      bcs  close                        ; Then blast off.
      jsr  nibble2ascii
      ; Store sector number in command
      stx  read_block+10
      sta  read_block+11
      lda  $AD
      cmp  #$C0                         ; Do not read beyond $C000
      bcc  :-
close:
      jsr  close_chn2
errexit:
      lda  #$80                         ; DEVICE NOT PRESENT ERROR
      sta  $90                          ; Statusbyte ST of I/O KERNAL
      rts

directory_read_complete:
      ;
      ; The old directory is stored at $A000. Now build the new directory
      ; at $B000.
      ;
      lda  $0200
      sta  $C3
      lda  $0201
      sta  $C4

      ; We search the entire directory for every file name, so start at $A000
      lda  #>$A000
      sta  $C2
      ldy  #$00
      sty  $C1
next_file:
      lda  $C1
      sta  $AE
      lda  $C2
      sta  $AF
      ; Y=0
      ldy #0
      lda  ($C3),y
      tax
      bne  :+                           ; All files processed?
      jmp  write_dir_to_disk            ; Then write dir to disk.
:     jsr  inc_c3c4_beyond_z            ; Skip block count
      txa
      bmi  insert_line                  ; Do we need to insert a line?
;      dec  $0200
process_dir_entry:
      ldy  #$02                         ; Get file type
      jsr  _load_ae_rom_hidden
      bpl  not_yet_found
      ; Adjust pointer to file name
      lda  #$05
      ora  $AE
      sta  $AE
      ; Compare file name
      ldy  #$00
:     lda  ($C3),y
      beq  :+
      jsr  _load_ae_rom_hidden
      cmp  ($C3),y
      bne  not_yet_found                ; File name not equal
      iny
      cpy  #$11                         ; If desktop passes too long file name (should not occur)
      bne  :-
      beq  no_space_left
:     cpy  #$10
      beq  :+
      jsr  _load_ae_rom_hidden
      cmp  #$A0                         ; First character beyond file name must be white space ($A0)
      bne  not_yet_found
:     ; We found the file name in the directory
      iny
      tya
      jsr add_to_c3c4
      lda  $AE
      and  #$E0
      sta  $AE
      cmp  $C1
      bne  @s
      lda  $AF
      cmp  $C2
      beq  @ns
@s:
      jsr swap_entries
@ns:
inc_dest_ptr:
      ; Increase destination pointer
      lda  #$20
      clc
      adc  $C1
      sta  $C1
      bcc  next_file
      inc  $C2
      lda  $C2
      cmp  #>$C000                      ; Destination buffer full?
      bcc  next_file
no_space_left:
      jmp  close

not_yet_found:
      jsr  next_dir_entry
      bcs  no_space_left
      jmp  process_dir_entry


insert_line:
      ldy  #$FF
      jsr  inc_c3c4_beyond_z
      ; Find an empty directory entry
      ldy  #$02
:     jsr  _load_ae_rom_hidden
      bpl  :+
      jsr  next_dir_entry
      bcc  :-
      ; No empty entry found, increase directory size by one sector
      lda  $AD
      cmp  #>$C000
      bcs  no_space_left
      inc  $AD
      bcc  :- ; Always
      ; Move the entry at ($AE) to the empty entry
:     jsr  swap_entries
      ; Write the line in to the destination entry
      ldy  #$1F
:     lda  dirline,y
      sta  ($C1),y
      dey
      bpl  :-
      bmi  inc_dest_ptr ; Always

next_dir_entry:
      lda  $AE
      and  #$E0
      clc
      adc  #$20
      sta  $AE
      bcc  :+
      inc  $AF
:     lda  $AF
      cmp  $AD
      rts

swap_entries:
      ldy #31
:     jsr  _load_ae_rom_hidden
      pha
      jsr  _load_c1_rom_hidden
      sta  ($AE),y
      pla
      sta  ($C1),y
      dey
      bpl :-
      rts

inc_c3c4_beyond_z:
      ; Search for a 0 byte
:     iny
      lda  ($C3),y
      bne  :-
      iny
      tya
add_to_c3c4:
      ; Add the number of bytes to $C3/$C4
      clc
      adc  $C3
      sta  $C3
      bcc  :+
      inc  $C4
:     rts

write_dir_to_disk:
      ; Start at sector 1
      lda  #'0'
      sta  write_block+10
      lda  #'1'
      sta  write_block+11
      lda  $C1
      bne  :+
      dec  $C2                          ; Prevent writing an empty sector
:     lda  #<$A000
      sta  $AE
      lda  #>$A000
      sta  $AF
      lda  #$02                         ; Sector in directory track
      sta  $AC
next_sector:
      ldy  #$00
      lda  $AF
      cmp  $C2
      bcs  :+
      lda  #18                          ; Directory track
      sta  ($AE),y
      iny
      lda  $AC
      sta  ($AE),y
      bne  not_last_sector
:     tya
      ; Last sector, Y=0
.global store_a_ff_to_ae
store_a_ff_to_ae:
      sta  ($AE),y
      lda  #$FF
      iny
      sta  ($AE),y
.global not_last_sector
not_last_sector:
      lda  $AC
      sec
      sbc  #$01
      jsr  nibble2ascii
      ; Store sector number
      stx  write_block+10
      sta  write_block+11
      jsr  send_seek_0
      jsr  send_256byte_to_channel_2
      jsr  send_write_block
      lda  $AF
      cmp  $C2
      bcs  :+
      inc  $AF
      inc  $AC
      jmp  next_sector

      ; Now do the BAM.
      ; This is a simplistic algorithm: All sectors in track 18 are initially
      ; assumed used. We have the number of sectors used in $AC, so a 0 is
      ; shifted in $AC times.
:     lda  #19         ; Track 18 has 19 sectors
      sec
      sbc  $AC
      sta  $C1
      lda  #$FF
      sta  $C2
      sta  $C3
      sta  $C4
:     clc
      rol  $C2
      rol  $C3
      rol  $C4
      dec  $AC
      bne  :-
      lda  $C4
      and  #$07
      sta  $C4
      ; Update the BAM. Update commands for sector 0
      lda  #'0'
      sta  read_block+10
      sta  read_block+11
      sta  write_block+10
      sta  write_block+11
      jsr  send_read_block
      jsr  send_seek_72    ; BAM for track 18 at offset 72
      lda  #$62
      jsr  listen_second
      ldx  #$00
:     lda  $C1,x
      jsr  IECOUT
      inx
      cpx  #4
      bne  :-
      jsr  UNLSTN
      jsr  send_write_block

      ; Send an "I" to command channel to make the drive reread the directory.
      lda  #$6F
      jsr  listen_second
      lda  #'I'
      jsr  IECOUT
      jsr  UNLSTN
      jmp  close_chn2

send_256byte_to_channel_2:
      lda  #$62
      jsr  listen_second
      ldy  #$00
:     jsr  _load_ae_rom_hidden
      jsr  IECOUT
      iny
      bne  :-
      jmp  UNLSTN

;
; Reads a sector from channel 2.
;
; The 254 bytes payload of a sector are stored in ($AC)+2 onwards.
; Pointer $AC/$AD is not increased
;
; Returns:
;
; A - Link to next sector
; X - Link to next track
;
read_sector_from_chan_2:
      lda  #$62
      jsr  talk_second
      ldy  #$02
      jsr  IECIN
      tax
      jsr  IECIN
      pha
:     jsr  IECIN
      sta  ($AC),y
      iny
      bne  :-
      jsr  UNTALK
      pla
      rts

open_hash_on_chan_2:
      lda  #$00
      sta  $90
      lda  #$F2
      jsr  listen_second
      lda  $90
      bmi  except_exit
      lda  #'#'
      jsr  IECOUT
      jmp  UNLSTN

close_chn2:
      lda  #$E2
      jsr  listen_second
      jmp  UNLSTN

listen_second:
      pha
      lda  $BA                          ; Current device number
      jsr  LISTEN
      pla
      jmp  SECOND

talk_second:
      pha
      lda  $BA                          ; Current device number
      jsr  TALK
      pla
      jmp  TKSA

send_read_block:
      ldx  #<(read_block - __diredit_cmds_RUN__)
      .byte $2c
send_write_block:
      ldx  #<(write_block - __diredit_cmds_RUN__)
      .byte $2c
send_seek_0:
      ldx  #<(seek_0 - __diredit_cmds_RUN__)
      .byte $2c
send_seek_72:
      ldx  #<(seek_72 - __diredit_cmds_RUN__)
      lda  #$6F                         ; Listen channel 15
      jsr  listen_second
:     lda  __diredit_cmds_RUN__,x
      beq  :+
      jsr  IECOUT
      inx
      bne  :-
:     jsr  UNLSTN
      ; Check for error omn cmd channel 15
      jsr  read_drive_status
      lda  $0202
      cmp  #$30
      beq  _rts2
except_exit:
      ; Error condition. Pull return address and abort directory write back.
      pla
      pla
      jmp  close

read_drive_identification:
      lda #$6F
      jsr listen_second
      lda #'U'
      jsr IECOUT
      lda #'I'
      jsr IECOUT
      jsr UNLSTN
      ; fall through

read_drive_status:
      lda #$6F
      jsr talk_second
      ldy #0
:     jsr IECIN
      cmp #$0D
      sta $0202,y
      beq :+
      iny
      cpy #$40 ; Avoid buffer overflow
      bne :-
:     jmp UNTALK


;
; Convert a nibble (actually a number 0..19) to ASCII with fixed with.
; 
; IN:   A  - Nibble
;
; OUT:  A - Least significant digit
;       X - Most sigificant digit
;
.proc nibble2ascii
      ldx  #'0'
      cmp  #$0A
      bcc  :+
      inx
      sbc  #$0A
:     ora  #'0'
r:    rts
.endproc

_rts2 = nibble2ascii::r

dirline:
      .byte $00, $00, $80, $12, $00, '-', '-', '-' 
      .byte '-', '-', '-', '-', '-', '-', '-', '-' 
      .byte '-', '-', '-', '-', '-', $00, $00, $00 
      .byte $00, $00, $00, $00, $00, $00, $00, $00 

.segment "diredit_cmds"

read_block:     .asciiz "U1:2 0 18 01"            ; Read block on channel 2 from drive 0, track 18 sector 1
write_block:    .asciiz "U2:2 0 18 01"            ; Write block on channel 2 to drive 0, track 18 sector 1
seek_0:         .asciiz "B-P 2 0"                 ; Seek channel 2 to position 0
seek_72:        .asciiz "B-P 2 72"                ; Seek channel 2 to position 72

