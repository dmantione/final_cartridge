;*****************************************************************************
;  Final Cartridge III reconstructed source code
;
;  After the user has reordered the entries in a directory using the desktop,
;  this code writes back the directory to disk in the desired order.
;*****************************************************************************

      .setcpu "6502x"

.include "../core/kernal.i"
.include "../core/fc3ioreg.i"
.include "../core/macros.i"
.include "persistent.i"

.import load_ae_rom_hidden
.import __diredit_cmds_LOAD__,__diredit_cmds_RUN__,__diredit_cmds_SIZE__

; locations of these variables are chosen to be unlikely used by the desktop
dir_track       := $AB  ; (normally used by KERNAL for tape)
track_sectors   := $A6  ; (normally used by KERNAL for tape)
dir_sector      := $A7  ; (normally used by KERNAL for RS232)

; original FC3 firmware uses these locations, so safe to use:
ptr1            := $AC  ; (normally used by KERNAL for SAVE)
ptr2            := $AE  ; (normally used by KERNAL for LOAD)
ptr3            := $C1  ; (normally used by KERNAL for SAVE)

.segment "desktop_helper_2"

.global write_directory_back_to_disk
write_directory_back_to_disk:
      jsr  write_directory_back_to_disk2
      pla
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
      lda #18
      sta dir_track
      lda #19
      sta track_sectors
      ; Check for compatible drive
      jsr read_drive_identification
      lda $02C0
      cmp #'7' ; Status code '73'
      bne jmp_errexit
      lda #'5'  ; From 1541
      cmp $02D1
      bne @test_1581
      lda $02D2
      cmp #'4'  ; From 154x
      beq @d64
      cmp #'7'  ; From 157x
      beq @d64
      bne jmp_errexit
@test_1581:
      cmp $02DA
      bne jmp_errexit
      lda #'8'
      cmp $02DB
      bne jmp_errexit
      ; C set if equal
      skip_1b_instr
      ; Fill $A000..$BFFF with #$00
@d64: clc

      ; Copy directory editing commands to low RAM
      ldx  #<(__diredit_cmds_SIZE__-1)
:     lda  __diredit_cmds_LOAD__,x
      sta  $0202,x
      dex
      bpl  :-

      bcc  :+
      ; D81 directory on track 40!
      lda  #'4'
      sta  read_block+7
      sta  write_block+7
      lda  #'0'
      sta  read_block+8
      sta  write_block+8
      lda  #40
      sta  dir_track
      sta  track_sectors

:     lda  #>$A000
      sta  ptr1+1
      ldx  #$20 ; Fill $2000 bytes
      lda  #$00
      tay
      sta  ptr1
      sta  ptr2
      sta  ptr3
fill_loop:
:     sta  (ptr1),y
      iny
      bne  :-
      inc  ptr1+1
      dex
      bne  :-

      jsr  open_hash_on_chan_2
      lda  #>$A000
      sta  ptr1+1
      ; Start by reading the BAM sector and skipping its contents
      jsr  send_read_block
      jsr  send_seek_0
      jsr  read_sector_from_chan_2
      sta  dir_sector
      jmp  @1
:     ; Read a directory sector
      jsr  send_read_block
      jsr  send_seek_0
      jsr  read_sector_from_chan_2
      inc  ptr1+1                       ; Increasepointer for next sector
@1:   cpx  #$00                         ; End of directory?
      beq  directory_read_complete
      cpx  dir_track                    ; Not on track 18?
      bne  close                        ; Then blast off.
      cmp  track_sectors                ; Sector >= 19?
      bcs  close                        ; Then blast off.
      jsr  to_dec
      ; Store sector number in command
      stx  read_block+10
      sta  read_block+11
      lda  ptr1+1
      cmp  #$C0                         ; Do not read beyond $C000
      bcc  :-
close:
      jsr  close_chn2
errexit:
      lda  #$80                         ; DEVICE NOT PRESENT ERROR
      sta  ST                           ; Statusbyte ST of I/O KERNAL
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
      sta  ptr3+1
      ldy  #$00
      sty  ptr3
next_file:
      lda  ptr3
      sta  ptr2
      lda  ptr3+1
      sta  ptr2+1
      ; Y=0
      ldy  #0
      lda  ($C3),y
      tax
      bne  :+                           ; All files processed?
      jmp  write_dir_to_disk            ; Then write dir to disk.
:     jsr  inc_c3c4_beyond_z            ; Skip block count
      txa
      bmi  insert_line                  ; Do we need to insert a line?
process_dir_entry:
      ldy  #$02                         ; Get file type
      jsr  _load_ptr2_rom_hidden
      bpl  not_yet_found
      ; Adjust pointer to file name
      lda  #$05
      ora  ptr2
      sta  ptr2
      ; Compare file name
      ldy  #$00
:     lda  ($C3),y
      beq  :+
      jsr  _load_ptr2_rom_hidden
      cmp  ($C3),y
      bne  not_yet_found                ; File name not equal
      iny
      cpy  #$11                         ; If desktop passes too long file name (should not occur)
      bne  :-
      beq  no_space_left
:     cpy  #$10
      beq  :+
      jsr  _load_ptr2_rom_hidden
      cmp  #$A0                         ; First character beyond file name must be white space ($A0)
      bne  not_yet_found
:     ; We found the file name in the directory
      iny
      tya
      jsr  add_to_c3c4
      lda  ptr2
      and  #$E0
      sta  ptr2
      cmp  ptr3
      bne  @s
      lda  ptr2+1
      cmp  ptr3+1
      beq  @ns
@s:   ; Swap both entries
      jsr swap_entries
@ns:
inc_dest_ptr:
      ; Increase destination pointer
      lda  #$20
      clc
      adc  ptr3
      sta  ptr3
      bcc  next_file
      inc  ptr3+1
      lda  ptr3+1
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
:     jsr  _load_ptr2_rom_hidden
      bpl  :+
      jsr  next_dir_entry
      bcc  :-
      ; No empty entry found, increase directory size by one sector
      lda  ptr1+1
      cmp  #>$C000
      bcs  no_space_left
      inc  ptr1+1
      bcc  :- ; Always
      ; Move the entry at (ptr2) to the empty entry
:     jsr  swap_entries
      ; Write the line in to the destination entry
      ldy  #$1F
:     lda  dirline,y
      sta  (ptr3),y
      dey
      bpl  :-
      bmi  inc_dest_ptr ; Always

next_dir_entry:
      lda  ptr2
      and  #$E0
      clc
      adc  #$20
      sta  ptr2
      bcc  :+
      inc  ptr2+1
:     lda  ptr2+1
      cmp  ptr1+1
      rts

swap_entries:
      ldy #31
:     jsr _swap_ptr2_ptr3_rom_hidden
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
      lda  dir_sector                   ; First sector of directory
      jsr  to_dec
      stx  write_block+10
      sta  write_block+11
      lda  ptr3
      bne  :+
      dec  ptr3+1                       ; Prevent writing an empty sector
:     lda  #<$A000
      sta  ptr2
      lda  #>$A000
      sta  ptr2+1
next_sector:
      ldy  #$00
      lda  ptr2+1
      cmp  ptr3+1
      bcs  :+
      lda  dir_track
      sta  (ptr2),y
      iny
      ldx  dir_sector
      inx
      txa
      sta  (ptr2),y
      bne  not_last_sector
:     tya
      ; Last sector, Y=0
      sta  (ptr2),y
      lda  #$FF
      iny
      sta  (ptr2),y
not_last_sector:
      lda  dir_sector
      jsr to_dec
      ; Store sector number
      stx  write_block+10
      sta  write_block+11
      jsr  send_seek_0
      jsr  send_256byte_to_channel_2
      jsr  send_write_block
      inc  dir_sector
      lda  ptr2+1
      cmp  ptr3+1
      bcs  :+
      inc  ptr2+1
      bcc  next_sector ; always

      ; Now do the BAM.
      ; This is a simplistic algorithm: All sectors in track 18 are initially
      ; assumed used. We have the number of sectors used in ptr1, so a 0 is
      ; shifted in dir_sector times.
:     lda  track_sectors
      sec
      sbc  dir_sector
      sta  $02B0
      ldx  #4
      lda  #$FF
:     sta  $02B1,x
      dex
      bpl  :-
:     clc
      rol  $02B1
      rol  $02B2
      rol  $02B3
      rol  $02B4
;      rol  $02C5  More than 32 sectors not possible: Buffer is 8KB
      dec  dir_sector
      bne  :-
      lda  $C4
      and  #$07
      sta  $C4
      ; Update the BAM. Update commands for BAM sector
      ldx  #'0'
      stx  read_block+10
      stx  write_block+10
      stx  read_block+11
      stx  write_block+11
      lda  #4
      sta  ptr3
      ldx  #<(seek_72 - __diredit_cmds_RUN__)
      lda  #40
      cmp  dir_track
      bne  @no_d81
      inc  write_block+11
      inc  read_block+11
      lda  #6
      sta  ptr3
      ldx  #<(seek_250 - __diredit_cmds_RUN__)
@no_d81:
      stx  ptr3+1
      jsr  send_read_block
      ldx  ptr3+1
      jsr  send_seek
      lda  #$62
      jsr  listen_second
      ldx  #$00
:     lda  $02B0,x
      jsr  IECOUT
      inx
      cpx  ptr3
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
:     jsr  _load_ptr2_rom_hidden
      jsr  IECOUT
      iny
      bne  :-
      jmp  UNLSTN

;
; Reads a sector from channel 2.
;
; The 254 bytes payload of a sector are stored in (ptr1)+2 onwards.
; Pointer ptr1 is not increased
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
      sta  (ptr1),y
      iny
      bne  :-
      jsr  UNTALK
      pla
      rts

open_hash_on_chan_2:
      lda  #$F2
      jsr  listen_second
      lda  ST
      bmi  except_exit
      lda  #'#'
      jsr  IECOUT
      jmp  UNLSTN

close_chn2:
      lda  #$E2
      jsr  listen_second
      jmp  UNLSTN

.global listen_second
listen_second:
      pha
      lda  #$00
      sta  ST
      lda  $BA                          ; Current device number
      jsr  LISTEN
      pla
      jmp  SECOND

talk_second:
      pha
      lda  #$00
      sta  ST
      lda  $BA                          ; Current device number
      jsr  TALK
      pla
      jmp  TKSA

send_read_block:
      ldx  #<(read_block - __diredit_cmds_RUN__)
      skip_2b_instr
send_write_block:
      ldx  #<(write_block - __diredit_cmds_RUN__)
      skip_2b_instr
send_seek_0:
      ldx  #<(seek_0 - __diredit_cmds_RUN__)
send_seek:
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
      lda  $02C0
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
      sta $02C0,y
      lda ST
      bne :+
      iny
      cpy #$40 ; Avoid buffer overflow
      bne :-
:     jmp UNTALK


;
; Convert a number 0..99 to ASCII with fixed with.
; 
; IN:   A  - Nibble
;
; OUT:  A - Least significant digit
;       X - Most sigificant digit
;
.proc to_dec
        ldx     #'0' - 1
        sec
:       inx
        sbc     #10
        bcs     :-
        adc     #'9' + 1
r:      rts
.endproc

_rts2 = to_dec::r

dirline:
      .byte $00, $00, $80, $12, $00, '-', '-', '-' 
      .byte '-', '-', '-', '-', '-', '-', '-', '-' 
      .byte '-', '-', '-', '-', '-', $00, $00, $00 
      .byte $00, $00, $00, $00, $00, $00, $00, $00 

.segment "diredit_cmds"

read_block:     .asciiz "U1:2 0 18 00"            ; Read block on channel 2 from drive 0, track 18 sector 1
write_block:    .asciiz "U2:2 0 18 01"            ; Write block on channel 2 to drive 0, track 18 sector 1
seek_0:         .asciiz "B-P 2 0"                 ; Seek channel 2 to position 0
seek_72:        .asciiz "B-P 2 72"                ; Seek channel 2 to position 72
seek_250:       .asciiz "B-P 2 250"               ; Seek channel 2 to position 250¨

