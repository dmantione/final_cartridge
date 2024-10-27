; ----------------------------------------------------------------
; Vectors
; ----------------------------------------------------------------
; This is put right after the cartridge's "cbm80" header and
; contains jump table, which is mostly used from other banks.

.include "../core/kernal.i"
.include "persistent.i"

; from init
.import entry
.import go_basic
.import init_load_and_basic_vectors
.import init_vectors_goto_psettings

; from format
.import fast_format
.import init_read_disk_name
.import init_write_bam

; from editor
.import print_screen

; from desktop_helper
.import perform_operation_for_desktop
.import monitor

.segment "vectors"

.assert * = $8009, error, "vectors must be at $8009!"

.global jentry
jentry:
        jmp     entry                         ; $8009

; this vector is called from other banks
        jmp     perform_operation_for_desktop ; $800c

.global jfast_format
jfast_format: ; monitor calls this
        jmp     fast_format                   ; $800f

; these vectors are called from other banks
        jmp     init_read_disk_name           ; $8012
        jmp     init_write_bam                ; $8015
        jmp     init_vectors_goto_psettings   ; $8018
        jmp     go_basic                      ; $801b
        jmp     print_screen                  ; $801e
        jmp     init_load_and_basic_vectors   ; $8021
        jmp     monitor                       ; $8024
