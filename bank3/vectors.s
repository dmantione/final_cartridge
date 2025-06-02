; ----------------------------------------------------------------
; Vectors
; ----------------------------------------------------------------
; This is put right after the cartridge's "cbm80" header and
; contains jump table, which is mostly used from other banks.

.import write_directory_back_to_disk
.import fast_format
.import restart_freezer

.segment "vectors"

jmp write_directory_back_to_disk
jmp fast_format
jmp restart_freezer

