.ifdef use_ill
.define skip_1b_instr .byte $80
.define skip_2b_instr .byte $0C
.else
.define skip_1b_instr .byte $24
.define skip_2b_instr .byte $2C
.endif
