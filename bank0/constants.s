
.segment "constants"

.global pow10lo
pow10lo:
        .byte   <1,<10,<100,<1000,<10000
.global pow10hi
pow10hi:
        .byte   >1,>10,>100,>1000,>10000

.global pow2
pow2:  .byte   $80,$40,$20,$10,$08,$04,$02,$01
