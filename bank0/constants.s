
.segment "constants"

.global pow10lo
pow10lo:
        .byte   <1,<10,<100,<1000,<10000
.global pow10hi
pow10hi:
        .byte   >1,>10,>100,>1000,>10000
