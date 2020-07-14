start:
    ldi r24, 2
    sts 0x04, r24  ; DDRB = 1
    sts 0x05, r24  ; PORTB = 1
loop:
    rjmp loop
