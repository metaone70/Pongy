; 10 SYS (2080)
*=$0801
        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $38, $30, $29, $00, $00, $00

*=$0820
        JSR     $E544               ; CLEAR SCREEN
        LDA     #$30                ; WRITE 0 ON SCREEN
        STA     $043C
        LDA     #$39                ; WRITE 9 ON SCREEN
        STA     $043B               ; SO WE NOW HAVE 90

        LDA     #$7F
        STA     $DD0D           
        LDA     #<NMI_COUNTER
        STA     $318
        LDA     #>NMI_COUNTER
        STA     $319
        LDA     #$8A
        STA     $DD04
        LDA     #$F0
        STA     $DD05
        LDA     #$10
        STA     $DD06
        LDA     #$0
        STA     $DD07
        LDA     #%00010001
        STA     $DD0E           ; START TIMER A
        LDA     #%01010001
        STA     $DD0F           ; START TIMER B
        LDA     #%10000010
        STA     $DD0D           ; ENABLE NMI
        RTS

NMI_COUNTER       
        PHA
        TXA
        PHA
        TYA
        PHA

        LDA     $DD0D
        BPL     DONE            ; NMI not from CIA2
        JSR     WRITE_COUNTER   ; GO AND WRITE THE COUNTER ON SCREEN

DONE    PLA
        TAY
        PLA
        TAX
        PLA

        RTI

WRITE_COUNTER

        DEC     $043C               ; DECREMENT RIGHT DIGIT
        LDA     $043C
        CMP     #$2F                ; DID WE REACH BELOW ZERO?
        BNE     EXIT                ; NO, CONTINUE PROCESS
        LDA     #$39                ; YES, ZERO THE RIGHT DIGIT
        STA     $043C               ; AND DECREMENT LEFT DIGIT
        DEC     $043B
        LDA     $043B               ; DID WE REACH BELOW ZERO?
        ;CMP    #$2F
        CMP     #$37
        BEQ     FINISH_AND_EXIT
EXIT    RTS


FINISH_AND_EXIT
        LDX     #$00
GOT     LDA     FINISH,X            ; WRITE FINISHED ON SCREEN
        STA     $05C7,X
        INX
        CPX     #$09
        BNE     GOT

        LDA     #$00
        STA     $DD0E
        STA     $DD0F
        RTS


FINISH     text    'finished!'
