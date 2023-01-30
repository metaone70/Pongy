; 10 SYS (2080)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $38, $30, $29, $00, $00, $00

*=$0820

        JSR $E544               ; CLEAR SCREEN
        LDA #$30                ; WRITE 0 ON SCREEN
        STA $043C
        LDA #$39                ; WRITE 9 ON SCREEN
        STA $043B               ; SO WE NOW HAVE 90

        SEI                     ; TURN OFF INTERRUPTS
        LDA #<TIMER             ; INTRODUCE INTERRUPT HANDLERS
        STA $0314               ; TO TIMER ROUTINE
        LDA #>TIMER
        STA $0315
        ASL $D019
        CLI
        RTS

TIMER
        ASL $D019
T2      JSR COUNT_DOWN
        JMP $EA31               ; WE REACH HERE AFTER THE COUNTDOWN IS FINISHED !!!


COUNT_DOWN                      ; THIS PART COUNTDOWNS FROM 90 TO 79
                                
T1      JSR DELAY               ; INSERT THE DELAY TO MIMIC SECONDS
        DEC $043C               ; DECREMENT RIGHT DIGIT
        LDA $043C
        CMP #$2F                ; DID WE REACH BELOW ZERO?
        BNE T2                  ; NO, CONTINUE PROCESS
        LDA #$39                ; YES, WRITE ZERO ON THE RIGHT DIGIT
        STA $043C               ; AND DECREMENT LEFT DIGIT
        DEC $043B
        LDA $043B               
        ;CMP #$2F
        CMP #$37                ; DID WE REACH THE DESIRED NUMBER? -> 7 FOR NOW 
        BNE T2                  ; NO, REPEAT AND RINSE






        LDX #$00                ; YES, WE COMPLETED THE COUNTDOWN
W3      LDA FINISH_TEXT,X       ; SO WRITE 'FINISHED!'
        STA $0400,X             ; ON SCREEN
        INX
        CPX #$09
        BNE W3 

;        LDA #$00                ; FINISHED WRITING ON SCREEN
;        STA $D01A               ; SO EXIT FROM INTERRUPTS
;        LDA #$31
;        STA $0314               ; I DO NOT WHY I NEED THESE
;        LDA #$EA                ; TO GET THE CURSOR BACK 
;        STA $0315               ; AND IF I DON'T WRITE THIS BLOCK
;        LDA #$81                ; THE COUNTDOWN CONTINUES
;        STA $DC0D
        RTS
      
DELAY                           ; THIS PART DELAYS THE COUNTDOWN
         LDA #$00               ; TO MIMIC SECONDS
         STA $1000
         STA $1001
@WAIT11  INC $1001
@WAIT10  INC $1000
         LDA $1000
         BNE @WAIT10
         LDA $1001
         CMP #$FF
         BNE @WAIT11
         RTS
     

FINISH_TEXT     text    'finished!'

       