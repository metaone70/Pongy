; 10 SYS (2080)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $38, $30, $29, $00, $00, $00


*=$0820

START

                JSR $E544
COUNTER
                SEI
                LDA #$39
                STA $0436
                LDA #$30
                STA $0437

                lda     #$7f
                sta     $dd0d           ; disable all CIA2 NMIs

                lda     #<tickerisr
                sta     $318
                lda     #>tickerisr
                sta     $319

                ; 985248 (=$F08A0) cycles per second ->
                ; load timer A with $F08A and timer B with $10

                lda     #$8a
                sta     $dd04
                lda     #$f0
                sta     $dd05
                lda     #$10
                sta     $dd06
                lda     #$0
                sta     $dd07

                lda     #%00010001
                sta     $dd0e           ; start timer A counting cycles
                lda     #$51
                sta     $dd0f           ; start timer B counting t.A underflows

                lda     #$82
                sta     $dd0d           ; enable NMI on t.B underflows
                rts

tickerisr       pha
                lda     $dd0d
                bpl     done            ; NMI not from CIA2
                JSR     WRITE           ; do something visible
done            pla
                RTI


WRITE
                DEC $0437
                LDA $0437
                CMP #$2F
                BNE EXIT
                LDA #$3A
                STA $0437
                DEC $0436
                LDA $0436
                CMP #$37
                BNE WRITE
                JMP WRITE2

EXIT            RTS



;NEED TO TERMINATE THE INTERRUPTS
WRITE2
                LDX #$00
W3              LDA FINISH_TEXT,X
                STA $0400,X
                INX
                CPX #$09
                BNE W3 

  ;SEI  ;; switch off interrupt
  ;lda #$35 ;; all RAM except D000-Dfff 
  ;sta $01  ;; write to $FFFA/$FFFB now possible
                     
  lda #$00  ;; stop Timer A
  sta $DD04
  sta $DD05
  sta $DD06
  sta $DD07
  sta $DD0E 
  sta $DD0F
  sta $dd0C
                lda     #$7f
                sta     $dd0d  

GETK2   JSR $FF9F               ; SCAN KEYBOARD
        JSR $FFE4               ; GET A CHARACTER
        CMP #$00        
        BEQ GETK2
        JMP START


FINISH_TEXT     text    'finished.'

                





