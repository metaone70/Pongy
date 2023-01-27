; 10 SYS (2080)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $38, $30, $29, $00, $00, $00

*=$0820
BEGINNING
        SEI
        LDA #$20              ;WHEN RESTORE KEY IS HIT
        STA $0318             ;IT POINTS TO THE START
        LDA #$08              ;OF THIS PROGRAM, WHICH IS 
        STA $0319             ; $0820
        LDA #$00               ;BLACK COLOR 
        STA $D021              ;FOR THE BORDER AND
        STA $D020              ;BACKGROUND

        LDA #%00011110         ; %0001, 1 : $0400 - $07FF SCREEN RAM 
        STA $D018              ;  %101, 5 : $2800-$2FFF   --> NO BIT 0
                               ; SCREEN RAM AT $0400, CHARACTER TAM $2800

        JMP MAINLOOP

;DRAW THE PLAYFIELD ON THE SCREEN-----------------------------------------------
DRAW_PLAY_SCREEN
        JSR $E544              ; CLEAR SCREEN VIA KERNAL ROUTINE
        LDA #$00
        STA $FB
        STA $FD                 ; $0400 SCREEN RAM
        STA $F7
        LDA #$48                ; $4800 CHARACTER DATA -->  TRANSFER TO $0400
        STA $FC
        LDA #$04
        STA $FE
        LDA #$00                ; $4C00 SCREEN COLOR DATA --> TRANSFER TO $D800
        STA $F9
        LDA #$4C
        STA $FA
        LDA #$D8                ; $D800 --> COLOR RAM
        STA $F8
        LDX #$00
LOOPTEXT        
        LDY #$00
INNERLOOPY      
        LDA ($FB),Y             ; LOOPING W/ ZERO PAGE ADDRESSES 
        STA ($FD),Y             ; FOR DATA COPYING
        LDA ($F9),Y
        STA ($F7),Y
        INY
        BNE INNERLOOPY
        INC $FC
        INC $FE
        INC $FA
        INC $F8
        INX
        CPX #$04
        BNE LOOPTEXT
        RTS

;SETUP SPRITES-----------------------------------------------------------------
SETUP_SPRITES    
        LDA #14
        STA $D027               ; LIGHT BLUE COLOR FOR SPRITE 0 (RIGHT PLAYER)
        LDA #10
        STA $D028               ; LIGHT RED COLOR FOR SPRITE 1 (LEFT PLAYER),
        LDA #01
        STA $D029               ; WHITE COLOR FOR SPRITE 2 (BALL)
        STA $D02A               ; WHITE FOR GOAL SPRITES
        STA $D02B
        STA $D02C
        STA $D02D

        LDA #$80                ; SET SPRITE POINTERS --> $2000 (128 x 64)
        STA $07F8               ; RIGHT PLAYER 
        LDA #$81                
        STA $07F9               ; LEFT PLAYER
        LDA #$82                
        STA $07FA               ; BALL
        LDA #$83
        STA $07FB               ; G 
        LDA #$84
        STA $07FC               ; O
        LDA #$85
        STA $07FD               ; A
        LDA #$86        
        STA $07FE               ; L

        LDA #%01111000          ; EXPAND GOAL SPRITES
        STA $D017               ; Y DIRECTION
        STA $D01D               ; X DIRECTION

        LDA #%00000111
        STA $D015               ; ENABLE SPRITES 0,1 AND 2

        LDX #$00
L1      LDA POSTABLE,X
        STA SPRITEPOS,X
        STA $D000,X
        INX
        CPX #$0E
        BNE L1

        LDA #%00000001          ; ENABLE MSB FOR PLAYER 1 
        STA $D010               ; (IT IS ALREADY ON THE FAR RIGHT AT THE BEGINNING)
        
        RTS

;BALL DIRECTION AT START----------------------------------------------------
BALL_DIRECTION_AT_START
        LDA #$C7                ; this block gets the SID producing
        STA $D40E               ; random numbers up 200 (0-199)
        STA $D40F               ; because it's divisible by 4 
        LDA #$80        
        STA $D412             

        LDA $D41B
        CMP #$32
        BCC BUL
        CMP #$64
        BCC BDL
        CMP #$96
        BCC BUR
        JMP BDR

BUL     LDA #$01
        STA BALLDIRECTION
        RTS

BDL     LDA #$02
        STA BALLDIRECTION
        RTS

BUR     LDA #$03
        STA BALLDIRECTION
        RTS

BDR     LDA #$04
        STA BALLDIRECTION

        LDA POSTABLE+04
        STA SPRITEPOS+04    
        LDA POSTABLE+05
        STA POSTABLE+05

        RTS

;DELAY LOOP---------------------------------------------------------------------
DELAY_LOOP
        LDA #$00
DLOOP   CMP $D012
        BNE DLOOP 
        RTS

;EXPAND SPRITE MSB--------------------------------------------------------------
PLACESPRITES
        LDA SPRITEPOS           ; PLAYER 1 SPRITE
        STA $D000
        LDA SPRITEPOS+1
        STA $D001
        LDA #%00000001          ; SET MSB ON FOR SPRITE 1
        ORA $D010               ; SINCE IT IS AT FAR RIGHT
        STA $D010

        LDA SPRITEPOS+2         ; PLAYER 2 IS IN STANDARD SCREEN
        STA $D002               ; NO NEED TO SET MSB
        LDA SPRITEPOS+3
        STA $D003

        LDA SPRITEPOS+4         ; BALL SPRITE
        STA $D004
        LDA SPRITEPOS+5
        STA $D005
        
        LDX BALLDIRECTION
URIGHT  CPX #$03
        BNE DRIGHT
        LDA SPRITEPOS+4
        CMP #$00
        BNE ENDPLACE
        LDA #%00000100
        ORA $D010
        STA $D010
        RTS
        
DRIGHT  CPX #$04
        BNE DLEFT
        LDA SPRITEPOS+4
        CMP #$00       
        BNE ENDPLACE
        LDA #%00000100
        ORA $D010
        STA $D010
        RTS

DLEFT   CPX #$02
        BNE ULEFT
        LDA SPRITEPOS+4
        CMP #$FF       
        BNE ENDPLACE
        LDA #%11111001
        AND $D010
        STA $D010
        RTS

ULEFT   LDA SPRITEPOS+4
        CMP #$FF
        BNE ENDPLACE
        LDA #%11111001
        AND $D010
        STA $D010        

ENDPLACE
        RTS

;START TIMER CLOCK--------------------------------------------------------------

START_TIMER_CLOCK
GETK1   JSR $FF9F               ; SCAN KEYBOARD
        JSR $FFE4               ; GET A CHARACTER
        CMP #$00        
        BEQ GETK1

        LDA     #$7F
        STA     $DD0D           ; disable all CIA2 NMIs
        LDA     #<TICKERLSR
        STA     $318
        LDA     #>TICKERLSR
        STA     $319
                ; 985248 (=$F08A0) cycles per second ->
                ; load timer A with $F08A and timer B with $10
        LDA     #$8A
        STA     $DD04
        LDA     #$F0
        STA     $DD05
        LDA     #$10
        STA     $DD06
        LDA     #$0
        STA     $DD07
        LDA     #$11
        STA     $DD0e           ; start timer A counting cycles
        LDA     #$51
        STA     $DD0F           ; start timer B counting t.A underflows
        LDA     #$82
        STA     $DD0D           ; enable NMI on t.B underflows
        RTS

TICKERLSR       
        PHA
        LDA     $DD0D
        BPL     DONET           ; NMI not from CIA2
        JSR     WRITE           ; GO AND WRITE THE COUNTER ON SCREEN
DONET   PLA
        RTI

WRITE
        DEC $043C               ; DECREMENT RIGHT DIGIT
        LDA $043C
        CMP #$2F                ; DID WE REACH BELOW ZERO?
        BNE EXITT               ; NO, CONTINUE PROCESS
        LDA #$3A                ; YES, ZERO THE RIGHT DIGIT
        STA $043C               ; AND DECREMENT LEFT DIGIT
        DEC $043B
        LDA $043B               ; DID WE REACH BELOW ZERO?
        CMP #$2F
        BNE WRITE               ;NO, REPEAT AND RINSE
        JMP GAME_OVER           ;YES, WE ARE OUT OF TIME - GAME IS OVER

EXITT   RTS

;GAME OVER AFTE 90 SECONDS------------------------------------------------------
GAME_OVER
        LDA #%00000000
        STA $D015               ; DISABLE ALL SPRITES
        LDX #$00                ; AND RESET THE COUNTER TO 90
        LDA #$39                ; ON SCREEN
        STA $043B
        LDA #$30
        STA $403C

GOT     LDA GAMEOVERTEXT,X      ; WRITE GAME OVER TEXT TO SCREEN
        STA $05C7,X
        INX
        CMP #$0A
        BNE GOT
GETK    JSR $FF9F               ; SCAN KEYBOARD
        JSR $FFE4               ; GET A CHARACTER
        CMP #$00        
        BEQ GETK
        JMP BEGINNING           ; JUMP TO BEGINNING

;RESET BALL WHEN PLAYER SCORES--------------------------------------------------
RESET_BALL_POSITION
        JSR BALL_DIRECTION_AT_START
        LDA POSTABLE+4
        STA SPRITEPOS+4
        STA $D004
        LDA POSTABLE+5
        STA SPRITEPOS+5
        STA $D005
        LDA #%00000001
        STA$D010
        RTS

;MAINLOOP-----------------------------------------------------------------------
MAINLOOP
        JSR DRAW_PLAY_SCREEN
        JSR SETUP_SPRITES
        JSR BALL_DIRECTION_AT_START
        JSR START_TIMER_CLOCK
LOOPO
        JSR DELAY_LOOP
        JSR PLACESPRITES
        JSR CHECK_CONTROLS_P1
        JSR CHECK_CONTROLS_P2
        JSR CHECK_PLAYER_COLLISION
        JSR CHECK_TOP_COLLISION
        JSR CHECK_BOTTOM_COLLISION
        JSR MOVE_BALL
        JSR CHECK_PLAYER1_SCORES
        JSR CHECK_PLAYER2_SCORES
        ;JSR CHECK_TIMER_CLOCK
        JMP LOOPO
--------------------------------------------------------------------------------

; CHECK FOR PLAYER 1 JOYSTICK (LATER IT WILL BE PADDLE CONTROL)-----------------
CHECK_CONTROLS_P1

        LDA $DC01                 ;READ JOYSTICK
UP1     LSR A                     ;CHECK UP
        BCS DOWN1                 ;NOT UP? THEN GO DOWN
        LDY SPRITEPOS+1
        DEY
        DEY
        CPY #$49
        BCS SETUP1
        LDY #$49
SETUP1  STY SPRITEPOS+1

DOWN1   LSR A                     ;CHECK DOWN
        BCS EXIT1                 ;NOT DOWN?
        LDY SPRITEPOS+1
        INY
        INY
        CPY #$D5
        BCC SETDOWN1
        LDY #$D5
SETDOWN1 
        STY SPRITEPOS+1

EXIT1   RTS

; CHECK FOR PLAYER 2 JOYSTICK (LATER IT WILL BE PADDLE CONTROL)-----------------
CHECK_CONTROLS_P2

        LDA $DC00                 ;READ JOYSTICK
UP2     LSR A                     ;CHECK UP
        BCS DOWN2                 ;NOT UP? THEN GO DOWN
        LDY SPRITEPOS+3
        DEY
        DEY
        CPY #$49
        BCS SETUP2
        LDY #$49
SETUP2  STY SPRITEPOS+3

DOWN2   LSR A                     ;CHECK DOWN
        BCS EXIT2                 ;NOT DOWN?
        LDY SPRITEPOS+3
        INY
        INY
        CPY #$D5
        BCC SETDOWN2
        LDY #$D5
SETDOWN2 
        STY SPRITEPOS+3

EXIT2   RTS


;CHECK FOR BALL VS PLAYER COLLISION------------------------------------
CHECK_PLAYER_COLLISION 

        LDA SPRITEPOS+01                ; PLAYER1 Y COORDINATE
        SEC
        SBC #$32
        LSR
        LSR
        LSR                             ; CALCULATE THE ROW NUMBER
        TAX
        DEX                             ; DECRESE IT BY ONE
        TXA
        STA PLAYER1_Y                   ; STORE THE Y CHARACTER NUMBER

        LDA SPRITEPOS+03                ; PLAYER2 Y COORDINATE
        SEC
        SBC #$32
        LSR
        LSR
        LSR
        TAX
        DEX
        TXA
        STA PLAYER2_Y                   ; STORE THE Y CHARACTER NUMBER

        LDA SPRITEPOS+05                ; PLAYER2 Y COORDINATE
        SEC
        SBC #$3A
        LSR
        LSR
        LSR
        STA BALL_Y                      ; STORE THE Y CHARACTER NUMBER

        LDA $D010                       ; CHECK BALL MSB
        CMP #$01                        ; MSB=BALL ON THE LEFT
        BEQ BALL_IS_ON_THE_LEFT         ; OTHER (MSB=5) ON THE RIGHT

;OTHERWISE BALL IS ON THE RIGHT
        LDA SPRITEPOS+00                ; PLAYER 1 X POS
        SEC
        SBC SPRITEPOS+04                ; PLAYER 1 X POS - BALL X POS
        CMP #$05                        ; IS THE BALL CLOSE ENAUGH?
        BNE NOHIT                       ; NO, SO THE BALL IS STILL AWAY FROM PLAYER 1
        LDA PLAYER1_Y                   ; YES, CHECK THE Y COORDINATES (CHARACTER NUMBERS)               
        TAX
        DEX
        DEX
        TXA
        CMP BALL_Y
        BEQ COLLIDED1
        LDA PLAYER1_Y               
        TAX
        DEX
        TXA
        CMP BALL_Y
        BEQ COLLIDED1
        LDA PLAYER1_Y
        CMP BALL_Y
        BEQ COLLIDED1
        LDA PLAYER1_Y
        TAX
        INX
        TXA
        CMP BALL_Y
        BEQ COLLIDED1        
        RTS

BALL_IS_ON_THE_LEFT
        LDA SPRITEPOS+04                ; BALL X POS
        SEC
        SBC SPRITEPOS+02                ; BALL X POS - PLAYER 2 X POS
        CMP #$06                        ; ARE THEY SIDE BY SIDE?
        BNE NOHIT                       ; NO, SO THE BALL IS STILL AWAY FROM PLAYER 2
        LDA PLAYER2_Y                   ; YES, CHECK THE Y COORDINATES (CHARACTER NUMBERS)
        TAX
        DEX
        DEX
        TXA
        CMP BALL_Y
        BEQ COLLIDED2
        LDA PLAYER2_Y                   
        TAX
        DEX
        TXA
        CMP BALL_Y
        BEQ COLLIDED2
        LDA PLAYER2_Y
        CMP BALL_Y
        BEQ COLLIDED2
        LDA PLAYER2_Y
        TAX
        INX
        TXA
        CMP BALL_Y
        BEQ COLLIDED2

NOHIT   RTS

COLLIDED1
        JSR PLAYERHITSOUND
        LDA BALLDIRECTION
        CMP #$03
        BNE OTHER1
        LDA #$01
        STA BALLDIRECTION
        RTS

OTHER1  LDA #$02
        STA BALLDIRECTION
        RTS

COLLIDED2
        JSR PLAYERHITSOUND
        LDA BALLDIRECTION
        CMP #$02
        BNE OTHER2
        LDA #$04
        STA BALLDIRECTION
        RTS

OTHER2  LDA #$03
        STA BALLDIRECTION
        RTS


;DIRECT BALL------------------------------------------------------------
MOVE_BALL
        LDA BALLDIRECTION
        CMP #$01
        BEQ BALL_UP_LEFT
        CMP #$02
        BEQ BALL_DOWN_LEFT
        CMP #$03
        BEQ BALL_UP_RIGHT
        CMP #$04
        BEQ BALL_DOWN_RIGHT
        RTS

; BALL MOVING UP & LEFT---------------------------------------------------------
BALL_UP_LEFT
        DEC SPRITEPOS+4                 ; DECREMENT THE X POS (TO LEFT)
        DEC SPRITEPOS+5                 ; DECREMENT THE Y POS (TO UP)
        RTS

; BALL MOVING DOWN & LEFT-------------------------------------------------
BALL_DOWN_LEFT
        DEC SPRITEPOS+4                 ; DECREMENT THE X POS (TO LEFT)
        INC SPRITEPOS+5                 ; INCREMENT THE Y POS (TO DOWN)
        RTS

; BALL MOVING UP & RIGHT--------------------------------------------------
BALL_UP_RIGHT
        INC SPRITEPOS+4                 ; DECREMENT THE X POS (TO LEFT)
        DEC SPRITEPOS+5                 ; INCREMENT THE Y POS (TO DOWN)
        RTS

; BALL MOVING DOWN & RIGHT-------------------------------------------------
BALL_DOWN_RIGHT
        INC SPRITEPOS+4                 ; DECREMENT THE X POS (TO LEFT)
        INC SPRITEPOS+5                 ; INCREMENT THE Y POS (TO DOWN)
        RTS

;CHECK IF PLAYER 1 SCORES-------------------------------------------------------
CHECK_PLAYER1_SCORES
        LDA $D010
        CMP #$01
        BNE SCEXIT1
        LDA SPRITEPOS+04
        CMP #$10
        BEQ PLAYER1_SCORES
SCEXIT1 RTS

PLAYER1_SCORES
        INC $0442
        LDA $0442
        CMP #$3A
        BNE DONE1
        LDA #$30
        STA $0442
        INC $0441
DONE1   JSR SCORESOUND
        JSR FLASHBORDER
        JSR RESET_BALL_POSITION
        RTS

;CHECK IF PLAYER 2 SCORES-------------------------------------------------------
CHECK_PLAYER2_SCORES
        LDA $D010
        CMP #$05
        BNE SCEXIT2
        LDA SPRITEPOS+04
        CMP #$45
        BEQ PLAYER2_SCORES
SCEXIT2 RTS

PLAYER2_SCORES
        INC $0436
        LDA $0436
        CMP #$3A
        BNE DONE2
        LDA #$30
        STA $0436
        INC $0435
DONE2   JSR SCORESOUND
        JSR FLASHBORDER
        JSR RESET_BALL_POSITION
        RTS

FLASHBORDER
        LDA #%001111111
        STA $D015               ; ENABLE SPRITES 0 TO 7

        LDX #$00
        LDY #$00
FL1     INC $D020
        INX
        CPX #$FF
        BNE FL1
        INY
        STY $D02A
        STY $D02B
        STY $D02C
        STY $D02D        
        CPY #$FF
        BNE FL1
        LDA #$00
        STA $D020
        LDA #%000000111
        STA $D015   
        RTS

;CHECK IF THE BALL IS AT THE BOTTOM OF THE FIELD-----------------------------
CHECK_BOTTOM_COLLISION
        LDA SPRITEPOS+5
        CMP #$DC
        BNE CBCEND
        LDA BALLDIRECTION
        CMP #$02
        BNE CHBC
        LDA #$01
        STA BALLDIRECTION
        JSR BORDERHITSOUND
        RTS

CHBC    LDA #$03
        STA BALLDIRECTION
        JSR BORDERHITSOUND        

CBCEND  RTS

;CHECK IF THE BALL IS AT THE TOP OF THE FIELD-----------------------------
CHECK_TOP_COLLISION
        LDA SPRITEPOS+5
        CMP #$40
        BNE CTCEND
        LDA BALLDIRECTION
        CMP #$01
        BNE CTC
        LDA #$02
        STA BALLDIRECTION
        JSR BORDERHITSOUND
        RTS

CTC     LDA #$04
        STA BALLDIRECTION
        JSR BORDERHITSOUND

CTCEND  RTS

;PLAY THE SOUND WHEN BALL HITS TOP AND BOTTOM BORDERS---------------------------
BORDERHITSOUND
;CLEAN SID
        LDA #0
        STA $D404
; VOLUME
        LDA #15
        STA $D418
; VOICE 1 FREQUENCY, LO-HI BYTE 
        LDA #143
        STA $D400
        LDA #10
        STA $D401
; VOICE 1 PULSE WAVEFORM WIDTH, LO-HI BYTE      
        LDA #128
        STA $D402
        LDA #0
        STA $D403
; VOICE 1 ADSR
        LDA #53
        STA $D405
        LDA #00
        STA $D406
; VOICE 1 WAVEFORM(S) PULSE 
        LDX #32
        INX
        TXA
        STA $D404
        RTS

;PLAY THE SOUND WHEN BALL HITS PLAYERS------------------------------------------
PLAYERHITSOUND
;CLEAN SID
        LDA #0
        STA $D40B
; VOLUME
        LDA #15
        STA $D418
; VOICE 2 FREQUENCY, LO-HI BYTE 
        LDA #156
        STA $D407
        LDA #12
        STA $D408
; VOICE 2 PULSE WAVEFORM WIDTH, LO-HI BYTE      
        LDA #128
        STA $D409
        LDA #0
        STA $D40A
; VOICE 2 ADSR
        LDA #53
        STA $D40C
        LDA #00
        STA $D40D
; VOICE 2 WAVEFORM(S) PULSE 
        LDX #32
        INX
        TXA
        STA $D40B
        RTS

;PLAY THE SOUND WHEN PLAYERS SCORE---------------------------------------------
SCORESOUND
        LDA  #00
        STA $D412
; VOICE 3 FREQUENCY, LO-HI BYTE 
        LDA #143
        STA $D40E
        LDA #10
        STA $D40F
; VOICE 3 PULSE WAVEFORM WIDTH, LO-HI BYTE 
        LDA #128
        STA $D410
        LDA #0
        STA $D411
; VOICE 3 ADSR
        LDA #76
        STA $D413
        LDA #2
        STA $D414
; VOICE 3 WAVEFORM(S) 
        LDX #128
        INX
        TXA
        STA $D412

        RTS


;-------VARIABLES--------------------------------------------------------

SPRITEPOS  = $0370                      ; POSITIONS FOR THE SPRITES

POSTABLE BYTE $41,$85,$16,$85,$AA,$8D,$40,$85,$7F,$85,$BF,$85,$FF,$85   ; SPRITE INITIAL POSITIONS

BALLDIRECTION BYTE $00                   ; 1=UP&LEFT, 2=DOWN&LEFT, 3=UP&RIGHT, 4=DOWN&RIGHT

PLAYER1_Y = $FB
PLAYER2_Y = $FC
BALL_Y = $FD

GAMEOVERTEXT text 'game  over'

;SPRITE DEFINITIONS---------------------------------------------------------
*=$2000

;SPRITE 0 DATA ; PLAYER 1 - RIGHT PLAYER --> POINTER $80
 byte $00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00
 byte $3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E
 byte $00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00
 byte $00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00

;SPRITE 1 DATA ; PLAYER 2 - LEFT PLAYER --> POINTER $81
 byte $00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00
 byte $3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E
 byte $00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00
 byte $00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00,$3E,$00,$00

;SPRITE 2 DATA ; BALL --> POINTER $82
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1C,$00,$00,$3E
 byte $00,$00,$6B,$00,$00,$7F,$00,$00,$7F,$00,$00,$36,$00,$00,$1C,$00
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;SPRITE 3 DATA ; G -> POINTER $83
 byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$00,$F0
 byte $00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$0F,$FF,$F0,$0F
 byte $FF,$F0,$0F,$FF,$F0,$0F,$FF,$F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F
 byte $F0,$00,$0F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$07

;SPRITE 4 DATA ; O -> POINTER $84
 byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$0F,$F0
 byte $00,$0F,$F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F,$F0,$00
 byte $0F,$F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F
 byte $F0,$00,$0F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00

;SPRITE 5 DATA ; A -> POINTER $85
 byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$0F,$F0
 byte $00,$0F,$F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F,$FF,$FF,$FF,$FF,$FF
 byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F
 byte $F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F,$F0,$00,$0F,$60

;SPRITE 3 DATA ; L -> POINTER $86
 byte $F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0
 byte $00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00
 byte $00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00
 byte $F0,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$C0

; NEW FONT FILE--------------------------------------------------------------       
*=$3800                          
incbin "zx.bin"

; PLAY SCREEN--------------------------------------------------------------    

;SCREEN DATA
*=$4800
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$13,$03,$0F,$12,$05,$20,$20,$20,$14,$09,$0D,$05,$20,$20,$20,$13,$03,$0F,$12,$05,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $10,$0C,$01,$19,$05,$12,$20,$32,$20,$20,$20,$20,$20,$30,$30,$20,$20,$20,$20,$39,$30,$20,$20,$20,$20,$30,$30,$20,$20,$20,$20,$20,$10,$0C,$01,$19,$05,$12,$20,$31
        BYTE    $79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$6C,$7B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$7C,$7E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78
        BYTE    $20,$20,$20,$20,$20,$0E,$05,$17,$20,$10,$0F,$0E,$07,$21,$20,$20,$20,$20,$20,$20,$20,$20,$28,$03,$29,$20,$32,$30,$32,$33,$20,$0D,$05,$14,$05,$13,$05,$16,$20,$20

;COLOR DATA
*=$4C00 
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0D,$0D,$0D,$0D,$0D,$00,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        BYTE    $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
        BYTE    $01,$01,$01,$01,$01,$01,$00,$00,$05,$05,$05,$05,$05,$05,$05,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$05
        BYTE    $05,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $05,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $05,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$05,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
        BYTE    $05,$05,$05,$05,$05,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$05,$05,$05,$05,$05,$05,$05,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
