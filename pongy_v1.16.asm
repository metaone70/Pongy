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

        ;LDA #%00011110         ; %0001, 1 : $0400 - $07FF SCREEN RAM 
        ;STA $D018              ;  %101, 5 : $2800-$2FFF   --> NO BIT 0
                               ; SCREEN RAM AT $0400, CHARACTER TAM $2800
        LDA #$01
        STA SPEED              ; SET SPEED TO 1

        LDA $DC02
        STA TEMPBUFFER

;LOAD BITMAP SCREEN-------------------------------------------------------------
INTRO
        LDA #$3B
        STA $D011
        LDA #$18
        STA $D016
        LDA #$78
        STA $D018
        LDA #$C6
        STA $DD00
        LDA #$00
        STA $D020
        LDA $8710
        STA $D021
        LDX #$FA
LOADIMAGE       
        LDA SCRDATA-1,X
        STA SCRRAM-1,X
        LDA SCRDATA+249,X
        STA SCRRAM+249,X
        LDA SCRDATA+499,X
        STA SCRRAM+499,X        
        LDA SCRDATA+749,X
        STA SCRRAM+749,X
        LDA COLDATA-1,X
        STA COLRAM-1,X  
        LDA COLDATA+249,X  
        STA COLRAM+249,X  
        LDA COLDATA+499,X  
        STA COLRAM+499,X  
        LDA COLDATA+749,X  
        STA COLRAM+749,X  
        DEX  
        BNE LOADIMAGE  
 
INPUTKEY        
        JSR $FF9F        
        JSR $FFE4
        BEQ INPUTKEY
        STA KEY
        CMP #$20
        BNE INPUTKEY

        LDA #$9B
        STA $D011
        LDA #$C8
        STA $D016                
        LDA #$1F 
        STA $D018
        LDA #$17
        STA $DD00        

        JMP MAINLOOP

;DRAW THE PLAYFIELD ON THE SCREEN-----------------------------------------------
DRAW_PLAY_SCREEN
        ;JSR $E544              ; CLEAR SCREEN VIA KERNAL ROUTINE
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
        LDA #$00                ; MULTICOLOR REGISTER
        STA $D025               ; BLACK AND LIGHT GREY
        LDA #$0F
        STA $D026
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

        LDA #%00000011          ; SET MULTICOLOR BITS     
        STA $D01C

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

        LDA #%01111011          ; EXPAND SPRITES 
        STA $D017               ; Y DIRECTION   -> EXCEPT THE BALL
        LDA #%01111000
        STA $D01D               ; X DIRECTION -> ONLY GOAL SPRITES
        
        LDA #%00000000
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
        LDA #$C7                ; GET RANDOM NUMBERS
        STA $D40E               ; FROM SID
        STA $D40F               ; UPTO 200
        LDA #$80                ; 200 IS DIVISABLE BY 4
        STA $D412               ; PSEUDO RANDOM NUMBER :-/

        LDA $D41B               ; THIS BLOCK DIVERTS THE BALL
        CMP #$32                ; IN ACCORDANCE WITH THE RANDOM NUMBER
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

        LDA POSTABLE+04         ; RESET THE BALL COORDINATES
        STA SPRITEPOS+04        ; IN ACCORDANCE WITH INITIAL
        LDA POSTABLE+05         ; POSITION TABLE
        STA POSTABLE+05

        RTS

;CHECK SPEED AND DELAY LOOP-----------------------------------------------------
SPEED_AND_DELAY
        LDA $043B               ; CHECK TIMER 
        CMP #$30                ; IF IS THE LAST 10 SECONDS
        BEQ DELAY_FASTEST       ; GO CRAZY FAST            
        CMP #$36                ; IF IT IS BELOW 60
        BCC DELAY_FASTER        ; GO FASTER                      
                                ; OTHERWISE START WITH STANDARD SPEED
DELAY_NORMAL                    ; SPEED FOR THE FIRST 50 SECONDS
         LDA #$00
         STA $21D0
         STA $21D1
@WAIT11  INC $21D1
@WAIT10  INC $21D0
         LDA $21D0
         CMP #128
         BNE @WAIT10
         LDA $21D1
         CMP #2                 ; ADJUSTMENT LINE FOR TIMING
         BNE @WAIT11
         RTS

DELAY_FASTER                     ; SPEED 2 FOR THE LAST 00 SECONDS
        LDA $D850
        CMP #$07        
        BEQ FASTER

        LDX #$00
YELLOW  LDA #$07                ; YELLOW LINES
        STA $D850,X
        STA $DB98,X
        INX
        CPX #$28
        BNE YELLOW

FASTER
         LDA #$00
         STA $F7
         STA $F8
@WAIT21  INC $F8
@WAIT20  INC $F7
         LDA $F7
         CMP #64
         BNE @WAIT20
         LDA $F8
         CMP #2                 ; ADJUSTMENT LINE FOR TIMING
         BNE @WAIT21
         LDA $043B
         RTS

DELAY_FASTEST                     ; SPEED FOR THE LAST 10 SECONDS
        LDA $D850
        CMP #$02        
        BEQ FASTEST

        LDX #$00
RED     LDA #$02                ; RED LINES
        STA $D850,X
        STA $DB98,X
        INX
        CPX #$28
        BNE RED

FASTEST
         LDA #$00
         STA $21D0
         STA $21D1
@WAIT31  INC $21D1
@WAIT30  INC $21D0
         LDA $21D0
         BNE @WAIT30
         LDA $21D1
         CMP #1                 ; ADJUSTMENT LINE FOR TIMING
         BNE @WAIT31
         INC $D020       
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

;WAIT FOR A BUTTON PRESS------------------------------------------------------------
WAIT_BUTTON
                
        LDA $DC01              
        LSR A
        LSR A
        LSR A                   ; CHECK PADDLE 1 BUTTON                 
        BCC EXITB               ; NOT PRESSED? GO AND CHECK PADDLE 2 BUTTON
        LSR A
        BCC EXITB
        JSR COLORWASH

RLOOP   LDA $D012
        BNE RLOOP

        JMP WAIT_BUTTON

EXITB   LDA #$20
        LDY #$00
CL1     STA $051F,Y
        STA $061E,Y
        INY
        CPY #$FF
        BNE CL1

        RTS

;COLOR WAHS---------------------------------------------------------------------
COLORWASH              
        LDA COLOR+$00
        STA COLOR+$28
        LDX #$00
CYCLE   LDA COLOR+$01,X
        STA COLOR+$00,X
        LDA COLOR,X
        STA $DAD0,X
        STA $DAA8,X
        INX
        CPX #$28
        BNE CYCLE
        RTS


;START TIMER CLOCK--------------------------------------------------------------
START_TIMER_CLOCK
        
        LDA #%00000111      ; ENABLE SPRITES
        STA $D015

        LDA #$7F
        STA $DD0D           ; disable all CIA2 NMIs
        LDA #<NMI
        STA $318
        LDA #>NMI
        STA $319
        LDA #$8A
        STA $DD04
        LDA #$F0
        STA $DD05
        LDA #$10
        STA $DD06
        LDA #$0
        STA $DD07
        LDA #%00010001
        STA $DD0E           ; start timer A counting cycles
        LDA #%01010001
        STA $DD0F           ; start timer B counting t.A underflows
        LDA #%10000010
        STA $DD0D           ; enable NMI on t.B underflows
        RTS

NMI       
        PHA
        TXA
        PHA
        TYA
        PHA

        LDA $DD0D
        BPL DONE            ; NMI not from CIA2
        JSR WRITE_COUNTER   ; GO AND WRITE THE COUNTER ON SCREEN
DONE    PLA
        TAY
        PLA
        TAX
        PLA

        RTI

WRITE_COUNTER
        DEC $043C               ; DECREMENT RIGHT DIGIT
        LDA $043C
        CMP #$2F                ; DID WE REACH BELOW ZERO?
        BNE EXIT                ; NO, CONTINUE PROCESS
        LDA #$39                ; YES, ZERO THE RIGHT DIGIT
        STA $043C               ; AND DECREMENT LEFT DIGIT
        DEC $043B
        LDA $043B               ; DID WE REACH BELOW ZERO?
        CMP #$2F
        ;CMP #$36
        BEQ GAME_OVER
EXIT    RTS

;GAME OVER AFTE 90 SECONDS------------------------------------------------------
GAME_OVER

        LDA #$00                ; STOP THE TIMER
        STA $DD0E
        STA $DD0F
        STA $D020

        LDA #%00000000
        STA $D015               ; DISABLE ALL SPRITES
        LDA #$00                
        STA $D83B               ;BLACKOUT THE TIMER
        STA $D83C

        LDX #$00
GOT1    LDA PRINT_GAME_OVER,X
        STA $05C7,X
        INX
        CPX #$0A
        BNE GOT1

        LDX #$00
GOT2    LDA START_GAME_MESSAGE1,X
        STA $06B2,X
        INX
        CPX #$14
        BNE GOT2

        LDX #$00
GOT3    LDA START_GAME_MESSAGE2,X
        STA $06DA,X
        INX
        CPX #$14
        BNE GOT3

        LDA $0435
        CMP $0441
        BCC PLAYER1WINS_MESSAGE
        LDA $0441
        CMP $0435
        BCC PLAYER2WINS_MESSAGE
        LDA $0436
        CMP $0442
        BCC PLAYER1WINS_MESSAGE
        LDA $0442
        CMP $0436
        BCC PLAYER2WINS_MESSAGE                
        JSR DRAW

FINALIZE_MATCH
        JSR WAIT_BUTTON
        JMP MAINLOOP           ; JUMP TO MAIN LOOP

PLAYER1WINS_MESSAGE
        LDX #$00
PW1     LDA PLAYER_1_WINS,X   
        STA $063D,X
        INX
        CPX #$0F
        BNE PW1
        JMP FINALIZE_MATCH

PLAYER2WINS_MESSAGE
        LDX #$00
PW2     LDA PLAYER_2_WINS,X 
        STA $063D,X
        INX
        CPX #$0F
        BNE PW2
        JMP FINALIZE_MATCH

DRAW
        LDX #$00
DW      LDA DRAW_MESSAGE,X 
        STA $063E,X
        INX
        CPX #$0E
        BNE DW
        RTS

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
        STA $D010
        RTS

;MAINLOOP-----------------------------------------------------------------------
MAINLOOP
        JSR DRAW_PLAY_SCREEN
        JSR SETUP_SPRITES
        JSR BALL_DIRECTION_AT_START 
        JSR WAIT_BUTTON
        JSR START_TIMER_CLOCK
LOOPO
        JSR SPEED_AND_DELAY
        JSR PLACESPRITES
        JSR CHECK_CONTROLS_P1
        JSR CHECK_CONTROLS_P2
        JSR CHECK_PLAYER_COLLISION
        JSR CHECK_TOP_COLLISION
        JSR CHECK_BOTTOM_COLLISION
        JSR MOVE_BALL
        JSR UPDATE_PLAYER1_SCORE
        JSR UPDATE_PLAYER2_SCORE
        JMP LOOPO
--------------------------------------------------------------------------------

; CHECK FOR PLAYER 1 PADDLE IN PORT 1-------------------------------------------
CHECK_CONTROLS_P1
        LDA #%11000000          ; SELECT PINS 6-7 FOR 
        STA $DC02               ; DATA PORT B  ->DISABLE KEYBOARD?
        LDA $DC00               ;Control-Port 1 selected (when bit6=1 and bit7=0)
        AND #%00111111          ;clear bit-6/7        
        ORA #%01000000          ;set bit-6
        STA $DC00               ;now control-port 1 is selected for reading the POT registers

        LDX $D419
        STX SPRITEPOS+1
        LDX SPRITEPOS+1
        CPX #$42
        BCS SETUP1
        LDX #$42

SETUP1  CPX #$C8
        BCC SETUP2
        LDX #$C8

SETUP2  STX SPRITEPOS+1

        LDA TEMPBUFFER
        STA $DC02
        RTS

; CHECK FOR PLAYER 2 PADDLE IN PORT 1-------------------------------------------
CHECK_CONTROLS_P2
        LDA #%11000000          ; SELECT PINS 6-7 FOR 
        STA $DC02               ; DATA PORT B  ->DISABLE KEYBOARD?
        LDA $DC00               ;Control-Port 1 selected (when bit6=1 and bit7=0)
        AND #%00111111          ;clear bit-6/7        
        ORA #%01000000          ;set bit-6
        STA $DC00               ;now control-port 1 is selected for reading the POT registers

        LDY $D41A 
        STY SPRITEPOS+3
        LDY SPRITEPOS+3
        CPY #$42
        BCS SETUP3
        LDY #$42
        
SETUP3  CPY #$C8
        BCC SETUP4
        LDY #$C8

SETUP4  STY SPRITEPOS+3

        LDA TEMPBUFFER
        STA $DC02
        RTS

;CHECK FOR BALL VS PLAYER COLLISION------------------------------------
CHECK_PLAYER_COLLISION 

        LDA $D01E
        CMP #%00000101
        BEQ COLLIDED1
        CMP #%00000110
        BEQ COLLIDED2
        RTS

COLLIDED1
        JSR PLAYERHITSOUND
        LDA BALLDIRECTION
        CMP #$03
        BNE OTHER1
        LDA #$01
        STA BALLDIRECTION
        RTS

OTHER1  LDA BALLDIRECTION
        CMP #$04
        BNE EGZIT1
        LDA #$02
        STA BALLDIRECTION
EGZIT1  RTS

COLLIDED2
        JSR PLAYERHITSOUND
        LDA BALLDIRECTION
        CMP #$02
        BNE OTHER2
        LDA #$04
        STA BALLDIRECTION
        RTS

OTHER2  LDA BALLDIRECTION
        CMP #$01
        BNE EGZIT2
        LDA #$03
        STA BALLDIRECTION
EGZIT2  RTS


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
UPDATE_PLAYER1_SCORE
        LDA $D010
        CMP #$01
        BNE SCEXIT1
        LDA SPRITEPOS+04
        CMP #$05
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
        JSR FLASHBORDER_BLUE
        JSR RESET_BALL_POSITION
        RTS

FLASHBORDER_BLUE
        LDA #%001111111
        STA $D015               ; ENABLE SPRITES 0 TO 7
        LDA #$0E
        STA $D020

        LDX #$00
FLB2    LDY #$00
FLB1    STY $D02A
        STY $D02B
        STY $D02C
        STY $D02D        
        INY
        CPY #$FF
        BNE FLB1
        INX
        CPX #$FF
        BNE FLB2

        LDA #$00
        STA $D020
        LDA #%000000111
        STA $D015   
        RTS

;CHECK IF PLAYER 2 SCORES-------------------------------------------------------
UPDATE_PLAYER2_SCORE
        LDA $D010
        CMP #$05
        BNE SCEXIT2
        LDA SPRITEPOS+04
        CMP #$50
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
        JSR FLASHBORDER_RED
        JSR RESET_BALL_POSITION
        RTS

FLASHBORDER_RED
        LDA #%001111111
        STA $D015               ; ENABLE SPRITES 0 TO 7
        LDA #$02
        STA $D020

        LDX #$00
FLR2    LDY #$00
FLR1    STY $D02A
        STY $D02B
        STY $D02C
        STY $D02D        
        INY
        CPY #$FF
        BNE FLR1
        INX
        CPX #$FF
        BNE FLR2

        LDA #$00
        STA $D020
        LDA #%000000111
        STA $D015   
        RTS

;CHECK IF THE BALL IS AT THE BOTTOM OF THE FIELD-----------------------------
CHECK_BOTTOM_COLLISION
        LDA SPRITEPOS+5
        CMP #$DB
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
        CMP #$43
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
SPEED BYTE 00

POSTABLE BYTE $37,$85,$1F,$85,$AA,$8D,$40,$85,$7F,$85,$BF,$85,$FF,$85   ; SPRITE INITIAL POSITIONS

BALLDIRECTION = $0380                   ; 1=UP&LEFT, 2=DOWN&LEFT, 3=UP&RIGHT, 4=DOWN&RIGHT
TEMPBUFFER BYTE 00

PLAYER1_Y = $FB
PLAYER2_Y = $FC
BALL_Y = $FD

SCRDATA = 32576                             ; BITMAP TITLE DATA - $7F40
COLDATA = 33576                             ; BITMAP TITLE COLOR DATA - $8328     
SCRRAM = 23552                              ; BITMAP TITLE SCREEN LOCATION - $5C00
COLRAM = 55296                              ; BITMAP TITLE COLOR LOCATION - $D800 
KEY BYTE 0                                 ; GET CHAR. OF KEY PRESSED

PRINT_GAME_OVER  text    'game  over'
PLAYER_1_WINS    text 'player 1 wins !'
PLAYER_2_WINS    text 'player 2 wins !'
DRAW_MESSAGE     text 'it is a draw ! '

START_GAME_MESSAGE1 text 'press paddle buttons'
START_GAME_MESSAGE2 text '   to start over    '

;COLOR WASH TABLE---------------------------------------------------------------

COLOR        BYTE $09,$09,$02,$02,$08
             BYTE $08,$0A,$0A,$0F,$0F
             BYTE $07,$07,$01,$01,$01
             BYTE $01,$01,$01,$01,$01
             BYTE $01,$01,$01,$01,$01
             BYTE $01,$01,$01,$07,$07
             BYTE $0F,$0F,$0A,$0A,$08
             BYTE $08,$02,$02,$09,$09
             BYTE $00,$00,$00,$00,$00


;SPRITE DEFINITIONS---------------------------------------------------------
*=$2000

;SPRITE 0 DATA ; PLAYER 1 - RIGHT PLAYER --> POINTER $80
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$15,$00
 byte $00,$2A,$00,$00,$2A,$00,$00,$2A,$00,$00,$2A,$00,$00,$2A,$00,$00
 byte $2A,$00,$00,$2A,$00,$00,$2A,$00,$00,$2A,$00,$00,$2A,$00,$00,$2A
 byte $00,$00,$15,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;SPRITE 1 DATA ; PLAYER 2 - LEFT PLAYER --> POINTER $81
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$54,$00,$00,$A8
 byte $00,$00,$A8,$00,$00,$A8,$00,$00,$A8,$00,$00,$A8,$00,$00,$A8,$00
 byte $00,$A8,$00,$00,$A8,$00,$00,$A8,$00,$00,$A8,$00,$00,$A8,$00,$00
 byte $54,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;SPRITE 2 DATA ; BALL --> POINTER $82
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3C,$00,$00,$7E
 byte $00,$00,$5A,$00,$00,$7E,$00,$00,$66,$00,$00,$3C,$00,$00,$00,$00
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

;SPRITE 6 DATA ; L -> POINTER $86
 byte $F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0
 byte $00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00
 byte $00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00
 byte $F0,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$C0

; NEW FONT FILE--------------------------------------------------------------       
*=$3800                          
incbin "zx3.bin"

; PLAY SCREEN--------------------------------------------------------------    

;SCREEN DATA
*=$4800
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$13,$03,$0F,$12,$05,$20,$20,$20,$14,$09,$0D,$05,$20,$20,$20,$13,$03,$0F,$12,$05,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $10,$0C,$01,$19,$05,$12,$20,$32,$20,$20,$20,$20,$20,$30,$30,$20,$20,$20,$20,$39,$30,$20,$20,$20,$20,$30,$30,$20,$20,$20,$20,$20,$10,$0C,$01,$19,$05,$12,$20,$31
        BYTE    $79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$17,$05,$0C,$03,$0F,$0D,$05,$20,$14,$0F,$20,$6D,$6E,$6F,$70,$71,$72,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$10,$0C,$05,$01,$13,$05,$20,$09,$0E,$13,$05,$12,$14,$20,$19,$0F,$15,$12,$20,$10,$01,$04,$04,$0C,$05,$20,$13,$05,$14,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$14,$0F,$20,$0A,$0F,$19,$13,$14,$09,$03,$0B,$20,$10,$0F,$12,$14,$20,$31,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$02,$05,$17,$01,$12,$05,$21,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$14,$08,$05,$20,$13,$10,$05,$05,$04,$20,$17,$09,$0C,$0C,$20,$07,$05,$14,$20,$06,$01,$13,$14,$05,$12,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$01,$0E,$04,$20,$0D,$0F,$12,$05,$20,$03,$08,$01,$0C,$0C,$05,$0E,$07,$09,$0E,$07,$20,$21,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$10,$12,$05,$13,$13,$20,$01,$20,$10,$01,$04,$04,$0C,$05,$20,$02,$15,$14,$14,$0F,$0E,$20,$14,$0F,$20,$13,$14,$01,$12,$14,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78
        BYTE    $20,$6D,$20,$6E,$20,$6F,$20,$70,$20,$71,$20,$72,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$51,$20,$20,$49,$4A,$4B,$4A,$4C,$4A,$4D,$20,$50,$4E,$4F,$20

;COLOR DATA
*=$4C00 
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0D,$0D,$0D,$0D,$0D,$00,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$00,$05,$05,$05,$05,$01,$01,$05,$05,$05,$05,$01,$01,$05,$05,$05,$05,$01,$01,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        BYTE    $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
        BYTE    $05,$00,$05,$00,$00,$00,$00,$00,$05,$05,$05,$05,$05,$05,$05,$00,$00,$00,$00,$05,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $05,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $05,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $05,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
        BYTE    $05,$01,$05,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01


*=$6000                          ; BITMAP KOALA IMAGE
incbin "pongy61.prg",2

