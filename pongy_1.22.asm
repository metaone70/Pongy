;PONGY! V1.22 (C) METESEV 2023
;WRITTEN BETWEEN 20jAN23-14FEB23

;MEMORY MAP OF THE GAME
;$0801 - $0810   PROGRAM START
;$0820 - $0FB2   MAIN PROGRAM
;$OFB3 - $1089   VARIABLES AND TABLES
;$2000 - $21B0   SPRITE DEFINITIONS
;$2200 - $2EEA   MUSIC FILE (4K)
;$3800 - $4800   FONT FILE (4K)
;$4800 - $4BE8   SCREEN DATA (1000 BYTES)     
;$4C00 - $4FE8   SCREEN COLOR (1000 BYTES)
;$6000 - $8711   BITMAP FILE (10K)

;ADDED JOYSTICK OPTION

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
        LDA #$00              ;BLACK COLOR 
        STA $D021             ;FOR THE BORDER AND
        STA $D020             ;BACKGROUND

        LDA $DC02             ;LOAD THE PORT DIRECTION 
        STA TEMPBUFFER        ; AND STORE IT TO BUFFER LOCATION

        JSR PLAY_MUSIC

;LOAD BITMAP SCREEN-------------------------------------------------------------
INTRO
        LDA #$3B                ; PREPARES SCREEN FOR BITMAP
        STA $D011
        LDA #$18
        STA $D016
        LDA #$78
        STA $D018
        LDA #$C6
        STA $DD00
        LDA #$00                ; BLACK BORDER
        STA $D020
        LDA $8710               ; BACKGROUND COLOR IS TAKEN FROM BITMAP FILE
        STA $D021
        LDX #$FA
LOADIMAGE       
        LDA SCRDATA-1,X         ; LOADS SCREENDATA TO SCREEN RAM LOCATION
        STA SCRRAM-1,X
        LDA SCRDATA+249,X
        STA SCRRAM+249,X
        LDA SCRDATA+499,X
        STA SCRRAM+499,X        
        LDA SCRDATA+749,X
        STA SCRRAM+749,X
        LDA COLDATA-1,X         ; LOADS COLORDATA TO COLOR RAM LOCATION
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
        JSR $FF9F                ; WAITS FOR SPACE KEY
        JSR $FFE4       
        BEQ INPUTKEY
        STA KEY
        CMP #$20                ; COMPARES IT TO SPACE ($20=32)
        BNE INPUTKEY

        LDA #$9B                ; PREPARES SCREEN FOR CHARACTER
        STA $D011
        LDA #$C8
        STA $D016                
        LDA #$1F 
        STA $D018
        LDA #$17
        STA $DD00         

        JSR STOP_MUSIC

        JMP MAINLOOP

;DRAW THE PLAYFIELD ON THE SCREEN-----------------------------------------------
DRAW_PLAY_SCREEN
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

        LDX #$00                ; LOAD STARTING POSITIONS
L1      LDA POSTABLE,X          ; FROM POSTABLE AND STORE IT
        STA SPRITEPOS,X         ; TO SPRITE POSITIONS TABLE
        STA $D000,X
        INX
        CPX #$0E                ; 7 SPTIRES X DIRECTIONS = 14
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

BUL     LDA #$01                ; 1 = UP LEFT
        STA BALLDIRECTION
        RTS

BDL     LDA #$02                ; 2 = DOWN LEFT
        STA BALLDIRECTION
        RTS

BUR     LDA #$03                ; 3 = UP RIGHT
        STA BALLDIRECTION
        RTS
        
BDR     LDA #$04                ; 4 = DOWN RIGHT
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
        
; -----------------------------> THIS DELAY IS FOR NORMAL SPEED
                                ; BETWEEN 90-60 SECONDS
DELAY_NORMAL                
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


;-------------------------------> THIS DELAY IS FOR FASTER SPEED 
                                ; BETWEEN 59-9 SECONDS
                                ; THIS PART USES THE ZERO PAGE FOR A FASTER STORAGE
                                ; OTHERWISE IT IS THE SAME ROUTINE WITH THE NORMAL SPEED

DELAY_FASTER                    
        LDA $D850               ; CHECK IF THE FIRST CHARACTER OF THE LINE IS YELLOW
        CMP #$07                ; IF IT IS ALREADY YELLOW        
        BEQ FASTER              ; NO NEED TO DRAW YELLOW LINES AND GO FASTER ROUTINE        

        LDX #$00
YELLOW  LDA #$07                ; DRAW THE YELLOW LINES
        STA $D850,X             ; UPPER LINE
        STA $DB98,X             ; LOWER LINE
        INX
        CPX #$28                ; 40 CHARACTERS
        BNE YELLOW

FASTER
         LDA #$00               ; DELAY ROUTINE
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

; ------------------------------> THIS DELAY IS FOR FASTEST SPEED 
                                 ; SPEED FOR THE LAST 9 SECONDS
DELAY_FASTEST                               
        LDA $D850               ; CHECK IF THE FIRST CHARACTER OF THE LINE IS RED
        CMP #$02                ; IF IT IS ALREADY RED
        BEQ FASTEST             ; NO NEED TO DRAW RED LINES AND GO TO FASTEST ROUTINE

        LDX #$00                
RED     LDA #$02                ; DRAW THE RED LINE
        STA $D850,X             ; UPPER LINE
        STA $DB98,X             ; LOWER LINE
        INX
        CPX #$28                ; 40 CHARACTERS
        BNE RED

FASTEST
         LDA #$00               ; DELAY ROUTINE
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
        ORA $D010               ; SINCE IT IS ALREADY AT FAR RIGHT
        STA $D010

        LDA SPRITEPOS+2         ; PLAYER 2 IS IN THE LEFT (STANDARD)
        STA $D002               ; SO NO NEED TO SET MSB
        LDA SPRITEPOS+3
        STA $D003

        LDA SPRITEPOS+4         ; BALL SPRITE
        STA $D004
        LDA SPRITEPOS+5
        STA $D005
        
        LDX BALLDIRECTION       ; IS THE BALL IS UP RIGHT?
URIGHT  CPX #$03                
        BNE DRIGHT              ; IF NOT, CONTINUE
        LDA SPRITEPOS+4         
        CMP #$00                ; IS THE BALL SPRITE X POSITION ZERO?
        BNE ENDPLACE            ; NO, CONTINUE
        LDA #%00000100          ; YES, SET THE MSB FOR THE BALL
        ORA $D010
        STA $D010
        RTS
        
DRIGHT  CPX #$04                ; IS THE BALL IS DOWN RIGHT?
        BNE DLEFT               ; IF NOT, CONTINUE
        LDA SPRITEPOS+4         
        CMP #$00                ; IS THE BALL SPRITE X POSITION ZERO? 
        BNE ENDPLACE            ; NO, GO TO END ROUTINE
        LDA #%00000100          ; YES, SET THE MSB FOR THE BALL
        ORA $D010
        STA $D010
        RTS

DLEFT   CPX #$02                ; IS THE BALL IS DOWN LEFT?     
        BNE ULEFT               ; IF NOT, CONTINUE
        LDA SPRITEPOS+4
        CMP #$FF                ; IS THE BALL SPRITE X POSITION 256?
        BNE ENDPLACE            ; NO, GO TO END ROUTINE
        LDA #%11111001          ; YES, CLEAR THE MSB
        AND $D010
        STA $D010
        RTS

ULEFT   LDA SPRITEPOS+4         ; THE BALL IS UP LEFT
        CMP #$FF                ; IS THE BALL SPRITE X POSITION 256?
        BNE ENDPLACE            ; NO, GO TO END ROUTINE 
        LDA #%11111001          ; YES, CLEAR THE MSB
        AND $D010
        STA $D010        

ENDPLACE
        RTS

;WAIT FOR A BUTTON PRESS------------------------------------------------------------
WAIT_BUTTON
        LDA SELECTION           ; LOAD CONTROLLER SELECTION VARIABLE
        CMP #$00                ; IS IT PADDLE?
        BEQ PADDLE_BUTTON       ; YES, GO AND WAIT FOR PADDLE BUTTON
        CMP #$01
        BEQ JOYSTICK_BUTTON                 
        JMP WAIT_BUTTON

JOYSTICK_BUTTON                 ; JOYSTICK IS SELECTED
        LDA $DC00               ; SO WAIT FOR JOYSTICK 2 BUTTON 
        LSR
        LSR 
        LSR
        LSR
        LSR
        BCC EXITB
        LDA $DC01               ; WAIT FOR JOYSTICK 1 BUTTON
        LSR
        LSR
        LSR
        LSR
        LSR
        BCC EXITB
        JSR COLORWASH           ; OTHERWISE, COLORWASH THE TEXT ON THE SCREEN      

RLOOP1  LDA $D012               ; SHORT DELAY ->WAIT RASTER
        BNE RLOOP1
        JMP JOYSTICK_BUTTON     ; GO BACK AND WAIT PADDLE BUTTON AGAIN

PADDLE_BUTTON
        LDA $DC01              
        LSR 
        LSR 
        LSR                     ; CHECK PADDLE 1 BUTTON                 
        BCC EXITB               ; IF PRESSED, EXIT WAITING BUTTON
        LSR                     ; CHECK PADDLE 2 BUTTON
        BCC EXITB               ; IF PRESSED, EXIT WAITING BUTTON
        JSR COLORWASH           ; OTHERWISE, COLORWASH THE TEXT ON THE SCREEN      

RLOOP2  LDA $D012               ; SHORT DELAY ->WAIT RASTER
        BNE RLOOP2
        JMP PADDLE_BUTTON       ; GO BACK AND WAIT PADDLE BUTTON AGAIN

EXITB   LDA #$20                ; CLEAR TEXT ON THE MIDDLE OF THE SCREEN
        LDY #$00
CL1     STA $0478,Y
        STA $0577,Y
        STA $0677,Y
        INY
        CPY #$FF
        BNE CL1
        RTS

;COLOR WAHS---------------------------------------------------------------------
COLORWASH              
        LDA COLOR+$00           ; THIS PART COLOR CYCLES THE TEXT ON SCREEN
        STA COLOR+$28           ; COLOR CODES ARE TAKEN FROM THE TABLE DOWN BELOW
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
START_TIMER_CLOCK           ; THIS PART IS THE 90SEC TIMER
        
        LDA #%00000111      ; ENABLE SPRITES
        STA $D015

        LDA #$7F
        STA $DD0D           ; DISABLE ALL CIA2 NMIs
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
        LDA #%00010001      ; START TIMER A COUNTING CYCLES
        STA $DD0E           
        LDA #%01010001      ; START TIMER B COUNTING T.A. UNDERFLOWS
        STA $DD0F           
        LDA #%10000010      ; ENABLE NMI ON T.B UNDERFLOWS
        STA $DD0D           
        RTS

NMI       
        PHA                  ; PRESERVE REGISTER BY PUSHING THEM TO THE STACK
        TXA
        PHA
        TYA
        PHA

        LDA $DD0D
        BPL DONE            ; IF NMI IS NOT FROM CIA2
        JSR WRITE_COUNTER   ; GO AND WRITE THE COUNTER ON SCREEN

DONE    PLA                 ; PULL THE REGISTERS BACK FROM THE STACK
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
        BEQ GAME_OVER           ; IF YES, THEN END THE GAME
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

        LDX #$00                ; WRITE THE TEXT ON THE SCREEN
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
                                ; THIS PART INITIALLY CHECKS THE LEFT DIGITS, IT IS EASIER
        LDA $0435               ; CHECK THE SCORE BY FIRST COMPARING THE LEFT DIGITS  
        CMP $0441               ; IF LEFT DIGIT OF PLAYER 2<PLAYER 1       
        BCC PLAYER1WINS_MESSAGE ; THEN PLAYER WINS
        LDA $0441               
        CMP $0435               ; LEFT DIGIT OF PLAYER 1<PLAYER 2
        BCC PLAYER2WINS_MESSAGE ; THEN PLAYER 2 WINS

                                ; IF WE ARE HERE, SO FIRST DIGITS ARE EQUAL
                                ; SO GO AND CHECK THE RIGHT DIGITS
        LDA $0436               ; CHECK THE SCORE BY COMPARING THE RIGHT DIGITS   
        CMP $0442               ; IF RIGHT DIGIT OF PLAYER2<PLAYER1 
        BCC PLAYER1WINS_MESSAGE ; PLAYER 1 WINS
        LDA $0442
        CMP $0436               ; IF RIGHT DIGIT OF PLAYER1<PLAYER2 
        BCC PLAYER2WINS_MESSAGE ; PLAYER 2 WINS          

        JSR DRAW                ; OTHERWISE, THE SCORES ARE EQUAL AND IT IS A DRAW       

FINALIZE_MATCH
        JSR WAIT_BUTTON         ; WAIT FOR PADDLE BUTTONS
        JMP MAINLOOP            ; JUMP TO MAIN LOOP

PLAYER1WINS_MESSAGE             ; THIS PART WRITES THE WINNIG TEXT ON SCREEN
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
        JSR BALL_DIRECTION_AT_START     ; AFTER A GOAL, RESET THE BALL POSITION             
        LDA POSTABLE+4                  ; IN ACCORDANCE WITH THE STARTING TABLE
        STA SPRITEPOS+4
        STA $D004
        LDA POSTABLE+5
        STA SPRITEPOS+5
        STA $D005
        LDA #%00000001                  ; CLEAR THE MSB
        STA $D010                       ; BECAUSE THE BALL WILL BE STARTING 
        RTS                             ; IN THE MIDDLE (NO NEED)

;MAINLOOP-----------------------------------------------------------------------
MAINLOOP
        JSR DRAW_PLAY_SCREEN            ; DRAWS THE PRE-DRAWN TEXT SCREEN
        JSR SETUP_SPRITES               ; SETS UP THE SPRITES
        JSR BALL_DIRECTION_AT_START     ; GET THE RANDOMIZED DIRECTION
        JSR SELECT_CONTROL              ; SELECT PADDLE OR JOYSTICK
        JSR WAIT_BUTTON                 ; WAIT FOR A PADDLE BUTTON TO BE PRESSED
        JSR START_TIMER_CLOCK           ; START THE COUNTDOWN CLOCK
LOOPO
        JSR SPEED_AND_DELAY             ; SET THE SPEED (IN ACCORDANCE WITH THE TIME)
        JSR PLACESPRITES                ; DRAW THE SPRITES ON SCREEN
        JSR CHECK_CONTROLS_P1           ; CHECK PLAYER 1 CONTROL AND STORE THE COORDINATES
        JSR CHECK_CONTROLS_P2           ; CHECK PLAYER 2 CONTROL AND STORE THE COORDINATES
        JSR CHECK_PLAYER_COLLISION      ; CHECK IF THE BALL HIT ANY OF THE PLAYERS
        JSR CHECK_TOP_COLLISION         ; CHECK IF THE BALL IS AT THE TOP AND DIVERT IF NECESSARY
        JSR CHECK_BOTTOM_COLLISION      ; CHECK IF THE BALL IS AT THE BOTTOM AND DIVERT IF NECESSARY
        JSR MOVE_BALL                   ; MOVE THE BALL ACCORDING TO THE EXISTING OR NEW DIRECTION
        JSR UPDATE_PLAYER1_SCORE        ; CHECK IF THE PLAYER 1 HAS SCORED THE GOAL AND UPDATE THE SCORE
        JSR UPDATE_PLAYER2_SCORE        ; CHECK IF THE PLAYER 2 HAS SCORED THE GOAL AND UPDATE THE SCORE
        JMP LOOPO
--------------------------------------------------------------------------------

; CHECK FOR PLAYER 1 PADDLE IN PORT 1-------------------------------------------
CHECK_CONTROLS_P1
        LDA SELECTION
        CMP #$00                
        BEQ PEDAL1              ; GO TO PEDAL CONTROLS                      
        JMP J1P1                ; OTHERWISE GO TO JOYSTICK CONTROL

PEDAL1  LDA #%11000000          ; SELECT PINS 6-7 FOR ($C0=192)
        STA $DC02               ; DATA PORT B  ->DISABLE KEYBOARD?
        LDA $DC00               ; Control-Port 1 selected (when bit6=1 and bit7=0)
        AND #%00111111          ; clear bit-6/7        
        ORA #%01000000          ; set bit-6
        STA $DC00               ; now control-port 1 is selected for reading the POT registers

        LDX $D419               ; GET THE PLAYER 1 COORDINATES

                                ; THIS PART OF THE ROUTINE IS COMMON
P1J     STX SPRITEPOS+1         ; STORE IT ON THE POSITION -> JOYSTICK ROUTINE JOINS FROM HERE
        LDX SPRITEPOS+1
        CPX #$42                ; CHECK IF IT IS ALREADY ON THE TOP 
        BCS SETUP1              ; IF NOT, GO AND CHECK IF IT IS AT THE BOTTOM
        LDX #$42                ; YES, IT IS AT THE TOP SO LOCK IT

SETUP1  CPX #$C8                ; IS IT AT THE BOTTOM?
        BCC SETUP2              ; NO, GO AND STORE IT TO THE TABLE
        LDX #$C8                ; YES, IT IS AT THE BOTTOM SO LOCK IT

SETUP2  STX SPRITEPOS+1

        LDA TEMPBUFFER          ; LOAD THE INITIAL VALUE FROM BUFFER
        STA $DC02               ; AND STORE IT TO PORT DIRECTION
        RTS

; CHECK FOR PLAYER 2 PADDLE IN PORT 1-------------------------------------------
CHECK_CONTROLS_P2
        LDA SELECTION
        CMP #$00
        BEQ PEDAL2
        JMP J2P2
PEDAL2  LDA #%11000000          ; SELECT PINS 6-7 FOR 
        STA $DC02               ; DATA PORT B  ->DISABLE KEYBOARD?
        LDA $DC00               ; Control-Port 1 selected (when bit6=1 and bit7=0)
        AND #%00111111          ; clear bit-6/7        
        ORA #%01000000          ; set bit-6
        STA $DC00               ; now control-port 1 is selected for reading the POT registers

        LDY $D41A               ; GET THE PLAYER 2 COORDINATES
P2J     STY SPRITEPOS+3         ; STORE IT ON THE POSITION -> JOYSTICK ROUTINE JOINS FROM HERE
        LDY SPRITEPOS+3
        CPY #$42                ; CHECK IF IT IS ALREADY ON THE TOP 
        BCS SETUP3              ; IF NOT, GO AND CHECK IF IT IS AT THE BOTTOM
        LDY #$42                ; YES, IT IS AT THE TOP SO LOCK IT
        
SETUP3  CPY #$C8                ; IS IT AT THE BOTTOM?
        BCC SETUP4              ; NO, GO AND STORE IT TO THE TABLE
        LDY #$C8                ; YES, IT IS AT THE BOTTOM SO LOCK IT

SETUP4  STY SPRITEPOS+3

        LDA TEMPBUFFER          ; LOAD THE INITIAL VALUE FROM BUFFER
        STA $DC02               ; AND STORE IT TO PORT DIRECTION
        RTS

;CHECK FOR BALL VS PLAYER COLLISION------------------------------------
CHECK_PLAYER_COLLISION 

        LDA $D01E               ; CHECK THE HARDWARE COLLISON REGISTER
        CMP #%00000101          ; COMPARE OT FOR PLAYER 1 VS BALL
        BEQ COLLIDED1           ; IF SET, BALL AND PLAYER 1 COLLIDED
        CMP #%00000110          ; COMPARE OT FOR PLAYER 2 VS BALL
        BEQ COLLIDED2           ; IF SET, BALL AND PLAYER 2 COLLIDED
        RTS                     ; RETURN IF NO COLLISION HAPPENED

COLLIDED1
        JSR PLAYERHITSOUND      ; PLAY PLAYER-BALL COLLISION SOUND EFFECT
        LDA BALLDIRECTION       ; CHECK THE BALL DIRECTION
        CMP #$03                ; IF IT IS UP RIGHT
        BNE OTHER1
        LDA #$01                ; THEN MAKE IT UP LEFT (DEFLECT)
        STA BALLDIRECTION
        RTS

OTHER1  LDA BALLDIRECTION       
        CMP #$04                ; IF IT IS DOWN RIGHT
        BNE EGZIT1
        LDA #$02                ; THEN MAKE IT DOWN LEFT (DEFLECT)
        STA BALLDIRECTION
EGZIT1  RTS

COLLIDED2
        JSR PLAYERHITSOUND      ; PLAY PLAYER-BALL COLLISION SOUND EFFECT
        LDA BALLDIRECTION       ; CHECK THE BALL DIRECTION
        CMP #$02                ; IF IT IS DOWN LEFT
        BNE OTHER2
        LDA #$04                ; THEN MAKE IT DOWN RIGHT (DEFLECT)
        STA BALLDIRECTION
        RTS

OTHER2  LDA BALLDIRECTION
        CMP #$01                ; IF IT IS UP LEFT
        BNE EGZIT2
        LDA #$03                ; THEN MAKE IT UP RIGHT
        STA BALLDIRECTION
EGZIT2  RTS


;DIRECT BALL------------------------------------------------------------
MOVE_BALL
        LDA BALLDIRECTION       ; LOAD THE BALL DIRECTION VARIABLE
        CMP #$01                ; IF 1, THEN MOVE IT UP LEFT
        BEQ BALL_UP_LEFT
        CMP #$02                ; IF 2, THEN MOVE IT DOWN LEFT
        BEQ BALL_DOWN_LEFT
        CMP #$03                ; IF 3, THEN MOVE IT UP RIGHT
        BEQ BALL_UP_RIGHT
        CMP #$04                ; IF 4, THEN MOVE IT DOWN RIGHT
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
                                ; HERE, WE FIRST CHECK THE MSB.
                                ; IF MSB=1, THEN ONLY PLAYER 1 IS AT RIGHT, BALL IS ON THE LEFT SCREEN
                                
        LDA $D010
        CMP #$01                ; IF MSB=1
        BNE SCEXIT1     
        LDA SPRITEPOS+04        ; CHECK THE BALL X POSITION
        CMP #$05                ; IF IT IS 5
        BEQ PLAYER1_SCORES      ; THEN IT HAS REACHED THE RIGHT LIMIT AND IT'S GOAL
SCEXIT1 RTS

PLAYER1_SCORES
        INC $0442               ; INCREMENT THE RIGHT DIGIT OF THE SCORE
        LDA $0442
        CMP #$3A                ; IS THE RIGHT DIGIT > 9 ?
        BNE DONE1               ; NO, CONTINUE
        LDA #$30                ; YES, RESET THE RIGHT DIGIT TO 0
        STA $0442
        INC $0441               ; INCREMENT THE LEFT DIGIT 
                                ; PLEASE NOTE THAT, IT IS NOT POSSIBLE TO SCORE MORE THAN 90
                                ; SO NO NEED TO CEHCK THE OVERFLOW OF THE LEFT DIGIT, IT IS WHAT IT IS


DONE1   JSR SCORESOUND          ; PLAY THE CROWD CHEER SOUND EFFCT 
        JSR FLASHBORDER_BLUE    ; FLASH THE BORDER BLUE FOR PLAYER 1
        JSR RESET_BALL_POSITION ; RESET THE BALL POSITION
        RTS

FLASHBORDER_BLUE
        LDA #%001111111         ; ENABLE SPRITES 0 TO 7
        STA $D015               ; TO SHOW THE GOAL SPRITES
        LDA #$0E                ; MAKE THE BORDER BLUE
        STA $D020

        LDX #$00                ; THIS PART IS FOR FLASHING THE GOAL SPRITES
FLB2    LDY #$00                ; BY CHANGING THE COLORS VERY FAST
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

        LDA #$00                ; MAKE THE BORDER BLACK AGAIN
        STA $D020
        LDA #%000000111         ; TURN OFF THE GOAL SPRITES (PLAYERS AND THE BALL ARE ON)
        STA $D015   
        RTS

;CHECK IF PLAYER 2 SCORES-------------------------------------------------------
UPDATE_PLAYER2_SCORE
                                ; HERE, WE FIRST CHECK THE MSB.
                                ; IF MSB=5, THEN PLAYER 1 AND THE BALL IS ON THE RIGHT

        LDA $D010
        CMP #$05
        BNE SCEXIT2
        LDA SPRITEPOS+04        ; CHECK THE BALL X POSITION
        CMP #$50                ; IF IT IS 50
        BEQ PLAYER2_SCORES      ; THEN IT HAS REACHED THE LEFT LIMIT AND IT'S GOAL
SCEXIT2 RTS

PLAYER2_SCORES
        INC $0436               ; INCREMENT THE RIGHT DIGIT OF THE SCORE
        LDA $0436
        CMP #$3A                ; IS THE RIGHT DIGIT > 9 ?
        BNE DONE2               ; NO, CONTINUE
        LDA #$30                ; YES, RESET THE RIGHT DIGIT TO 0
        STA $0436
        INC $0435               ; INCREMENT THE LEFT DIGIT 
                                ; PLEASE NOTE THAT, IT IS NOT POSSIBLE TO SCORE MORE THAN 90
                                ; SO NO NEED TO CEHCK THE OVERFLOW OF THE LEFT DIGIT, IT IS WHAT IT IS

DONE2   JSR SCORESOUND          ; PLAY THE CROWD CHEER SOUND EFFCT
        JSR FLASHBORDER_RED     ; FLASH THE BORDER RED FOR PLAYER 2
        JSR RESET_BALL_POSITION ; RESET THE BALL POSITION
        RTS

FLASHBORDER_RED
        LDA #%001111111
        STA $D015               ; ENABLE SPRITES 0 TO 7
        LDA #$02                ; TO SHOW THE GOAL SPRITES
        STA $D020               ; MAKE THE BORDER RED

        LDX #$00                ; THIS PART IS FOR FLASHING THE GOAL SPRITES
FLR2    LDY #$00                ; BY CHANGING THE COLORS VERY FAST
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

        LDA #$00                ; MAKE THE BORDER BLACK AGAIN
        STA $D020
        LDA #%000000111         ; TURN OFF THE GOAL SPRITES (PLAYERS AND THE BALL ARE ON)
        STA $D015   
        RTS

;CHECK IF THE BALL IS AT THE BOTTOM OF THE FIELD-----------------------------
CHECK_BOTTOM_COLLISION
        LDA SPRITEPOS+5         ; CHECK THE BALL'S Y POSITION
        CMP #$DB                ; DID IT REACH BOTTOM?
        BNE CBCEND              ; NO, CONTINUE
        LDA BALLDIRECTION       ; YES, CHECK THE DIRECTION
        CMP #$02                ; IS IT DOWN LEFT?
        BNE CHBC                ; NO, CONTINUE
        LDA #$01                ; YES, MAKE IT UP LEFT (DEFLECT)
        STA BALLDIRECTION
        JSR BORDERHITSOUND      ; PLAY THE BORDER HIT SOUND EFFECT
        RTS

CHBC    LDA #$03                ; IT WAS DOWN LEFT, SO MAKE IT UP RIGHT (DEFLECT)
        STA BALLDIRECTION
        JSR BORDERHITSOUND       ; PLAY THE BORDER HIT SOUND EFFECT 

CBCEND  RTS

;CHECK IF THE BALL IS AT THE TOP OF THE FIELD-----------------------------
CHECK_TOP_COLLISION
        LDA SPRITEPOS+5         ; CHECK THE BALL'S Y POSITION
        CMP #$41                ; DID IT REACH TOP?
        BNE CTCEND              ; NO, CONTINUE
        LDA BALLDIRECTION       ; YES, CHECK THE DIRECTION
        CMP #$01                ; IS IT UP LEFT?   
        BNE CTC                 ; NO, CONTINUE
        LDA #$02                ; YES, MAKE IT DOWN LEFT (DEFLECT)
        STA BALLDIRECTION
        JSR BORDERHITSOUND      ; PLAY THE BORDER HIT SOUND EFFECT
        RTS

CTC     LDA #$04                ; IT WAS UP RIGHT, SO MAKE IT DOWN RIGHT (DEFLECT)
        STA BALLDIRECTION
        JSR BORDERHITSOUND      ; PLAY THE BORDER HIT SOUND EFFECT

CTCEND  RTS

;PLAY THE SOUND WHEN BALL HITS TOP AND BOTTOM BORDERS---------------------------
BORDERHITSOUND
;CLEAN SID                      ; VOICE 1 SETTINGS
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
;CLEAN SID                      ; VOICE 2 SETTINGS
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
        LDA  #00                 ; VOICE 3 SETTINGS      
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

; PLAY INTRO MUSIC--------------------------------------------------------------

PLAY_MUSIC
        SEI             ; THIS ROUTINE PLAYS THE SOUND OF THE MUSIC.BIN FILE
        LDA #<IRQ       ; IT IS DRIVEN BY THE IRQ
        LDX #>IRQ
        STA $314
        STX $315
        LDY #$7F 
        STY $DC0D
        LDA #$01
        STA $D01A
        STA $D019 
        LDA MUSIC
        JSR $2200       ; PLAYER ROUTINE INSIDE THE MUSIC FILE
        CLI
        RTS

IRQ     INC $D019
        JSR $2203
        JMP $EA31

; STOP INTRO MUSIC--------------------------------------------------------------

STOP_MUSIC                
        SEI             ; THIS PART CLEARS THE MUSIC INTERRUPT AND STOPS IT
        LDA #$31
        STA $314
        LDA #$EA
        STA $315
        LDA #$81
        STA $DC0D
        LDA #$00
        STA $D01A
        INC $D019
        LDA $DC0D
        JSR $2200
        CLI
        RTS

; MAKE PADDLE OR JOYSTICK SELECTION---------------------------------------------
SELECT_CONTROL
        JSR $FF9F
        JSR $FFE4              ; WAITS FOR KEY PRESS
        STA CONTROLKEY
        BEQ SELECT_CONTROL
        CMP #74                ; COMPARES IT TO 'J' ($4A)
        BEQ JOYSTICK    
        CMP #80                ; COMPARES IT TO 'P' ($50) 
        BEQ PADDLE
        JMP SELECT_CONTROL

JOYSTICK
        LDA #$01                ; JOYSTICK =1
        STA SELECTION
        LDX #$00
JTEXT   LDA JOYSTICK_SELECTED,X
        STA $0663,X
        INX
        CPX #$11
        BNE JTEXT
        JMP WRITE_BUTTON_TEXT

PADDLE
        LDA #$00                 ; PADDLE = 0
        STA SELECTION           
        LDX #$00
PDTEXT  LDA PADDLE_SELECTED,X
        STA $0663,X
        INX
        CPX #$11
        BNE PDTEXT

WRITE_BUTTON_TEXT
        LDX #$00
WBT     LDA PRESS_A_BUTTON,X
        STA $06AF,X
        INX
        CPX #$1A
        BNE WBT
        JMP WAIT_BUTTON

; JOYSTICK CONTROL ROUTINE FOR PLAYER 1-----------------------------------------
J1P1    LDA #$FF
        STA $DC00
        STA $DC02
        LDA #$01                ; CHECK JOYSTICK PORT 1 FOR PLAYER 1
        BIT $DC01
        BNE JOY
        LDX SPRITEPOS+1        ; PLAYER 1 Y COORDINATE
        DEX                     ; MOVE UP (2 PIXELS) 
        DEX
        JMP P1J                ; CONTINUE CHECKING P1 POSITION

JOY     LDA #$02
        BIT $DC01
        BNE JEXIT1
        LDX SPRITEPOS+1
        INX
        INX
        JMP P1J
JEXIT1  RTS


; IF JOYSTICK SELECTED, CHECK PLAYER CONTROL 2----------------------------------
J2P2    LDA #$7F
        STA $DC00
        STA $DC02
        LDA #$01
        BIT $DC00
        BNE JOY2
        LDY SPRITEPOS+3
        DEY
        DEY
        JMP P2J
        
JOY2    LDA #$02
        BIT $DC00
        BNE JEXIT2
        LDY SPRITEPOS+3
        INY
        INY
        JMP P2J
JEXIT2  RTS


;-------VARIABLES--------------------------------------------------------

SPRITEPOS  = $0370                      ; POSITIONS FOR THE SPRITES

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
KEY BYTE 0                                  ; GET CHAR. OF KEY PRESSED
CONTROLKEY BYTE 0                           ; PADDLE OR JOYSTICK SELECTION VARIABLE
SELECTION BYTE 0                            ; 0=PADDLE, 1=JOYSTICK
MUSIC BYTE 0

PRINT_GAME_OVER         text    'game  over'
PLAYER_1_WINS           text 'player 1 wins !'
PLAYER_2_WINS           text 'player 2 wins !'
DRAW_MESSAGE            text 'it is a draw ! '
JOYSTICK_SELECTED       text 'joystick selected'
PADDLE_SELECTED         text  ' paddle selected '
PRESS_A_BUTTON          text  'press a button to continue'

START_GAME_MESSAGE1 text '   press a button   '
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
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 byte $00,$2A,$00,$00,$2A,$00,$00,$2A,$00,$00,$2A,$00,$00,$2A,$00,$00
 byte $2A,$00,$00,$2A,$00,$00,$2A,$00,$00,$2A,$00,$00,$2A,$00,$00,$2A
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;SPRITE 1 DATA ; PLAYER 2 - LEFT PLAYER --> POINTER $81
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$A8
 byte $00,$00,$A8,$00,$00,$A8,$00,$00,$A8,$00,$00,$A8,$00,$00,$A8,$00
 byte $00,$A8,$00,$00,$A8,$00,$00,$A8,$00,$00,$A8,$00,$00,$A8,$00,$00
 byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

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


; MUSIC FILE----------------------------------------------------------------
*=$2200
incbin "donence.bin",2

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
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$17,$05,$0C,$03,$0F,$0D,$05,$20,$14,$0F,$20,$6D,$6E,$6F,$70,$71,$72,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$10,$0C,$05,$01,$13,$05,$20,$03,$0F,$0E,$0E,$05,$03,$14,$20,$19,$0F,$15,$12,$20,$03,$0F,$0E,$14,$12,$0F,$0C,$0C,$05,$12,$13,$3A,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$10,$01,$04,$04,$0C,$05,$20,$13,$05,$14,$20,$2D,$3E,$20,$10,$0F,$12,$14,$20,$31,$20,$0F,$12,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$0A,$0F,$19,$13,$14,$09,$03,$0B,$13,$20,$20,$2D,$3E,$20,$10,$0F,$12,$14,$20,$31,$20,$26,$20,$32,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$10,$0C,$05,$01,$13,$05,$20,$0D,$01,$0B,$05,$20,$19,$0F,$15,$12,$20,$13,$05,$0C,$05,$03,$14,$09,$0F,$0E,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$28,$10,$29,$20,$10,$01,$04,$04,$0C,$05,$20,$20,$20,$20,$28,$0A,$29,$20,$0A,$0F,$19,$13,$14,$09,$03,$0B,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$02,$05,$17,$01,$12,$05,$21,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$14,$08,$05,$20,$13,$10,$05,$05,$04,$20,$17,$09,$0C,$0C,$20,$07,$05,$14,$20,$06,$01,$13,$14,$05,$12,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$01,$0E,$04,$20,$0D,$0F,$12,$05,$20,$03,$08,$01,$0C,$0C,$05,$0E,$07,$09,$0E,$07,$20,$21,$20,$20,$20,$20,$20,$20,$20,$20
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
        BYTE    $05,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $05,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
        BYTE    $05,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$05
        BYTE    $0F,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$05,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00
        BYTE    $00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00
        BYTE    $00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$01,$01,$01,$00,$00,$00,$00
        BYTE    $00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$0F
        BYTE    $00,$00,$00,$00,$00,$01,$01,$01,$01,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05
        BYTE    $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
        BYTE    $05,$01,$05,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01

*=$6000                          ; BITMAP KOALA IMAGE
incbin "pongy61.prg",2

