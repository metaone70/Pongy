; 10 SYS (2080)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $38, $30, $29, $00, $00, $00

*=$0820
        SEI
        ;LDA #$20               ;WHEN RESTORE KEY IS HIT
        ;STA $0318              ;IT POINTS TO THE START
        ;LDA #$08               ;OF THIS PROGRAM, WHICH IS 
        ;STA $0319              ; $0820
        LDA #$00                ;BLACK COLOR 
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

        LDA #$80                ; SET SPRITE POINTERS --> $2000 (128 x 64)
        STA $07F8               ; RIGHT PLAYER 
        LDA #$81                
        STA $07F9               ; LEFT PLAYER
        LDA #$82                
        STA $07FA               ; BALL

        LDA #%00000011          ; EXPAND SPRITES 0 AND 1 IN Y DIRECTION
        STA $D017

        LDA #%00000111
        STA $D015               ; ENABLE SPRITES 0,1 AND 2

SETSPRITE1POS                   ; PLAYER 1 SPRITE WITH MSB ENABLED
        LDA POSTABLE+1
        STA $D001
        LDA POSTABLE
        STA $D000
        LDA $D010
        ORA #%00000001
        STA $D010

SETSPRITE2POS                   ; PLAYER 2 SPRITE
        LDA POSTABLE+3
        STA $D003
        LDA POSTABLE+2
        STA $D002

SETSPRITE3POS                   ; BALL SPRITE (MES TO BE ENABLED LATER)
        LDA POSTABLE+5
        STA $D005
        LDA POSTABLE+4
        STA $D004
        RTS


; BALL MOVING UP & LEFT---------------------------------------------------
BALL_UP_LEFT
        LDA #$01
        STA BALLDIRECTION               ; BALL DIRECTION IS UP&LEFT
        JSR DELAY_LOOP                  ; SLOW DOWN THE PROCESS
        JSR CHECK_CONTROLS              ; CHECK PLAYER CONTROLS
        JSR CHECK_PLAYER2_COLLISION     ; CHECK WHETHER THE BALL COLLIDED WITH PLAYER 2
        JSR CHECK_TOP_COLLISION         ; CHECK WHETHER THE BALL IS AT THE TOP OF THE FIELD
        DEC SPRITEPOS+4                 ; DECREMENT THE X POS (TO LEFT)
        DEC SPRITEPOS+5                 ; DECREMENT THE Y POS (TO UP)
        JSR UPDATE_BALL_SPRITE_POS      ; WRITE TO THE REGISTERS
        JMP BALL_UP_LEFT                ; CONTINUE GOING UP & LEFT

; BALL MOVING DOWN & LEFT-------------------------------------------------
BALL_DOWN_LEFT
        LDA #$02
        STA BALLDIRECTION               ; BALL DIRECTION IS DOWN&LEFT
        JSR DELAY_LOOP                  ; SLOW DOWN THE PROCESS
        JSR CHECK_CONTROLS              ; CHECK PLAYER CONTROLS
        JSR CHECK_PLAYER2_COLLISION     ; CHECK WHETHER THE BALL COLLIDED WITH PLAYER 2
        JSR CHECK_BOTTOM_COLLISION      ; CHECK WHETHER THE BALL IS AT THE BOTTOM OF THE FIELD
        DEC SPRITEPOS+4                 ; DECREMENT THE X POS (TO LEFT)
        INC SPRITEPOS+5                 ; INCREMENT THE Y POS (TO DOWN)
        JSR UPDATE_BALL_SPRITE_POS      ; WRITE TO THE REGISTERS
        JMP BALL_DOWN_LEFT              ; CONTINUE GOING DOWN & LEFT

;CHECK IF THE BALL IS AT THE BOTTOM OF THE FIELD-----------------------------
CHECK_BOTTOM_COLLISION
        LDA SPRITEPOS+5
        CMP #$E9
        BNE CBCEND
        LDA BALLDIRECTION
        CMP #$02
        BEQ BALL_UP_LEFT
        CMP #$04
        BEQ BALL_UP_RIGHT
CBCEND  RTS

;BRANCH IF PLAYERS HIT THE BALL-------------------------------------------
BALLHITPLAYER2
        LDA BALLDIRECTION
        CMP #$01
        BEQ BALL_UP_RIGHT
        CMP #$02
        BEQ BALL_DOWN_RIGHT

BALLHITPLAYER1
        LDA BALLDIRECTION
        CMP #$03
        BEQ BALL_UP_LEFT
        CMP #$04
        BEQ BALL_DOWN_LEFT


;CHECK FOR BALL VS PLAYER 2 COLLISION------------------------------------
CHECK_PLAYER2_COLLISION 
        LDA $D01E
        CMP #%00000110
        BEQ BALLHITPLAYER2
        RTS

;CHECK FOR BALL VS PLAYER 1 COLLISION------------------------------------
CHECK_PLAYER1_COLLISION 
        LDA $D01E
        CMP #%00000101
        BEQ BALLHITPLAYER1
        RTS


; BALL MOVING UP & RIGHT--------------------------------------------------
BALL_UP_RIGHT
        LDA #$03
        STA BALLDIRECTION               ; BALL DIRECTION IS UP&RIGHT
        JSR DELAY_LOOP                  ; SLOW DOWN THE PROCESS
        JSR CHECK_CONTROLS              ; CHECK PLAYER CONTROLS
        JSR CHECK_PLAYER1_COLLISION     ; CHECK WHETHER THE BALL COLLIDED WITH PLAYER 1
        JSR CHECK_TOP_COLLISION         ; CHECK WHETHER THE BALL IS AT THE TOP OF THE FIELD
        INC SPRITEPOS+4                 ; DECREMENT THE X POS (TO LEFT)
        DEC SPRITEPOS+5                 ; INCREMENT THE Y POS (TO DOWN)
        JSR UPDATE_BALL_SPRITE_POS      ; WRITE TO THE REGISTERS
        JMP BALL_UP_RIGHT               ; CONTINUE GOING DOWN & RIGHT


; BALL MOVING DOWN & RIGHT-------------------------------------------------
BALL_DOWN_RIGHT
        LDA #$04
        STA BALLDIRECTION               ; BALL DIRECTION IS DOWN&RIGHT
        JSR DELAY_LOOP                  ; SLOW DOWN THE PROCESS
        JSR CHECK_CONTROLS              ; CHECK PLAYER CONTROLS
        JSR CHECK_PLAYER1_COLLISION     ; CHECK WHETHER THE BALL COLLIDED WITH PLAYER 1
        JSR CHECK_BOTTOM_COLLISION      ; CHECK WHETHER THE BALL IS AT THE BOTTOM OF THE FIELD        
        INC SPRITEPOS+4                 ; DECREMENT THE X POS (TO LEFT)
        INC SPRITEPOS+5                 ; INCREMENT THE Y POS (TO DOWN)
        JSR UPDATE_BALL_SPRITE_POS      ; WRITE TO THE REGISTERS
        JMP BALL_DOWN_RIGHT             ; CONTINUE GOING DOWN & RIGHT


;BALL DIRECTION AT START----------------------------------------------------

BALL_DIRECTION_AT_START
        LDA #$C7                ; this block gets the SID producing
        STA $D40E               ; random numbers up 200 (0-199)
        STA $D40F               ; because it's divisible by 4 
        LDA #$80        
        STA $D412             

        LDA $D41B
        CMP #$32
        BCC BALL_UP_LEFT
        CMP #$64
        BCC BALL_DOWN_LEFT
        CMP #$96
        BCC BALL_UP_RIGHT
        JMP BALL_DOWN_RIGHT


;CHECK IF THE BALL IS AT THE TOP OF THE FIELD-----------------------------
CHECK_TOP_COLLISION
        LDA SPRITEPOS+5
        CMP #$32
        BNE CTCEND
        LDA BALLDIRECTION
        CMP #$01
        BEQ BALL_DOWN_LEFT
        CMP #$03
        BEQ BALL_DOWN_RIGHT
CTCEND  RTS

;DELAY LOOP--------------------------------------------------------------
DELAY_LOOP
        LDA #$FA
DLOOP   CMP $D012
        BNE DLOOP 
        RTS

;UPDATE BALL SPRITE POSITION--------------------------------------------------
UPDATE_BALL_SPRITE_POS
        LDA SPRITEPOS+4
        STA $D004
        LDA SPRITEPOS+5
        STA $D005
        RTS

;MAINLOOP---------------------------------------------------------------
MAINLOOP
        JSR DRAW_PLAY_SCREEN
        JSR SETUP_SPRITES
        ;JSR EXPANDSPRITE1POS
        JMP BALL_DIRECTION_AT_START
------------------------------------------------------------------------


; CHECK FOR KEYBOARD PRESS (LATER IT WILL BE PADDLE CONTROL)--------------------
CHECK_CONTROLS
        JSR $FFE4
        CMP #87                         ; W (PLAYER 2 UP)
        BEQ PLAYER_2_UP
        CMP #83                         ; S (PLAYER 2 DOWN)
        BEQ PLAYER_2_DOWN
        CMP #80                         ; P (PLAYER 1 UP)
        BEQ PLAYER_1_UP
        CMP #76                         ; L (PLAYER 1 DOWN)
        BEQ PLAYER_1_DOWN
        RTS

;MOVE THE PLAYERS IN ACCORDANCE WITH THE CONTROLS-------------------------------
PLAYER_1_UP
        LDA SPRITEPOS+1        
        CMP #110
        BEQ P1UEND
        DEC SPRITEPOS+1
        LDA SPRITEPOS+1
        STA $D001        
P1UEND  RTS

PLAYER_1_DOWN
        LDA SPRITEPOS+1        
        CMP #220
        BEQ P1DEND
        INC SPRITEPOS+1
        LDA SPRITEPOS+1
        STA $D001        
P1DEND  RTS

PLAYER_2_UP
        LDA SPRITEPOS+3        
        CMP #110
        BEQ P2UEND
        DEC SPRITEPOS+3
        LDA SPRITEPOS+3
        STA $D003       
P2UEND  RTS

PLAYER_2_DOWN
        LDA SPRITEPOS+3        
        CMP #220
        BEQ P2DEND
        INC SPRITEPOS+3
        LDA SPRITEPOS+3
        STA $D003       
P2DEND  RTS
  

;-------VARIABLES--------------------------------------------------------

SPRITEPOS  = $0370                      ; POSITIONS FOR THE SPRITES

POSTABLE BYTE $37,$85,$20,$85,$AC,$90   ; SPRITE INITIAL POSITIONS

BALLDIRECTION   BYTE 00                 ; 1=UP&LEFT, 2=DOWN&LEFT, 3=UP&RIGHT, 4=DOWN&RIGHT


;SPRITE DEFINITIONS---------------------------------------------------------
*=$2000

;SPRITE 0 DATA ; PLAYER 1 - RIGHT PLAYER --> POINTER $80
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$0F,$00,$00,$0F,$00,$00
        BYTE $0F,$00,$00,$0F,$00,$00,$0F,$00,$00,$0F,$00,$00,$0F,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;SPRITE 1 DATA ; PLAYER 2 - LEFT PLAYER --> POINTER $81
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00
        BYTE $00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$F0,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


;SPRITE 2 DATA ; BALL --> POINTER $82
        byte $00,$00,$00,$00,$00,$00,$00,$00
        byte $00,$00,$00,$00,$00,$00,$00,$00
        byte $00,$00,$00,$00,$00,$00,$00,$00
        byte $00,$00,$00,$00,$1C,$00,$00,$3E
        byte $00,$00,$6B,$00,$00,$7F,$00,$00
        byte $7F,$00,$00,$36,$00,$00,$1C,$00
        byte $00,$00,$00,$00,$00,$00,$00,$00
        byte $00,$00,$00,$00,$00,$00,$00,$00


; NEW FONT FILE--------------------------------------------------------------       
*=$3800                          
incbin "zx.bin"

; PLAY SCREEN--------------------------------------------------------------    

;SCREEN DATA
*=$4800
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$13,$03,$0F,$12,$05,$20,$20,$20,$14,$09,$0D,$05,$20,$20,$20,$13,$03,$0F,$12,$05,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $10,$0C,$01,$19,$05,$12,$20,$32,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$10,$0C,$01,$19,$05,$12,$20,$31
        BYTE    $79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79
        BYTE    $75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
        BYTE    $75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
        BYTE    $75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
        BYTE    $75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
        BYTE    $75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
        BYTE    $78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78
        BYTE    $20,$20,$20,$20,$20,$0E,$05,$17,$20,$10,$0F,$0E,$07,$21,$20,$20,$20,$20,$20,$20,$20,$20,$28,$03,$29,$20,$32,$30,$32,$33,$20,$0D,$05,$14,$05,$13,$05,$16,$20,$20

;COLOR DATA
*=$4C00 
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0D,$0D,$0D,$0D,$0D,$00,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$00,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$00,$00,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        BYTE    $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
        BYTE    $05,$00,$05,$00,$00,$00,$00,$00,$05,$05,$05,$05,$05,$05,$05,$00,$00,$00,$00,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$05
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
