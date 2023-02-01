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

        LDA #%01111000          ; EXPAND SPRITES 
        STA $D017               ; Y DIRECTION   -> EXCEPT THE BALL
        LDA #%01111000
        STA $D01D               ; X DIRECTION -> ONLY GOAL SPRITES
        
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
        

MAINLOOP

        JSR CHECK_CONTROLS_P1
        JSR CHECK_CONTROLS_P2
        JSR PLACESPRITES
        JMP MAINLOOP



CHECK_CONTROLS_P1

        LDX $D419
        STX SPRITEPOS+1
        LDX SPRITEPOS+1
        CPX #$49
        BCS SETUP1
        LDX #$49

SETUP1  CPX #$D5
        BCC SETUP2
        LDX #$D5

SETUP2  STX SPRITEPOS+1
        RTS
       

; CHECK FOR PLAYER 2 JOYSTICK (LATER IT WILL BE PADDLE CONTROL)-----------------
CHECK_CONTROLS_P2

        LDY $D41A 
        STY SPRITEPOS+3
        LDY SPRITEPOS+3
        CPY #$49
        BCS SETUP3
        LDY #$49
        
SETUP3  CPY #$D5
        BCC SETUP4
        LDY #$D5

SETUP4  STY SPRITEPOS+3
        RTS



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



















SPRITEPOS  = $0370                      ; POSITIONS FOR THE SPRITES
SPEED BYTE 00

POSTABLE BYTE $41,$85,$16,$85,$AA,$8D,$40,$85,$7F,$85,$BF,$85,$FF,$85   ; SPRITE INITIAL POSITIONS

BALLDIRECTION BYTE $00                   ; 1=UP&LEFT, 2=DOWN&LEFT, 3=UP&RIGHT, 4=DOWN&RIGHT

PLAYER1_Y = $FB
PLAYER2_Y = $FC
BALL_Y = $FD

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
 byte $00,$00,$6B,$00,$00,$6B,$00,$00,$7F,$00,$00,$36,$00,$00,$1C,$00
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







