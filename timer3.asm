; 10 SYS (2080)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $38, $30, $29, $00, $00, $00


*=$0820

        JSR $E544  
        LDA #$30
        STA $043C
        LDA #$39
        STA $043B

GETK1   JSR $FF9F               ; SCAN KEYBOARD
        JSR $FFE4               ; GET A CHARACTER
        CMP #$00        
        BEQ GETK1


        SEI                     ; TURN OFF INTERRUPTS
        
        LDA #%01010010
        STA $DC0F               ; CIA#1 TIMER B - CONTROL REGISTER B

;BITS 5 AND 6
;00: Watch the system cycle
;01: Watch if the user port’s CNT pin received a positive value
;10: Watch if Timer A underflowed
;11: Watch both if CNT pin is positive and Timer A underflowed

;COUNTER WILL BE COUNTING BACK FROM 255

; set the starting values of Timer A and Timer B
; TIMER A WILL HAVE THE VALU $FC $03 - 64.515
        LDA #$FC
        STA $DC04
        LDA #$03
        STA $DC05

; set Timer B’s value to $FF $FF
        LDA #$FF
        STA $DC06
        STA $DC07

; UPTO HERE, WE STARTED TIMER B BUT IT DOES NOTHİNG
; Timer A is not running, and therefore it never generates the event which Timer B is waiting to happen

; Now we need to tell what should the C-64 do when the interrupt event occurs
; we put the low and high byte of this address into memory locations $0314 and $0315 as vectors        
        LDA #<TIMER2
        STA $0314
        LDA #>TIMER2
        STA $0315

; Nowe we start Timer A by setting 1 to the 0th bit of its control register
; and also to the 4th bit to load the default value

        LDA #%00010001
        STA $DC0E

        LDA #$01
        STA $D019
        STA $D01A

; And let’s finish this code block with a jump to nowhere

        CLI             ; CLear Interrupts
        RTS


TIMER2
        LDA #$01
        STA $D019
        ;INC $D020

T1      JSR DELAY2
        DEC $043C               ; DECREMENT RIGHT DIGIT
        LDA $043C
        CMP #$2F                ; DID WE REACH BELOW ZERO?
        BNE T1               ; NO, CONTINUE PROCESS
        LDA #$39                ; YES, ZERO THE RIGHT DIGIT
        STA $043C               ; AND DECREMENT LEFT DIGIT
        DEC $043B
        LDA $043B               ; DID WE REACH BELOW ZERO?
        ;CMP #$2F
        CMP #$37
        BEQ FINISH
        JMP $EA31

FINISH
        LDX #$00
WAIT    LDA FINISH_TEXT,X
        STA $0400,X
        INX
        CPX #$09
        BNE WAIT 
        RTS           


DELAY
        LDA #$00
DLOOP   CMP $D012
        BNE DLOOP 
        RTS        

DELAY2
         LDA #$00
         STA $C820
         STA $C821
@WAIT11  INC $C821
@WAIT10  INC $C820
         LDA $C820
         BNE @WAIT10
         LDA $C821
         CMP #$FF    ;Adjust this for timing
         BNE @WAIT11
         RTS











TIMER
        LDA #$01
        STA $D019

        ;INC $D020
        
        LDX #$00
LOOP    LDA #$01
        STA $D800,X
        LDA $DC04,X
        STA $0400,X
        CLC
        INX
        CPX #$04
        BNE LOOP

        ;LDA #%01111111  ; we acknowledge the interrupt so the CPU will know it can accept another one
        ;STA $DC0D       ; $DC0D is the interrupt control register, and here we tell it to allow all timer interrupts again
        ;LDA $DC0D       ; This readout is what acknowledges the interrupt and lets the CPU know it can listen to the next one
        JMP $EA31



FINISH_TEXT     text    'finished.'





       