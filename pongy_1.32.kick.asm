//pongy! v1.30 (c) metesev 2023
//written between 20jan23-24feb23

//memory map of the game
//$0801 - $0810   program start
//$0820 - $10c9   main program
//$10ca - $1179   variables 
//$117a - $14a7   tables
//$2000 - $21b0   sprite definitions
//$2200 - $2eea   music file (4k)
//$3800 - $4800   font file (4k)
//$4800 - $4be8   screen data (1000 .bytes)     
//$4c00 - $4fe8   screen color (1000 .bytes)
//$6000 - $8711   bitmap file (10k)

//added joystick option
//added serving option (after a goal, the ball starts from either player positions, which is pseudo random)
//added 2 more angles -> upper part 45, middle 15, bottom 30 degrees... 45 is the fastest

.label current_arraylo = $fd        // current anglennn address
.label current_arrayhi = $fe        // current anglennn address
.label angle_pointer   = $fc

*=$0801 "basic upstart"
BasicUpstart($0820)

*=$0820 "main program"
beginning:
        sei
        lda #$20              //when restore key is hit
        sta $0318             //it points to the start
        lda #$08              //of this program, which is 
        sta $0319             // $0820
        lda #$00              //black color 
        sta $d021             //for the border and
        sta $d020             //background

        lda $dc02             //load the port direction 
        sta tempbuffer        // and store it to buffer location

        jsr play_music

//load bitmap screen-------------------------------------------------------------
intro:
        lda #$3b                                // prepares screen for bitmap
        sta $d011
        lda #$18
        sta $d016
        lda #$78        //% 0111 1000
        sta $d018       //% xxxx 1xxx bitmap is at $2000, 
        lda #$c6        //% 1100 0110 
        sta $dd00       //% xxxx 10xx bank1: $4000-$7fff
        lda #$00                                 // black border
        sta $d020
        lda $8710
        sta $d021
 
        ldx #$00
!loop:  lda scrdata,x
        sta scrram,x
        lda scrdata+$100,x
        sta scrram+$100,x
        lda scrdata+$200,x
        sta scrram+$200,x       
        lda scrdata+$2e8,x
        sta scrram+$2e8,x
        lda coldata,x
        sta colram,x
        lda coldata+$100,x
        sta colram+$100,x
        lda coldata+$200,x
        sta colram+$200,x
        lda coldata+$2e8,x
        sta colram+$2e8,x
        inx
        bne !loop-
 
 inputkey:        
        jsr $ff9f                // waits for space key
        jsr $ffe4       
        beq inputkey
        sta key
        cmp #$20                // compares it to space ($20=32)
        bne inputkey

        lda #$9b                // prepares screen for character
        sta $d011
        lda #$c8
        sta $d016                
        lda #$1f                //screen mem is at $0400, 
        sta $d018               //%0001 1111
        lda #$17                
        sta $dd00               //%00010111

        jsr stop_music
        jmp mainloop

//draw the playfield on the screen-----------------------------------------------
draw_play_screen:
        lda #$00
        sta $fb
        sta $fd                 // $0400 screen ram
        sta $f7
        lda #$48                // $4800 character data -->  transfer to $0400
        sta $fc
        lda #$04
        sta $fe
        lda #$00                // $4c00 screen color data --> transfer to $d800
        sta $f9
        lda #$4c
        sta $fa
        lda #$d8                // $d800 --> color ram
        sta $f8
        ldx #$00
looptext:        
        ldy #$00
innerloopy:      
        lda ($fb),y             // looping w/ zero page addresses 
        sta ($fd),y             // for data copying
        lda ($f9),y
        sta ($f7),y
        iny
        bne innerloopy
        inc $fc
        inc $fe
        inc $fa
        inc $f8
        inx
        cpx #$04
        bne looptext
        rts

//setup sprites-----------------------------------------------------------------
setup_sprites:    
        lda #$00                // multicolor register
        sta $d025               // black and light grey
        lda #$0f
        sta $d026
        lda #14
        sta $d027               // light blue color for sprite 0 (right player)
        lda #10
        sta $d028               // light red color for sprite 1 (left player),
        lda #01
        sta $d029               // white color for sprite 2 (ball)
        sta $d02a               // white for goal sprites
        sta $d02b
        sta $d02c
        sta $d02d

        lda #%00000011          // set multicolor bits     
        sta $d01c

        lda #$80                // set sprite pointers --> $2000 (128 x 64)
        sta $07f8               // right player 
        lda #$81                
        sta $07f9               // left player
        lda #$82                
        sta $07fa               // ball
        lda #$83
        sta $07fb               // g 
        lda #$84
        sta $07fc               // o
        lda #$85
        sta $07fd               // a
        lda #$86        
        sta $07fe               // l

        lda #%01111011          // expand sprites 
        sta $d017               // y direction   -> except the ball
        lda #%01111000
        sta $d01d               // x direction -> only goal sprites
        
        lda #%00000000
        sta $d015               // enable sprites 0,1 and 2

        ldx #$00                // load starting positions
l1:     lda postable,x          // from postable and store it
        sta spritepos,x         // to sprite positions table
        sta $d000,x
        inx
        cpx #$0e                // 7 sptires x directions = 14
        bne l1

        lda #%00000001          // enable msb for player 1 
        sta $d010               // (it is already on the far right at the beginning)
        
        rts

//ball direction at start----------------------------------------------------
ball_direction_at_start:
        lda #$c7                // get random numbers
        sta $d40e               // from sid
        sta $d40f               // upto 200
        lda #$80                // 200 is divisable by 4
        sta $d412               // pseudo random number :-/

        lda $d41b               // this block diverts the ball
        cmp #$32                // in accordance with the random number
        bcc bul                 
        cmp #$64
        bcc bdl
        cmp #$96
        bcc bur
        jmp bdr

bul:    lda #$01                // 1 = up left
        sta balldirection
        jmp initball

bdl:    lda #$02                // 2 = down left
        sta balldirection
        jmp initball

bur:    lda #$03                // 3 = up right
        sta balldirection
        jmp initball
        
bdr:    lda #$04                // 4 = down right
        sta balldirection

initball:
        lda postable+04         // reset the ball coordinates
        sta spritepos+04        // in accordance with initial
        lda postable+05         // position table
        sta postable+05

        rts

//check speed and delay loop-----------------------------------------------------
speed_and_delay:
        lda $043b               // check timer 
        cmp #$30                // if is the last 10 seconds
        beq delay_fastest       // go crazy fast            
        cmp #$36                // if it is below 60
        bcc delay_faster        // go faster                      
        jmp delay_normal        // otherwise start with standard speed

//-------------------------------> this delay is for faster speed 
                                // between 59-9 seconds
delay_faster:                    
        lda $d850               // check if the first character of the line is yellow
        cmp #$07                // if it is already yellow        
        beq faster              // no need to draw yellow lines and go faster routine        

        ldx #$00
yel:    lda #$07                // draw the yellow lines
        sta $d850,x             // upper line
        sta $db98,x             // lower line
        inx
        cpx #$28                // 40 characters
        bne yel

faster:
        lda current_arrayhi
        cmp #$1f
        bne not15f
        lda #$00               // faster delay routine for 15 degrees
        sta $f7
        sta $f8
wait21: inc $f8
wait20: inc $f7
        lda $f7
        cmp #$cd
        bne wait20
        lda $f8
        cmp #1                 // adjustment line for timing
        bne wait21
        lda $043b
        rts

not15f:
        lda #$00               // delay routine for standard speed
        sta $f7
        sta $f8
wait23: inc $f8
wait22: inc $f7
        lda $f7
        cmp #$20
        bne wait22
        lda $f8
        cmp #2                 // adjustment line for timing
        bne wait23
        lda $043b
        rts

// ------------------------------> this delay is for fastest speed 
                                 // speed for the last 9 seconds
delay_fastest:                               
        lda $d850               // check if the first character of the line is red
        cmp #$02                // if it is already red
        beq fastest             // no need to draw red lines and go to fastest routine

        ldx #$00                
redd:   lda #$02                // draw the red line
        sta $d850,x             // upper line
        sta $db98,x             // lower line
        inx
        cpx #$28                // 40 characters
        bne redd

fastest:
        lda current_arrayhi
        cmp #$1f 
        bne not15fs
        lda #$00               // delay routine
        sta $f7
        sta $f8
wait31: inc $f8
wait30: inc $f7
        lda $f7
        cmp #$a0
        bne wait30
        lda $f8
        cmp #1                 // adjustment line for timing
        bne wait31
        inc $d020       
        rts

not15fs:lda #$00               // delay routine
        sta $f7
        sta $f8
wait33:  inc $f8
wait32:  inc $f7
        lda $f7
        cmp #$fd
        bne wait32
        lda $f8
        cmp #1                 // adjustment line for timing
        bne wait33
        inc $d020       
        rts

// -----------------------------> this delay is for normal speed
                                // between 90-60 seconds            
delay_normal:                
        lda current_arrayhi     // check if it is 15 degrees
        cmp #$1f 
        bne not15               // if not, move to the standard speed routine
        lda #$00                // this is the same with standard but a little 
        sta $f7                 // faster
        sta $f8
wait35: inc $f8
wait34: inc $f7
        lda $f7
        cmp #$10
        bne wait34
        lda $f8
        cmp #2                 // adjustment line for timing
        bne wait35
        rts

not15:  lda #$00                // not 15 degrees, so delay in standard speed
        sta $f7
        sta $f8
wait37: inc $f8
wait36: inc $f7
        lda $f7
        cmp #$70
        bne wait36
        lda $f8
        cmp #2                 // adjustment line for timing
        bne wait37
        rts

//expand sprite msb--------------------------------------------------------------
placesprites:
        lda spritepos           // player 1 sprite
        sta $d000
        lda spritepos+1
        sta $d001
        lda #%00000001          // set msb on for sprite 1
        ora $d010               // since it is already at far right
        sta $d010

        lda spritepos+2         // player 2 is in the left (standard)
        sta $d002               // so no need to set msb
        lda spritepos+3
        sta $d003

        lda spritepos+4         // ball sprite
        sta $d004
        lda spritepos+5
        sta $d005
        
        ldx balldirection       // is the ball is up right?
uright: cpx #$03                
        bne dright              // if not, continue
        lda spritepos+4         
        cmp #$00                // is the ball sprite x position zero?
        bne endplace            // no, continue
        lda #%00000100          // yes, set the msb for the ball
        ora $d010
        sta $d010
        rts
        
dright: cpx #$04                // is the ball is down right?
        bne dleft               // if not, continue
        lda spritepos+4         
        cmp #$00                // is the ball sprite x position zero? 
        bne endplace            // no, go to end routine
        lda #%00000100          // yes, set the msb for the ball
        ora $d010
        sta $d010
        rts

dleft:  cpx #$02                // is the ball is down left?     
        bne uleft               // if not, continue
        lda spritepos+4
        cmp #$ff                // is the ball sprite x position 256?
        bne endplace            // no, go to end routine
        lda #%11111001          // yes, clear the msb
        and $d010
        sta $d010
        rts

uleft:  lda spritepos+4         // the ball is up left
        cmp #$ff                // is the ball sprite x position 256?
        bne endplace            // no, go to end routine 
        lda #%11111001          // yes, clear the msb
        and $d010
        sta $d010        

endplace:
        rts

//wait for a button press------------------------------------------------------------
wait_button:
        lda #<angle15y          // get lo .byte of angle15y array
        sta current_arraylo     // store it to $fd
        lda #>angle15y          // get hi .byte of angle15y array
        sta current_arrayhi     // store it to $fe

        lda selection           // load controller selection variable
        cmp #$00                // is it paddle?
        beq paddle_button       // yes, go and wait for paddle button
        cmp #$01
        beq joystick_button                 
        jmp wait_button

joystick_button:                // joystick is selected
        lda $dc00               // so wait for joystick 2 button 
        lsr
        lsr 
        lsr
        lsr
        lsr
        bcc exitb
        lda $dc01               // wait for joystick 1 button
        lsr
        lsr
        lsr
        lsr
        lsr
        bcc exitb
        jsr colorwash           // otherwise, colorwash the text on the screen      

rloop1: lda $d012               // short delay ->wait raster
        bne rloop1
        jmp joystick_button     // go back and wait paddle button again

paddle_button:
        lda $dc01              
        lsr 
        lsr 
        lsr                     // check paddle 1 button                 
        bcc exitb               // if pressed, exit waiting button
        lsr                     // check paddle 2 button
        bcc exitb               // if pressed, exit waiting button
        jsr colorwash           // otherwise, colorwash the text on the screen      

rloop2: lda $d012               // short delay ->wait raster
        bne rloop2
        jmp paddle_button       // go back and wait paddle button again

exitb:  lda #$20                // clear text on the middle of the screen
        ldy #$00
cl1:    sta $0478,y
        sta $0577,y
        sta $0677,y
        iny
        cpy #$ff
        bne cl1
        rts

//color wahs---------------------------------------------------------------------
colorwash:              
        lda color+$00           // this part color cycles the text on screen
        sta color+$28           // color codes are taken from the table down below
        ldx #$00
cycle:  lda color+$01,x
        sta color+$00,x
        lda color,x
        sta $dad0,x
        sta $daa8,x
        inx
        cpx #$28
        bne cycle
        rts

//start timer clock--------------------------------------------------------------
start_timer_clock:          // this part is the 90sec timer

        lda #%00000111      // enable sprites
        sta $d015
        lda #$7f
        sta $dd0d           // disable all cia2 nmis
        lda #<nmi
        sta $318
        lda #>nmi
        sta $319
        lda #$8a
        sta $dd04
        lda #$f0
        sta $dd05
        lda #$10
        sta $dd06
        lda #$0
        sta $dd07
        lda #%00010001      // start timer a counting cycles
        sta $dd0e           
        lda #%01010001      // start timer b counting t.a. underflows
        sta $dd0f           
        lda #%10000010      // enable nmi on t.b underflows
        sta $dd0d           
        rts

nmi:       
        pha                  // preserve register by pushing them to the stack
        txa
        pha
        tya
        pha
        lda $dd0d
        bpl done            // if nmi is not from cia2
        jsr write_counter   // go and write the counter on screen

done:   pla                 // pull the registers back from the stack
        tay
        pla
        tax
        pla
        rti

write_counter:
        dec $043c               // decrement right digit
        lda $043c
        cmp #$2f                // did we reach below zero?
        bne exit                // no, continue process
        lda #$39                // yes, zero the right digit
        sta $043c               // and decrement left digit
        dec $043b
        lda $043b               // did we reach below zero?
        cmp #$2f
        beq game_over           // if yes, then end the game
exit:   rts

//game over afte 90 seconds------------------------------------------------------
game_over:

        lda #$00                // stop the timer
        sta $dd0e
        sta $dd0f
        sta $d020

        lda #%00000000
        sta $d015               // disable all sprites
        lda #$00                
        sta $d83b               //blackout the timer
        sta $d83c

        ldx #$00                // write the text on the screen
got1:   lda print_game_over,x
        sta $05c7,x
        inx
        cpx #$0a
        bne got1

        ldx #$00
got2:   lda start_game_message1,x
        sta $06b0,x
        inx
        cpx #$17
        bne got2

        ldx #$00
got3:   lda start_game_message2,x
        sta $06da,x
        inx
        cpx #$14
        bne got3
                                // this part initially checks the left digits, it is easier
        lda $0435               // check the score by first comparing the left digits  
        cmp $0441               // if left digit of player 2<player 1       
        bcc player1wins_message // then player wins
        lda $0441               
        cmp $0435               // left digit of player 1<player 2
        bcc player2wins_message // then player 2 wins

                                // if we are here, so first digits are equal
                                // so go and check the right digits
        lda $0436               // check the score by comparing the right digits   
        cmp $0442               // if right digit of player2<player1 
        bcc player1wins_message // player 1 wins
        lda $0442
        cmp $0436               // if right digit of player1<player2 
        bcc player2wins_message // player 2 wins          

        jsr draw                // otherwise, the scores are equal and it is a draw       

finalize_match:
        jsr wait_button         // wait for paddle/joystick buttons
        jmp mainloop            // jump to main loop

player1wins_message:            // this part writes the winnig text on screen
        ldx #$00
pw1:    lda player_1_wins,x   
        sta $063d,x
        inx
        cpx #$0f
        bne pw1
        jmp finalize_match

player2wins_message:
        ldx #$00
pw2:    lda player_2_wins,x 
        sta $063d,x
        inx
        cpx #$0f
        bne pw2
        jmp finalize_match

draw:
        ldx #$00
dw:     lda draw_message,x 
        sta $063e,x
        inx
        cpx #$0e
        bne dw
        rts

//reset ball when player scores--------------------------------------------------
reset_ball_position:
        jsr ball_direction_at_start     // after a goal, reset the ball position             

        lda balldirection
        cmp #$01
        beq player1serving
        cmp #$02
        beq player1serving
        cmp #$03
        beq player2serving
        cmp #$04
        beq player2serving
        rts

player2serving:
        lda #$22
        sta spritepos+4
        lda spritepos+3
        clc        
        adc #$09
        sta spritepos+5
        lda #$01                        // set msb for only player 1
        sta $d010
        jsr placesprites
        rts

player1serving:
        lda #$34
        sta spritepos+4
        lda spritepos+1                 // load player 1 y position
        clc        
        adc #$09
        sta spritepos+5                 // and store it on the ball
        lda #$05                        // set msb for player 1 and ball
        sta $d010
        jsr placesprites
        rts  


//mainloop-----------------------------------------------------------------------
mainloop:
        jsr draw_play_screen            // draws the pre-drawn text screen
        jsr setup_sprites               // sets up the sprites
        jsr ball_direction_at_start     // get the randomized direction
        jsr select_preferences          // select number of players and controllers
        jsr wait_button                 // wait for a paddle button to be pressed
        jsr start_timer_clock           // start the countdown clock

gameloop:
        jsr speed_and_delay             // set the speed (in accordance with the time)
        jsr check_controls_p1           // check player 1 control and store the coordinates
        jsr check_controls_p2           // check player 2 control and store the coordinates
        jsr check_player_collision      // check if the ball hit any of the players
        jsr check_top_collision         // check if the ball is at the top and divert if necessary
        jsr check_bottom_collision      // check if the ball is at the bottom and divert if necessary
        jsr move_ball                   // move the ball according to the existing or new direction
        jsr update_player1_score        // check if the player 1 has scored the goal and update the score
        jsr update_player2_score        // check if the player 2 has scored the goal and update the score
        jsr placesprites                // draw the sprites on screen
        jmp gameloop
//--------------------------------------------------------------------------------

// check for player 1 paddle in port 1-------------------------------------------
check_controls_p1:
        lda selection
        cmp #$00                // selection 0 = peddals
        beq pedal1              // go to pedal controls                      
        jmp j1p1                // otherwise go to joystick control

pedal1: //lda #%11000000          // select pins 6-7 for ($c0=192)
        //sta $dc02               // data port b  ->disable keyboard?
        lda $dc00               // control-port 1 selected (when bit6=1 and bit7=0)
        and #%00111111          // clear bit-6/7        
        ora #%01000000          // set bit-6
        sta $dc00               // now control-port 1 is selected for reading the pot registers

        ldx $d419               // get the player 1 coordinates

                                // this part of the routine is common
p1j:    stx spritepos+1         // store it on the position -> joystick routine joins from here
        ldx spritepos+1
        cpx #$48                // check if it is already on the top 
        bcs setup1              // if not, go and check if it is at the bottom
        ldx #$48                // yes, it is at the top so lock it

setup1: cpx #$cc                // is it at the bottom?
        bcc setup2              // no, go and store it to the table
        ldx #$cc                // yes, it is at the bottom so lock it

setup2: stx spritepos+1

        lda tempbuffer          // load the initial value from buffer
        sta $dc02               // and store it to port direction
        rts

// check for player 2 paddle in port 1-------------------------------------------
check_controls_p2:
        lda selection
        cmp #$00
        beq pedal2
        jmp j2p2
pedal2: //lda #%11000000          // select pins 6-7 for 
        //sta $dc02               // data port b  ->disable keyboard?
        lda $dc00               // control-port 1 selected (when bit6=1 and bit7=0)
        and #%00111111          // clear bit-6/7        
        ora #%01000000          // set bit-6
        sta $dc00               // now control-port 1 is selected for reading the pot registers

        ldy $d41a               // get the player 2 coordinates
p2j:    sty spritepos+3         // store it on the position -> joystick routine joins from here
        ldy spritepos+3
        cpy #$48                // check if it is already on the top 
        bcs setup3              // if not, go and check if it is at the bottom
        ldy #$48                // yes, it is at the top so lock it
        
setup3: cpy #$cc                // is it at the bottom?
        bcc setup4              // no, go and store it to the table
        ldy #$cc                // yes, it is at the bottom so lock it

setup4: sty spritepos+3

        lda tempbuffer          // load the initial value from buffer
        sta $dc02               // and store it to port direction
        rts

//check for ball vs player collision---------------------------------------------
check_player_collision:

        lda $d01e               // check the hardware collison register
        cmp #%00000101          // compare ot for player 1 vs ball
        beq collided1           // if set, ball and player 1 collided
        cmp #%00000110          // compare ot for player 2 vs ball
        beq collided2           // if set, ball and player 2 collided
        rts                     // return if no collision happened

collided1:                      // player 1 collision----------------------------
        lda spritepos+5         // load ball y position
        sec                
        sbc spritepos+1         // subtract the player 1 position
        bcs midbot1             // if positive, continue checking (it is either middle or bottom)       
        lda #>angle45y          // set current_arrayhi address to angle45y
        sta current_arrayhi
        jmp cont1               // and continue with rest of collision routine

midbot1:
        sta delta               // ball hit middle or bottom part
        lda delta               // load the difference
        sec
        sbc #$0b                // subtract the middle part difference            
        bcs bot1                // if positive, then ball hit the bottom part
        lda #>angle15y          // set current_arrayhi address to angle45y
        sta current_arrayhi
        jmp cont1               // and continue with rest of collision

bot1:   lda #>angle30y          // set current_arrayhi address to angle30y
        sta current_arrayhi

cont1:  jsr playerhitsound      // play player-ball collision sound effect
        lda balldirection       // check the ball direction
        cmp #$03                // if it is up right
        bne other1
        lda #$01                // then make it up left (deflect)
        sta balldirection
        lda #$00                // collision happened
        sta angle_pointer       // so reset the angle table pointer 
        rts

other1: lda balldirection       
        cmp #$04                // if it is down right
        bne egzit1
        lda #$02                // then make it down left (deflect)
        sta balldirection
        lda #$00                // collision happened
        sta angle_pointer       // so reset the angle table pointer
egzit1: rts

collided2:                      // player 2 collision----------------------------
        lda spritepos+5         // load ball y position
        sec                
        sbc spritepos+3         // subtract the player 1 position
        bcs midbot2             // if positive, continue checking (it is either middle or bottom)       
        lda #>angle45y          // set current_arrayhi address to angle45y
        sta current_arrayhi
        jmp cont2               // and continue with rest of collision

midbot2: 
        sta delta               // ball hit middle or bottom part
        lda delta               // load the difference
        sec
        sbc #$0b                // subtract the middle&upper part height            
        bcs bot2                // if positive, then ball hit the upper part
        lda #>angle15y          // set current_arrayhi address to angle15y
        sta current_arrayhi
        jmp cont2               // and continue with rest of collision

bot2:   lda #>angle30y          // set current_arrayhi address to angle30y
        sta current_arrayhi

cont2:   
        jsr playerhitsound      // play player-ball collision sound effect
        lda balldirection       // check the ball direction
        cmp #$02                // if it is down left
        bne other2
        lda #$04                // then make it down right (deflect)
        sta balldirection
        lda #$00                // collision happened
        sta angle_pointer       // so reset the angle table pointer
        rts

other2: lda balldirection
        cmp #$01                // if it is up left
        bne egzit2
        lda #$03                // then make it up right
        sta balldirection
        lda #$00                // collision happened
        sta angle_pointer       // so reset the angle table pointer
egzit2: rts

//direct ball------------------------------------------------------------
move_ball:
        ldy angle_pointer
        lda ($fd),y             // current_arraylo address -> get the value pointed
        sta addsub              // and store it to temp address

        lda balldirection       // load the ball direction variable
        cmp #$01                // if 1, then move it up left
        beq ball_up_left
        cmp #$02                // if 2, then move it down left
        beq ball_down_left
        cmp #$03                // if 3, then move it up right
        beq ball_up_right
        cmp #$04                // if 4, then move it down right
        beq ball_down_right
        rts

// ball moving up & left---------------------------------------------------------
ball_up_left:                           // decx & decy
        dec spritepos+4                 // decrement the x pos (to left)      
        lda addsub
        beq ul
        dec spritepos+5
ul:     inc angle_pointer               // inc angle_pointer after moving ball
        rts

// ball moving down & left-------------------------------------------------
ball_down_left:                         // decx & incy
        dec spritepos+4                 // decrement the x pos (to left)
        lda addsub
        beq dl
        inc spritepos+5
dl:     inc angle_pointer               // inc angle_pointer after moving ball
        rts

// ball moving up & right--------------------------------------------------
ball_up_right:                          // incx & decy
        inc spritepos+4                 // decrement the x pos (to left)
        lda addsub
        beq ur
        dec spritepos+5
ur:     inc angle_pointer               // inc angle_pointer after moving ball
        rts

// ball moving down & right-------------------------------------------------
ball_down_right:                        // incx & incy
        inc spritepos+4                 // decrement the x pos (to left)
        lda addsub
        beq dr
        inc spritepos+5
dr:     inc angle_pointer               // inc angle_pointer after moving ball
        rts

//check if player 1 scores-------------------------------------------------------
update_player1_score:
                                // here, we first check the msb.
                                // if msb=1, then only player 1 is at right, ball is on the left screen
        lda $d010
        cmp #$01                // if msb=1
        bne scexit1     
        lda spritepos+04        // check the ball x position
        cmp #$05                // if it is 5
        beq player1_scores      // then it has reached the right limit and it's goal
scexit1:rts

player1_scores:
        inc $0442               // increment the right digit of the score
        lda $0442
        cmp #$3a                // is the right digit > 9 ?
        bne done1               // no, continue
        lda #$30                // yes, reset the right digit to 0
        sta $0442
        inc $0441               // increment the left digit 
                                // please note that, it is not possible to score more than 90
                                // so no need to cehck the overflow of the left digit, it is what it is

done1:  jsr scoresound          // play the crowd cheer sound effct 
        jsr flashborder_blue    // flash the border blue for player 1
        jsr reset_ball_position // reset the ball position
        lda #>angle15y          // get hi .byte of angle15y array
        sta current_arrayhi     // store it to $fe
        rts

flashborder_blue:
        lda #%001111111         // enable sprites 0 to 7
        sta $d015               // to show the goal sprites
        lda #$0e                // make the border blue
        sta $d020

        ldx #$00                // this part is for flashing the goal sprites
flb2:   ldy #$00                // by changing the colors very fast
flb1:   sty $d02a
        sty $d02b
        sty $d02c
        sty $d02d        
        iny
        cpy #$ff
        bne flb1
        inx
        cpx #$ff
        bne flb2

        lda #$00                // make the border black again
        sta $d020
        lda #%000000111         // turn off the goal sprites (players and the ball are on)
        sta $d015   
        rts

//check if player 2 scores-------------------------------------------------------
update_player2_score:
                                // here, we first check the msb.
                                // if msb=5, then player 1 and the ball is on the right
        lda $d010
        cmp #$05
        bne scexit2
        lda spritepos+04        // check the ball x position
        cmp #$50                // if it is 50
        beq player2_scores      // then it has reached the left limit and it's goal
scexit2:rts

player2_scores:
        inc $0436               // increment the right digit of the score
        lda $0436
        cmp #$3a                // is the right digit > 9 ?
        bne done2               // no, continue
        lda #$30                // yes, reset the right digit to 0
        sta $0436
        inc $0435               // increment the left digit 
                                // please note that, it is not possible to score more than 90
                                // so no need to cehck the overflow of the left digit, it is what it is

done2:  jsr scoresound          // play the crowd cheer sound effct
        jsr flashborder_red     // flash the border red for player 2
        jsr reset_ball_position // reset the ball position
        lda #>angle15y          // get hi .byte of angle15y array
        sta current_arrayhi     // store it to $fe
        rts

flashborder_red:
        lda #%001111111
        sta $d015               // enable sprites 0 to 7
        lda #$02                // to show the goal sprites
        sta $d020               // make the border red

        ldx #$00                // this part is for flashing the goal sprites
flr2:   ldy #$00                // by changing the colors very fast
flr1:   sty $d02a
        sty $d02b
        sty $d02c
        sty $d02d        
        iny
        cpy #$ff
        bne flr1
        inx
        cpx #$ff
        bne flr2

        lda #$00                // make the border black again
        sta $d020
        lda #%000000111         // turn off the goal sprites (players and the ball are on)
        sta $d015   
        rts

//check if the ball is at the bottom of the field-----------------------------
check_bottom_collision:
        lda spritepos+5         // check the ball's y position
        cmp #$db                // did it reach bottom?
        bcc cbcend              // no, continue
        dec spritepos+5         // throw out ball from collison line
        lda balldirection       // yes, check the direction
        cmp #$02                // is it down left?
        bne chbc                // no, continue
        lda #$01                // yes, make it up left (deflect)
        sta balldirection
        jsr borderhitsound      // play the border hit sound effect
        rts

chbc:   lda #$03                // it was down left, so make it up right (deflect)
        sta balldirection
        jsr borderhitsound       // play the border hit sound effect 
cbcend: rts

//check if the ball is at the top of the field-----------------------------
check_top_collision:
        lda spritepos+5         // check the ball's y position
        cmp #$42                // did it reach top?
        bcs ctcend              // no, continue
        inc spritepos+5         // throw out ball from collison line
        lda balldirection       // yes, check the direction
        cmp #$01                // is it up left?   
        bne ctc                 // no, continue
        lda #$02                // yes, make it down left (deflect)
        sta balldirection
        jsr borderhitsound      // play the border hit sound effect
        rts

ctc:    lda #$04                // it was up right, so make it down right (deflect)
        sta balldirection
        jsr borderhitsound      // play the border hit sound effect

ctcend: rts

//play the sound when ball hits top and bottom borders---------------------------
borderhitsound:
//clean sid                      // voice 1 settings
        lda #0
        sta $d404
// volume
        lda #15
        sta $d418
// voice 1 frequency, lo-hi .byte 
        lda #143
        sta $d400
        lda #10
        sta $d401
// voice 1 pulse waveform width, lo-hi .byte      
        lda #128
        sta $d402
        lda #0
        sta $d403
// voice 1 adsr
        lda #53
        sta $d405
        lda #00
        sta $d406
// voice 1 waveform(s) pulse 
        ldx #32
        inx
        txa
        sta $d404
        rts

//play the sound when ball hits players------------------------------------------
playerhitsound:
//clean sid                      // voice 2 settings
        lda #0
        sta $d40b
// volume
        lda #15
        sta $d418
// voice 2 frequency, lo-hi .byte 
        lda #156
        sta $d407
        lda #12
        sta $d408
// voice 2 pulse waveform width, lo-hi .byte      
        lda #128
        sta $d409
        lda #0
        sta $d40a
// voice 2 adsr
        lda #53
        sta $d40c
        lda #00
        sta $d40d
// voice 2 waveform(s) pulse 
        ldx #32
        inx
        txa
        sta $d40b
        rts

//play the sound when players score---------------------------------------------
scoresound:
        lda  #00                 // voice 3 settings      
        sta $d412
// voice 3 frequency, lo-hi .byte 
        lda #143
        sta $d40e
        lda #10
        sta $d40f
// voice 3 pulse waveform width, lo-hi .byte 
        lda #128
        sta $d410
        lda #0
        sta $d411
// voice 3 adsr
        lda #76
        sta $d413
        lda #2
        sta $d414
// voice 3 waveform(s) 
        ldx #128
        inx
        txa
        sta $d412

        rts

// play intro music--------------------------------------------------------------

play_music:
        sei             // this routine plays the sound of the music.bin file
        lda #<irq       // it is driven by the irq
        ldx #>irq
        sta $314
        stx $315
        ldy #$7f 
        sty $dc0d
        lda #$01
        sta $d01a
        sta $d019 
        lda music
        jsr $2200       // player routine inside the music file
        cli
        rts

irq:    inc $d019
        jsr $2203
        jmp $ea31

// stop intro music--------------------------------------------------------------

stop_music:                
        sei             // this part clears the music interrupt and stops it
        lda #$31
        sta $314
        lda #$ea
        sta $315
        lda #$81
        sta $dc0d
        lda #$00
        sta $d01a
        inc $d019
        lda $dc0d
        jsr $2200
        cli
        rts

// make paddle or joystick selection---------------------------------------------
select_preferences:

        ldx #$00
ptex:   lda player_number_text,x
        sta $056e,x
        inx
        cpx #$18
        bne ptex   

        lda #$00
        sta controlkey1
        sta controlkey2

select_opponent:
        jsr $ffe4              // waits for key press
        beq select_opponent
        sta controlkey1
        lda controlkey1
        cmp #49                // compares it to '1'
        beq oneplayer    
        cmp #50                // compares it to '2'
        beq twoplayer
        jmp select_opponent      

oneplayer:
        lda #$01
        sta noofplayers
        lda #49
        sta $0588
        jmp !+

twoplayer:
        lda #$02
        sta noofplayers
        lda #50
        sta $0588

!: 
        ldx #$00
ctex:   lda controller_text,x
        sta $05ba,x
        inx
        cpx #$21
        bne ctex   

        lda #$00
        sta controlkey1

select_control:
        jsr $ffe4              // waits for key press
        beq select_control
        sta controlkey2       
        lda controlkey2
        cmp #74                // compares it to 'j'
        beq joystick    
        cmp #80                // compares it to 'p'
        beq paddle
        jmp select_control

joystick:
        lda #$01                // joystick =1
        sta selection
        ldx #$00
jtext:  lda joystick_selected,x
        sta $0613,x
        inx
        cpx #$11
        bne jtext
        jmp write_button_text

paddle:
        lda #$00                 // paddle = 0
        sta selection           
        ldx #$00
pdtext: lda paddle_selected,x
        sta $0613,x
        inx
        cpx #$11
        bne pdtext

write_button_text:
        ldx #$00
wbt:    lda press_a_button,x
        sta $06aa,x
        inx
        cpx #$23
        bne wbt
 
        lda #$00
        sta controlkey2

        jmp wait_button

// joystick control routine for player 1-----------------------------------------
j1p1:   //lda #$ff
        //sta $dc00
        //sta $dc02
        lda #$01                // check joystick port 1 for player 1
        bit $dc01
        bne joy
        ldx spritepos+1        // player 1 y coordinate
        dex                     // move up (2 pixels) 
        dex
        jmp p1j                // continue checking p1 position

joy:    lda #$02
        bit $dc01
        bne jexit1
        ldx spritepos+1
        inx
        inx
        jmp p1j
jexit1: rts


// if joystick selected, check player control 2----------------------------------
j2p2:   //lda #$7f
        //sta $dc00
        //sta $dc02
        lda #$01
        bit $dc00
        bne joy2
        ldy spritepos+3
        dey
        dey
        jmp p2j
        
joy2:   lda #$02
        bit $dc00
        bne jexit2
        ldy spritepos+3
        iny
        iny
        jmp p2j
jexit2: rts


//-------variables--------------------------------------------------------

.label spritepos=$0370                       // positions for the sprites
postable: .byte $37,$85,$1f,$85,$aa,$8d,$40,$85,$7f,$85,$bf,$85,$ff,$85   // sprite initial positions
.label balldirection=$0380                   // 1=up&left, 2=down&left, 3=up&right, 4=down&right
.label scrdata=$7f40                         // bitmap title data - $7f40
.label coldata=$8328                         // bitmap title color data - $8328     
.label scrram=$5c00                          // bitmap title screen location - $5c00
.label colram=$d800                          // bitmap title color location - $d800 
tempbuffer:     .byte 0
key:            .byte 0                      // get char. of key pressed
controlkey1:    .byte 0                      // number of player selection variable
controlkey2:    .byte 0                      // paddle or joystick selection variable
selection:      .byte 0                      // 0=paddle, 1=joystick
music:          .byte 0

delta:          .byte $00
addsub:         .byte $00     // temporarily write the value of table
noofplayers:    .byte $00

print_game_over:        .text "game  over"
player_1_wins:          .text "player 1 wins !"
player_2_wins:          .text "player 2 wins !"
draw_message:           .text "it is a draw ! "
joystick_selected:      .text "joystick selected"
paddle_selected:        .text " paddle selected "
press_a_button:         .text "press controller button to continue"
player_number_text:     .text "number of players? (1/2)"
controller_text:        .text "controller ? (j)oystick/(p)addles"
start_game_message1:    .text "press controller button"
start_game_message2:    .text "   to start over    "

//color wash table---------------------------------------------------------------

color:       .byte $09,$09,$02,$02,$08
             .byte $08,$0a,$0a,$0f,$0f
             .byte $07,$07,$01,$01,$01
             .byte $01,$01,$01,$01,$01
             .byte $01,$01,$01,$01,$01
             .byte $01,$01,$01,$07,$07
             .byte $0f,$0f,$0a,$0a,$08
             .byte $08,$02,$02,$09,$09
             .byte $00,$00,$00,$00,$00
//-------------------------------------------------------------------------------
//angle y inc/dec tables
*=$1d00 "angle arrays"
angle45y:
        .byte    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
        .byte    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
        .byte    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
        .byte    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
        .byte    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
        .byte    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
        .byte    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

angle30y:
        .byte    1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1
        .byte    1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0
        .byte    1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1
        .byte    1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1
        .byte    0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1
        .byte    1,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1
        .byte    1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,0

angle15y:
        .byte    0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0
        .byte    0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1
        .byte    0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0
        .byte    0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0
        .byte    1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0
        .byte    0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0
        .byte    0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1


//sprite definitions---------------------------------------------------------
*=$2000 "sprite definitions"
//sprite 0 data // player 1 - right player --> pointer $80
 .byte $00,$00,$00,$00,$00,$3a,$00,$00,$3a,$00,$00,$3a,$00,$00,$3a,$00
 .byte $00,$2a,$00,$00,$2a,$00,$00,$2a,$00,$00,$2a,$00,$00,$2a,$00,$00
 .byte $2a,$00,$00,$3a,$00,$00,$3a,$00,$00,$3a,$00,$00,$3a,$00,$00,$00
 .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

//sprite 1 data // player 2 - left player --> pointer $81
 .byte $00,$00,$00,$ac,$00,$00,$ac,$00,$00,$ac,$00,$00,$ac,$00,$00,$a8
 .byte $00,$00,$a8,$00,$00,$a8,$00,$00,$a8,$00,$00,$a8,$00,$00,$a8,$00
 .byte $00,$ac,$00,$00,$ac,$00,$00,$ac,$00,$00,$ac,$00,$00,$00,$00,$00
 .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

//sprite 2 data // ball --> pointer $82
 .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3c,$00,$00,$7e
 .byte $00,$00,$5a,$00,$00,$7e,$00,$00,$66,$00,$00,$3c,$00,$00,$00,$00
 .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

//sprite 3 data // g -> pointer $83
 .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f0,$00,$00,$f0
 .byte $00,$00,$f0,$00,$00,$f0,$00,$00,$f0,$00,$00,$f0,$0f,$ff,$f0,$0f
 .byte $ff,$f0,$0f,$ff,$f0,$0f,$ff,$f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f
 .byte $f0,$00,$0f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$07

//sprite 4 data // o -> pointer $84
 .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f0,$00,$0f,$f0
 .byte $00,$0f,$f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f,$f0,$00
 .byte $0f,$f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f
 .byte $f0,$00,$0f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00

//sprite 5 data // a -> pointer $85
 .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f0,$00,$0f,$f0
 .byte $00,$0f,$f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f,$ff,$ff,$ff,$ff,$ff
 .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f
 .byte $f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f,$f0,$00,$0f,$60

//sprite 6 data // l -> pointer $86
 .byte $f0,$00,$00,$f0,$00,$00,$f0,$00,$00,$f0,$00,$00,$f0,$00,$00,$f0
 .byte $00,$00,$f0,$00,$00,$f0,$00,$00,$f0,$00,$00,$f0,$00,$00,$f0,$00
 .byte $00,$f0,$00,$00,$f0,$00,$00,$f0,$00,$00,$f0,$00,$00,$f0,$00,$00
 .byte $f0,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$c0

// music file----------------------------------------------------------------
*=$2200 "music file"
.import binary "donence.bin",2

// new font file--------------------------------------------------------------       
*=$3800 "font file"             
.import binary "zx5.bin"

// play screen--------------------------------------------------------------    
//screen data
*=$4800 "screen data"
.import binary "play_screen_data.bin",2

//color data
*=$4c00 "screen color data"
.import binary "play_screen_color.bin",2

*=$6000 "bitmap koala image"
.import binary "pongy61.prg",2
