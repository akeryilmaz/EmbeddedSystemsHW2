#include "p18f8722.inc"

    CONFIG OSC=HSPLL, FCMEN=OFF, IESO=OFF,PWRT=OFF,BOREN=OFF, WDT=OFF, MCLRE=ON, LPT1OSC=OFF, LVP=OFF, XINST=OFF, DEBUG=OFF
;*******************************************************************************
; Variables & Constants
;*******************************************************************************
    ; variables 
    ; health
    ; level
    ; timer1 starting value
    ; number of balls spawned in that level
    ; 15bits to determine which balls are active(5-10-15 are used for each level)
    ; 15 times 5bits for determining where the balls are
    UDATA_ACS
w_temp res 1
status_temp res 1
pclath_temp res 1
iterator res 1
pressed res 1 ; pressed[0] := RG0, pressed[2] := RG2, pressed[3] := RG3    
health res 1; 7 segment display of health is at portH.0 -> b'00000001' = 1
level res 1; 7 segment display of level is at portH.1 -> b'00000010' = 2
timer0_counter res 1
timer0_state res 1 ; this is set when the required time has passed for each level
timer1_initial_value res 1
numberOfBallsToCreate res 1
activeBalls res 1 ; 6 balls can be active at 1 time
ball1Position res 1 ; 0 indicates top left, 23 indicates bottom right. Add 4 per update.
ball2Position res 1
ball3Position res 1 
ball4Position res 1
ball5Position res 1 
ball6Position res 1
barPosition res 1 ; Leftmost bar position. Between 20-22(inclusive) for easier comparison. 20 means bar is at 20 and 21'st points. 22 means bar is at 22nd and 23th points.
timer1Modulo res 1; used for detecting where to spawn new ball
tempBallPosition res 1
barMoveDirection ; 1 if right, 0 if left

;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    main                   ; go to beginning of program

;Interrupt Vector    

INT_VECT CODE     0x08
    GOTO    isr             ;go to interrupt service routine

;;;;;;;;;;;; Register handling for proper operation of main program ;;;;;;;;;;;;
save_registers:
    movwf   w_temp          ;Copy W to TEMP register
    swapf   STATUS, w       ;Swap status to be saved into W
    clrf    STATUS          ;bank 0, regardless of current bank, Clears IRP,RP1,RP0
    movwf   status_temp     ;Save status to bank zero STATUS_TEMP register
    movf    PCLATH, w       ;Only required if using pages 1, 2 and/or 3
    movwf   pclath_temp     ;Save PCLATH into W
    clrf    PCLATH          ;Page zero, regardless of current page
    return

restore_registers:
    movf    pclath_temp, w  ;Restore PCLATH
    movwf   PCLATH          ;Move W into PCLATH
    swapf   status_temp, w  ;Swap STATUS_TEMP register into W
    movwf   STATUS          ;Move W into STATUS register
    swapf   w_temp, f       ;Swap W_TEMP
    swapf   w_temp, w       ;Swap W_TEMP into W
    return
    
;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************

level_shift_table
    MOVF    PCL, F  ; A simple read of PCL will update PCLATH, PCLATU
    RLNCF   WREG, W ; multiply index X2
    ADDWF   PCL, F  ; modify program counter
    RETLW 0 ;should not happen
    RETLW d'1' ;1 -> shitf 1
    RETLW d'3' ;2 -> shift 3
    RETLW d'5' ;3 -> shift 5
    
level_table
    MOVF    PCL, F  ; A simple read of PCL will update PCLATH, PCLATU
    RLNCF   WREG, W ; multiply index X2
    ADDWF   PCL, F  ; modify program counter
    RETLW 0 ;should not happen
    RETLW d'5' ;1 -> 5 balls
    RETLW d'10' ;2 -> 10 balls
    RETLW d'15' ;3 -> 15 balls
    
timer0_table
    MOVF    PCL, F  ; A simple read of PCL will update PCLATH, PCLATU
    RLNCF   WREG, W ; multiply index X2
    ADDWF   PCL, F  ; modify program counter
    RETLW 0 ;should not happen
    RETLW d'100' ;1 -> 100*4,992 = 499,2 ms
    RETLW d'80' ;2 -> 80*4,992 = 399,39 ms
    RETLW d'60' ;3 -> 60*4,992 = 299,52 ms
    
segment_table
    MOVF    PCL, F  ; A simple read of PCL will update PCLATH, PCLATU
    RLNCF   WREG, W ; multiply index X2
    ADDWF   PCL, F  ; modify program counter
    RETLW b'00111111' ;0 representation in LEDs
    RETLW b'00000110' ;1 representation in LEDs
    RETLW b'01011011' ;2 representation in LEDs
    RETLW b'01001111' ;3 representation in LEDs
    RETLW b'01100110' ;4 representation in LEDs
    RETLW b'01101101' ;5 representation in LEDs
    RETLW b'01111101' ;6 representation in LEDs
    RETLW b'00000111' ;7 representation in LEDs
    RETLW b'01111111' ;8 representation in LEDs
       
ball_position_light_table ;Assume a valid ball position on wreg
    rlncf   WREG ; multiply wreg by 2
    MOVF    PCL, F  ; A simple read of PCL will update PCLATH, PCLATU
    RLNCF   WREG, W ; multiply index X2
    ADDWF   PCL, F  ; modify program counter
    bsf	    LATA,0
    return
    bsf	    LATB,0
    return
    bsf	    LATC,0
    return
    bsf	    LATD,0
    return
    bsf	    LATA,1
    return
    bsf	    LATB,1
    return
    bsf	    LATC,1
    return
    bsf	    LATD,1
    return
    bsf	    LATA,2
    return
    bsf	    LATB,2
    return
    bsf	    LATC,2
    return
    bsf	    LATD,2
    return
    bsf	    LATA,3
    return
    bsf	    LATB,3
    return
    bsf	    LATC,3
    return
    bsf	    LATD,3
    return
    bsf	    LATA,4
    return
    bsf	    LATB,4
    return
    bsf	    LATC,4
    return
    bsf	    LATD,4
    return
    bsf	    LATA,5
    return
    bsf	    LATB,5
    return
    bsf	    LATC,5
    return
    bsf	    LATD,5
    return
    
isr:
    btfss INTCON, 2 ; TMR0IF is bit 2
    retfie ;some other interrupt, should not happen return
    decf    timer0_counter, f              ;Timer interrupt handler part begins here by decrementing count variable
    btfss   STATUS, Z               ;Is the result Zero?
    goto    timer_interrupt_exit    ;No, then exit from interrupt service routine
    clrf    timer0_counter                 ;Yes, then clear count variable
    comf    timer0_state, f                ;Complement our state variable

timer_interrupt_exit:
    bcf     INTCON, 2           ;Clear TMROIF
    movlw   d'61'               ;256-61=195; 195*256*100 = 4992000 instruction cycle;
    movwf   TMR0
    call    restore_registers   ;Restore STATUS and PCLATH registers to their state before interrupt occurs
    retfie
    
;NOTE: the +-100 ms might be the same for all balls at each update or it might be unique to each ball at each update.

; <X> indicates X is a label/state etc.
; "X" indicates X is a variable

; initialize 
; -> set the bar at RA5 & RB5
; -> set level to 1 at D3 of 7segment display
; -> set health to 5 at D0 of 7segment display
; set "ball update period" to its new value
; -> goto <start>
initialize
    ;setup timers
    clrf TMR0; TMR0 = 0
    clrf INTCON; Interrupts disabled for now
    movlw b'11010111'; enable timer, 8-bit operation, ; falling edge, select prescaler ; with 1:256, internal source
    movwf T0CON; T0CON <-W
    movlw d'61'; 10MHZ clock -> 10^7 cycles per second -> 10^-4 ms per cycle; 
    movwf TMR0L; counter can count x*256 cycles -> x*256*10^-4 ms -> x=195 for 4,992 ms -> 256-195 = 61 = '0x3d'
    movlw d'100'; 100*4,992= 499,2 ms is passed to the counter to count ~500ms for level1
    movwf timer0_counter
    movlw b'01000101'
    movwf T1CON
    ;setup ports, inputs-outputs etc.
    movlw 0x0F
    movwf ADCON1 ; set A/D conversion
    movlw b'00001101' ; RG0-RG2-RG3 are input 
    movwf TRISG
    clrf LATG ; clear port G content just in case TODO can we clear without reading?
    clrf TRISA; RA0-RA5, RB0-RB5, RC0-RC5, RD0-RD5 are outputs
    clrf TRISB
    clrf TRISC
    clrf TRISD
    clrf LATA ; clear output port content just in case TODO can we clear without reading?
    clrf LATB
    clrf LATC
    clrf LATD
    clrf TRISH ; porth and portj are 7segment display(outputs)
    clrf TRISJ
    clrf LATH
    clrf LATJ
    ;set variables
    movlw d'5'
    movwf health
    movlw d'5'
    movwf numberOfBallsToCreate
    movlw d'1'
    movwf level
    movlw d'1'
    movwf TRISH ; enable first 7segment display for setting health
    movlw b'01101101' ;5 for 7segment display
    movwf LATJ ; TODO we may need to movwf to TRISJ instead
    nop ;it says wait a while on the hw pdf
    clrf TRISH
    clrf LATJ
    movlw d'2'
    movwf TRISH ;enable second 7segment display for setting level
    movlw b'00000110' ; 1 for 7segment display
    movwf LATJ ; TODO we may need to movwf to TRISJ instead
    nop ;it says wait a while on the hw pdf
    clrf TRISH
    clrf LATJ
    
    clrf activeBalls
    movlw d'20'
    movwf barPosition
    movlw b'00100000'
    movwf LATA ; light the bar
    movwf LATB ; light the bar
    return


; -> save timer1 value(16 bit)
; wait for RGO, if it is pressed and released goto loop
wait_rg0_press:         
    btfsc pressed, 0
    goto wait_rg0_release
    btfss PORTG, 0
    goto wait_rg0_press
    bsf pressed, 0
    
wait_rg0_release:
    btfsc PORTG, 0
    goto wait_rg0_release
    bcf pressed, 0
    ;Enable interrupts 
    movlw   b'11100000' ;Enable Global, peripheral, Timer0 by setting GIE, PEIE, TMR0IE bits to 1
    movwf   INTCON
    bsf     T0CON, 7    ;Enable Timer0 by setting TMR0ON to 1
    movf    TMR1L,W
    movwf   timer1_initial_value
    goto main_loop
    
light_balls
    movlw b'00100000'
    andwf LATA
    andwf LATB
    andwf LATC
    andwf LATD
    btfss activeBalls, 0 ; if ball is active, skip
    goto ligthball2
    movf ball1Position, W
    call ball_position_light_table
ligthball2:
    btfss activeBalls, 1 ; if ball is active, skip
    goto ligthball3
    movf ball2Position, W
    call ball_position_light_table
ligthball3:
    btfss activeBalls, 2 ; if ball is active, skip
    goto ligthball4
    movf ball3Position, W
    call ball_position_light_table
ligthball4:
    btfss activeBalls, 3 ; if ball is active, skip
    goto ligthball5
    movf ball4Position, W
    call ball_position_light_table
ligthball5:
    btfss activeBalls, 4 ; if ball is active, skip
    goto ligthball6
    movf ball5Position, W
    call ball_position_light_table
ligthball6:
    btfss activeBalls, 5 ; if ball is active, skip
    goto finish_ligth_ball
    movf ball6Position, W
    call ball_position_light_table
finish_ligth_ball:
    return
    
checkBall1   ;while moving the bar, check ball1, whether it is caught, missed or early to decide  
    movf barPosition, 0
    subwf ball1Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 0 ;(bar is on the ball, deactivate the ball)
    incf barPosition, 0 ; store in W
    subwf ball1Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 0 ;(bar is on the ball, deactivate the ball)
    return
    
checkBall2   ;while moving the bar, check ball2, whether it is caught, missed or early to decide
    movf barPosition, 0
    subwf ball2Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 1 ;(bar is on the ball, deactivate the ball)
    incf barPosition, 0 ; store in W
    subwf ball2Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 1 ;(bar is on the ball, deactivate the ball)
    return
    
checkBall3  ;while moving the bar, check ball3, whether it is caught, missed or early to decide
    movf barPosition, 0
    subwf ball3Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 2 ;(bar is on the ball, deactivate the ball)
    incf barPosition, 0 ; store in W
    subwf ball3Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 2 ;(bar is on the ball, deactivate the ball)
    return
    
checkBall4  ;while moving the bar, check ball4, whether it is caught, missed or early to decide
    movf barPosition, 0
    subwf ball4Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 3 ;(bar is on the ball, deactivate the ball)
    incf barPosition, 0 ; store in W
    subwf ball4Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 3 ;(bar is on the ball, deactivate the ball)
    return
    
checkBall5   ;while moving the bar, check ball5, whether it is caught, missed or early to decide
    movf barPosition, 0
    subwf ball5Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 4 ;(bar is on the ball, deactivate the ball)
    incf barPosition, 0 ; store in W
    subwf ball5Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 4 ;(bar is on the ball, deactivate the ball)
    return
    
checkBall6   ;while moving the bar, check ball6, whether it is caught, missed or early to decide
    movf barPosition, 0
    subwf ball6Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 5 ;(bar is on the ball, deactivate the ball)
    incf barPosition, 0 ; store in W
    subwf ball6Position, 0 ; store in W
    btfsc STATUS,Z ; if result is not zero skip
    bcf activeBalls, 5 ;(bar is on the ball, deactivate the ball)
    return
    
   
    
; move the bar
; -> if RG2 is pressed & bar is not at RE5-RF5 move right. Reset RG2 to 0 and goto <move the active balls>.
; -> if RG3 is pressed & bar is not at RA5-RB5 move left. Reset RG3 to 0 and goto <move the active balls>.
; ->goto <move the active balls>
moveTheBar
    btfsc PORTG,2 ; if RG2 is NOT pressed don't execute move right
    goto moveRight
    btfsc PORTG,3 ; if RG3 is NOT pressed don't execute move left
    goto moveLeft
    return
    
moveRight:
    movlw d'22'
    cpfslt barPosition ; skip if we are already on the rightmost position
    return ;(barPosition=22)
    bsf barMoveDirection,0
    goto resetBarLights
    
moveLeft:
    movlw d'20'
    cpfsgt barPosition ;skip if we are already on the leftmost position
    return ;(barPosition=20)
    bcf barMoveDirection,0
    goto resetBarLights
    
resetBarLights:
    movlw d'20' 
    cpfsgt barPosition ; barPosition  = 20
    bcf LATA,5
    cpfsgt barPosition ; barPosition  = 20
    bcf LATB,5
    movlw d'21'
    subwf barPosition, 0 ; store in W
    btfsc STATUS,Z ; barPosition = 21
    bcf LATB,5
    btfsc STATUS,Z ; barPosition = 21
    bcf LATC,5
    movlw d'22'
    cpfslt barPosition 
    bcf LATC,5 ;barPosition = 22
    cpfslt barPosition 
    bcf LATD,5 ;barPosition = 22
    btfsc barMoveDirection,0 ; 0 = move left ;; 1 = move right
    incf barPosition
    btfss barMoveDirection,0 ; 0 = move left ;; 1 = move right
    decf barPosition
    goto lightTheBar
    
lightTheBar:
    ;TODO light the bar
    ; only the 5th light of A-F will be on (don't forget to close the previous light positions)
    
case20:     ; case for bar=20
        movlw d'20'
        cpfseq barPosition
        goto case21
        bsf LATA,5
        bsf LATB,5
        call checkBall1
        call checkBall2
        call checkBall3
        call checkBall4
        call checkBall5
        call checkBall6
        return
case21:     ; case for bar=21
        movlw d'21'
        cpfseq barPosition
        goto case22
        bsf LATB,5
        bsf LATC,5
        call checkBall1
        call checkBall2
        call checkBall3
        call checkBall4
        call checkBall5
        call checkBall6
        return

case22:     ; case for bar=22
        movlw d'22'
        cpfseq barPosition
        goto case20
        bsf LATC,5
        bsf LATD,5
        call checkBall1
        call checkBall2
        call checkBall3
        call checkBall4
        call checkBall5
        call checkBall6
        return
    
    
; move the active balls
    ; -> if "ball update period" +-100ms passed
    ;   -> for each active ball(can find with "15bit active balls")
    ;       -> add 6 to the corresponding "6bit ball location"
    ; -> goto <check active balls>

    ; check active balls
    ; -> for each active ball(can find with "15bit active balls")
    ;   -> if "6bit ball location" >= 36, i.e ball hasn't been caught
    ;       -> decrement "health" and update 7segment display
    ;       -> if "health" == 0, goto <restart> ----------- <- may also goto <lose> if we want to wait for sometime
    ;       -> deactivate the ball by updating "15bit active balls"
    ;   -> else if "6bit ball location" >=30 & ball is on the bar, i.e ball has been caught
    ;       -> deactivate the ball by updating "15bit active balls"
    ; -> if "15 bit active balls" == 0, goto <next level>
    ; -> goto <create the balls>

    ; create the balls
    ; -> if we need to create more balls and  "ball update period" ms passed
    ;   -> find a non active ball index from "15bit active balls". (can use "number of spawned balls")
    ;       -> Set that index to 1.
    ;       -> Set the "6bit ball location" corresponding to that index to [0,5] based on "timer1 starting value" & timer0
    ;       -> increase "number of spawned balls" by 1
    ; -> goto <move the bar>
ballUpdate:
    btfss   timer0_state, 0 ; if enough time hasn't passed, return
    goto    main_loop
    clrf    timer0_state    ; enough time has passed rearrange timer and move balls
    decf    numberOfBallsToCreate
    btfss   STATUS, Z               ;Is the result Zero?
    goto    skip_level_configuration
    incf    level 
    movf    level, W
    sublw   d'4'; if level is four
    btfsc   STATUS, Z               ;Is the result Zero?
    goto    idle;
    movf    level, W
    call    level_table
    movwf   numberOfBallsToCreate
    movlw   b'00000010'
    movwf   LATH
    movf    level, W
    call    segment_table
    movwf   LATJ
skip_level_configuration:
    movf    level, W
    call    timer0_table
    movwf   timer0_counter
    ;update balls
    btfsc activeBalls, 0 ; if ball not active, skip
    call ball1Update
    btfsc activeBalls, 1 ; if ball not active, skip
    call ball2Update
    btfsc activeBalls, 2 ; if ball not active, skip
    call ball3Update
    btfsc activeBalls, 3 ; if ball not active, skip
    call ball4Update
    btfsc activeBalls, 4 ; if ball not active, skip
    call ball5Update
    btfsc activeBalls, 5 ; if ball not active, skip
    call ball6Update
    ;create a new ball
    movlw b'00000011' ; for modulo
    andwf timer1_initial_value, 0; store in W
    movwf timer1Modulo
    movf level, W
    call level_shift_table
    movwf iterator
timer_shitf_loop:
    rrcf timer1_initial_value
    decfsz iterator
    goto timer_shitf_loop
    btfsc activeBalls, 0 ; if ball is not active, skip
    goto createBall2
    bsf activeBalls, 0
    movff timer1Modulo, ball1Position
    goto finish_ball_creation
createBall2:
    btfsc activeBalls, 1 ; if ball is not active, skip
    goto createBall3
    bsf activeBalls, 1
    movff timer1Modulo, ball2Position
    goto finish_ball_creation
createBall3:
    btfsc activeBalls, 2 ; if ball is not active, skip
    goto createBall4
    bsf activeBalls, 2
    movff timer1Modulo, ball3Position
    goto finish_ball_creation
createBall4:
    btfsc activeBalls, 3 ; if ball is not active, skip
    goto createBall5
    bsf activeBalls, 3
    movff timer1Modulo, ball4Position
    goto finish_ball_creation
createBall5:
    btfsc activeBalls, 4 ; if ball is not active, skip
    goto createBall6
    bsf activeBalls, 4
    movff timer1Modulo, ball5Position
    goto finish_ball_creation
createBall6:
    btfsc activeBalls, 5 ; if ball is not active, skip
    goto finish_ball_creation
    bsf activeBalls, 5
    movff timer1Modulo, ball6Position
finish_ball_creation:
    call light_balls
    goto main_loop
openCreatedBallLights
    movlw 0
    cpfsgt timer1Modulo ; if timer1Modulo <= 0 LATA 
    bsf LATA,0
    cpfsgt timer1Modulo
    return
    movlw d'1'
    cpfsgt timer1Modulo ; if timer1Modulo <= 1 LATB
    bsf LATB,0
    cpfsgt timer1Modulo
    return
    movlw d'2'
    cpfsgt timer1Modulo ; if timer1Modulo <= 2 LATC
    bsf LATC,0
    cpfsgt timer1Modulo
    return
    ; ELSE === if timer1Modulo <= 3 LATD
    bsf LATD,0
    return
    
ball1Update
    movlw d'4'
    addwf ball1Position
    call checkBall1
    btfss activeBalls, 0 ; if the ball is caught return
    return
    movlw d'24'
    cpfslt ball1Position ; if ball position <24 don't decrease health
    call decreaseHealth
    movlw d'24'
    cpfslt ball1Position ; if ball position <24 don't deactive the ball
    bcf activeBalls, 0
    return

ball2Update
    movlw d'4'
    addwf ball2Position
    call checkBall2
    btfss activeBalls, 1 ; if the ball is caught return
    return
    movlw d'24'
    cpfslt ball2Position ; if ball position <24 don't decrease health
    call decreaseHealth
    movlw d'24'
    cpfslt ball2Position ; if ball position <24 don't deactive the ball
    bcf activeBalls, 1
    return

ball3Update
    movlw d'4'
    addwf ball3Position
    call checkBall3
    btfss activeBalls, 2 ; if the ball is caught return
    return
    movlw d'24'
    cpfslt ball3Position ; if ball position <24 don't decrease health
    call decreaseHealth
    movlw d'24'
    cpfslt ball3Position ; if ball position <24 don't deactive the ball
    bcf activeBalls, 2
    return
   
ball4Update
    movlw d'4'
    addwf ball4Position
    call checkBall4
    btfss activeBalls, 3 ; if the ball is caught return
    return
    movlw d'24'
    cpfslt ball4Position ; if ball position <24 don't decrease health
    call decreaseHealth
    movlw d'24'
    cpfslt ball4Position ; if ball position <24 don't deactive the ball
    bcf activeBalls, 3
    return 
    
ball5Update
    movlw d'4'
    addwf ball5Position
    call checkBall5
    btfss activeBalls, 4 ; if the ball is caught return
    return
    movlw d'24'
    cpfslt ball5Position ; if ball position <24 don't decrease health
    call decreaseHealth
    movlw d'24'
    cpfslt ball5Position ; if ball position <24 don't deactive the ball
    bcf activeBalls, 4
    return
    
ball6Update
    movlw d'4'
    addwf ball6Position
    call checkBall6
    btfss activeBalls, 5 ; if the ball is caught return
    return
    movlw d'24'
    cpfslt ball6Position ; if ball position <24 don't decrease health
    call decreaseHealth
    movlw d'24'
    cpfslt ball6Position ; if ball position <24 don't deactive the ball
    bcf activeBalls, 5
    return
    

decreaseHealth
    dcfsnz health, 1
    goto idle
    movlw b'00000001'
    movwf LATH
    movf health, 0
    call segment_table
    movwf LATJ
    return
    
; next level
; if "level" == 3, goto <restart>
; set "number of spawned balls" to 0
; increment "level" by 1 and update 7segment display
; set "ball update period" to its new value
; goto <move the bar>

; restart 
; set "number of spawned balls" to 0
; set "15 bit active balls" to 0
; set "level" to 1 and update 7segment display
; set "health" to 5 and update 7segment display
; set "ball update period" to its new value
; set RG0 to 0
; goto <start>

; lose(OPTIONAL?)
; wait for some
; goto <restart>


idle:  ; restart part is here 
    bcf	INTCON, 7   ;disable interrupts
    bcf T0CON, 7    ;disable Timer0 by setting TMR0ON to 0
    movlw d'61'; 
    movwf TMR0L; 
    movlw d'100'; 
    movwf timer0_counter
    clrf timer0_state
    clrf activeBalls
    clrf pressed
    movlw d'1'
    movwf level
    movlw d'5'
    movwf health
    ; TODO bar position should be reset
    clrf LATG
    ;7-segent display for health
    movlw b'00000001'
    movwf LATH
    movf health, 0
    call segment_table
    movwf LATJ
    ;7-segent display for level
    movlw   b'00000010'
    movwf   LATH
    movf    level, W
    call    segment_table
    movwf   LATJ
    goto wait_rg0_press
main
    call initialize
    goto idle
main_loop:
    call moveTheBar
    movlw d'0'
    cpfsgt health ; if health > 0 skip
    goto idle 
    goto ballUpdate
    goto main_loop
    END
