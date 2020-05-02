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
health res 1; 7 segment display of health is at portH.0 -> b'00000001' = 1
level res 1; 7 segment display of level is at portH.1 -> b'00000010' = 2
timer1StartingVal res 1
numberOfSpawnedBalls res 1
activeBallsSet1 res 1 ; 5 balls. only use rightmost 5 bits
activeBallsSet2 res 1 ; 5 balls. only use rightmost 5 bits after level-1
activeBallsSet3 res 1 ; 5 balls. only use rightmost 5 bits after level-2
ball1Position res 1 ; 0 indicates top left, 23 indicates bottom right. Add 4 per update.
ball2Position res 1
ball3Position res 1 
ball4Position res 1
ball5Position res 1 
ball6Position res 1
ball7Position res 1 
ball8Position res 1
ball9Position res 1 
ball10Position res 1
ball11Position res 1 
ball12Position res 1
ball13Position res 1 
ball14Position res 1
ball15Position res 1
barPosition res 1 ; Leftmost bar position. Between 20-22(inclusive) for easier comparison. 20 means bar is at 20 and 21'st points. 22 means bar is at 22nd and 23th points.
 
;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    main                   ; go to beginning of program

;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************
	
    
    
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
    ;setup ports, inputs-outputs etc.
    movlw 0x0F
    movwf ADCON1 ; set A/D conversion
    movlw 0x07 ; RG0-RG1-RG2(1+2+4=7) are input 
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
    movlw 5
    movwf health
    movlw 1
    movwf level
    movlw 1
    movwf TRISH ; enable first 7segment display for setting health
    movlw b'01101101' ;5 for 7segment display
    movwf LATJ ; TODO we may need to movwf to TRISJ instead
    nop ;it says wait a while on the hw pdf
    clrf TRISH
    clrf LATJ
    movlw 2
    movwf TRISH ;enable second 7segment display for setting level
    movlw b'00000110' ; 1 for 7segment display
    movwf LATJ ; TODO we may need to movwf to TRISJ instead
    nop ;it says wait a while on the hw pdf
    clrf TRISH
    clrf LATJ
    ;TODO set timer1 start value
    clrf numberOfSpawnedBalls
    clrf activeBallsSet1
    clrf activeBallsSet2
    clrf activeBallsSet3
    movlw 20
    movwf barPosition
    mowlw b'00100000'
    movwf LATA ; light the bar
    movwf LATB ; light the bar
    return

; start
; -> if RG0 is never pressed goto start
; -> if pressed before, goto <move the bar>
; -> save timer1 value(16 bit)
; -> goto <move the bar>
start
    btfss PORTG,0 ; if RG0 is pressed break from start loop
    goto start
    ;TODO save timer1 value
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
	movlw 22
	cpfslt barPosition ; skip if we are already on the rightmost position
	return ;(barPosition=22)
	incf barPosition
	goto lightTheBar
	
    moveLeft:
	movlw 20
	cpfsgt barPosition ;skip if we are already on the leftmost position
	return ;(barPosition=20)
	decf barPosition
	goto lightTheBar
	
    lightTheBar:
	;TODO light the bar
	mowlw b'00100000' ; only the 5th light of A-F will be on (don't forget to close the previous light positions)
	
	return
	
; move the active balls
    ; -> if "ball update period" +-100ms passed
    ;	-> for each active ball(can find with "15bit active balls")
    ;	    -> add 6 to the corresponding "6bit ball location"
    ; -> goto <check active balls>

    ; check active balls
    ; -> for each active ball(can find with "15bit active balls")
    ;	-> if "6bit ball location" >= 36, i.e ball hasn't been caught
    ;	    -> decrement "health" and update 7segment display
    ;	    -> if "health" == 0, goto <restart>	----------- <- may also goto <lose> if we want to wait for sometime
    ;	    -> deactivate the ball by updating "15bit active balls"
    ;	-> else if "6bit ball location" >=30 & ball is on the bar, i.e ball has been caught
    ;	    -> deactivate the ball by updating "15bit active balls"
    ; -> if "15 bit active balls" == 0, goto <next level>
    ; -> goto <create the balls>

    ; create the balls
    ; -> if we need to create more balls and  "ball update period" ms passed
    ;	-> find a non active ball index from "15bit active balls". (can use "number of spawned balls")
    ;	    -> Set that index to 1.
    ;	    -> Set the "6bit ball location" corresponding to that index to [0,5] based on "timer1 starting value" & timer0
    ;	    -> increase "number of spawned balls" by 1
    ; -> goto <move the bar>

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
    
main
    call initialize
    call start
    loop:
	call moveTheBar
	
    goto loop
    END
