;***********************************************************
;*
;*	Alexander_Uong_Lab6_sourcecode.asm
;*
;*	BumpBot program supporting external interrupts. Added functionality includes 
;*	displaying on LCD the amount of times rightwhisker and left whisker are pressed
;*	and adding functionality to pins 2 and 3, clearing the right and left whisker respectively
;*	This is the skeleton file for Lab 6 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Alexander Uong
;*	   Date: 2/21/2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register 
.def	ilcnt = r23				; Inner Loop Counter
.def	olcnt = r24				; Outer Loop Counter
	
.def	rightwhiskercounter = r5
.def	leftwhiskercounter =  r6
.def	botregister = r25		;register used in bumpbot functions

.equ	WTime = 100				; Time to wait in wait loop

.equ	WskrR = 0				; Right Whisker Input Bit
.equ	WskrL = 1				; Left Whisker Input Bit

//.equ	EngEnR = 4				; Right Engine Enable Bit
//.equ	EngEnL = 7				; Left Engine Enable Bit
.equ	EngDirR = 5				; Right Engine Direction Bit
.equ	EngDirL = 6				; Left Engine Direction Bit

.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command
.equ	MovBck = $00				; Move Backward Command
.equ	TurnR = (1<<EngDirL)			; Turn Right Command
.equ	TurnL = (1<<EngDirR)			; Turn Left Command
//.equ	Halt = (1<<EngEnR|1<<EngEnL)		; Halt Command
;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$0002					;if pin 0 is pressed
		rcall HitRight			
		reti

.org	$0004					;if pin 1 is pressed
		rcall HitLeft
		reti

.org	$0006					;if pin 2 is pressed
		rcall ClrRight
		reti

.org	$0008					;if pin 3 is pressed
		rcall ClrLeft
		reti

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		LDI mpr, LOW(RAMEND)	; Low Byte of End SRAM Address
		OUT SPL, mpr			; Write Byte to SPL
		LDI mpr, HIGH(RAMEND)	; High Byte of End SRAM Address
		OUT SPH, mpr			; Write Byte to SPH
		
		; Initialize Port B for output
		ldi mpr, $FF	;Set port b data direction register
		out DDRB, mpr	;set for output
		ldi mpr, $00	;initialize port b data register
		out PORTB, mpr	;all port b outputs are now low
		
		; Initialize Port D for input
		ldi mpr, $00	;set port d data direction register
		out DDRD, mpr	;set for input
		ldi mpr, $FF	;initialize port d data register
		out PORTD, mpr	;all port d inputs are tri-state

		; Initialize external interrupts
		; Set the Interrupt Sense Control to falling edge 
		ldi mpr,0b10101010
		sts EICRA, mpr


		; Configure the External Interrupt Mask
		ldi mpr, 0b00001111
		out EIMSK, mpr

		;Initialize LCD Display
		rcall LCDInit

		;Initialize leftwhiskercounter to 0 and display on first line of LCD
		clr rightwhiskercounter
		mov mpr, rightwhiskercounter		;copy rightwhiskercounter into mpr
	
		ldi XL, LOW(LCDLn1Addr)				;X register points to address of line1 of the LCD
		ldi XH, HIGH(LCDLn1Addr)
		
		st X, mpr							;store mpr into x register
		rcall Bin2ASCII						;Bin2ASCII converts binary number of ascii equivalent

		
		;Initialize rightwhiskercounter to 0 and display on second line of LCD
		clr	leftwhiskercounter 
		mov mpr, leftwhiskercounter		;copy leftwhiskercounter into mpr
		
		ldi XL, LOW(LCDLn2Addr)			;X register points to address of line2 of the LCD
		Ldi XH, High(LCDLn2Addr)


		st X, mpr						;store mpr into x register
		rcall Bin2ASCII					;Bin2ASCII converts binary number of ascii equivalent

		; Turn on interrupts
			; NOTE: This must be the last thing to do in the INIT function
		sei

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program

		; TODO: ???
		ldi botregister, MovFwd		;load the MoveFwd functionality into the bot register
		out PORTB, botregister		;store this into output PORTB
		rcall LCDWrite				;LCD driver that writes contents stored onto LCD Display

		rjmp	MAIN			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
;	You will probably want several functions, one to handle the 
;	left whisker interrupt, one to handle the right whisker 
;	interrupt, and maybe a wait function
;------------------------------------------------------------

;----------------------------------------------------------------
; Sub:	HitRight
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------

HitRight:
		push	botregister			; Save mpr register
		push	wait			; Save wait register
		in		botregister, SREG	; Save program state
		push	botregister			;

		; Move Backwards for a second
		ldi		botregister, MovBck	; Load Move Backward command
		out		PORTB, botregister	; Send command to port
		ldi		wait, WTime	; Wait for 1 second
		rcall	WaitFunc			; Call wait function

		; Turn left for a second
		ldi		botregister, TurnL	; Load Turn Left Command
		out		PORTB, botregister	; Send command to port
		ldi		wait, WTime	; Wait for 1 second
		rcall	WaitFunc			; Call wait function

		inc rightwhiskercounter		;increments the rightwhiskercounter
		rcall UpdateRight			;call to updateright function below

		;clear flag register
		ldi botregister, 0b00001111		;Setting the respective bit places to 1 resets the flag register contents
		out EIFR, botregister			;this is to avoid queued interrupts

		pop		botregister		; Restore program state
		out		SREG, botregister	;
		pop		wait		; Restore wait register
		pop		botregister		; Restore mpr
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	HitLeft
; Desc:	Handles functionality of the TekBot when the left whisker
;		is triggered.
;----------------------------------------------------------------
HitLeft:
		push	botregister			; Save mpr register
		push	wait			; Save wait register
		in		botregister, SREG	; Save program state
		push	botregister			;

		; Move Backwards for a second
		ldi		botregister, MovBck	; Load Move Backward command
		out		PORTB, botregister	; Send command to port
		ldi		wait, WTime	; Wait for 1 second
		rcall	WaitFunc			; Call wait function

		; Turn right for a second
		ldi		botregister, TurnR	; Load Turn Left Command
		out		PORTB, botregister	; Send command to port
		ldi		wait, WTime	; Wait for 1 second
		rcall	WaitFunc			; Call wait function

		inc leftwhiskercounter		;incrmeents leftwhiskercounter
		rcall UpdateLeft			;call to updateleft function below

		;clear flag register
		ldi botregister, 0b00001111		;Setting the respective bit places to 1 resets the flag register contents
		out EIFR, botregister			;this is to avoid queued interrupts


		pop		botregister		; Restore program state
		out		SREG, botregister	;
		pop		wait		; Restore wait register
		pop		botregister		; Restore mpr
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	WaitFunc
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;----------------------------------------------------------------
WaitFunc:
		push	wait			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt		; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		wait		; Decrement wait 
		brne	Loop			; Continue Wait loop	

		pop		olcnt		; Restore olcnt register
		pop		ilcnt		; Restore ilcnt register
		pop		wait		; Restore wait register
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	UpdateRight
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
UpdateRight:
	rcall LCDClrLn1		;LCD driver clear line 1 of the display
	
	clr mpr							;clear mpr register
	mov mpr, rightwhiskercounter	;copy rightwhiskercounter into mpr register
	

	ldi XL, LOW(LCDLn1Addr)			;Set the X register to point to address of line1 of LCD display
	ldi XH, HIGH(LCDLn1Addr)
		
	st X, mpr						;store mpr into x register
	rcall Bin2ASCII					;call to Bin2ASCII

	ret
;----------------------------------------------------------------
; Sub:	UpdateLeft
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
UpdateLeft:
	rcall LCDClrLn2					;LCD driver clear line 2 of the display
	
	clr mpr							;clear mpr register
	mov mpr, leftwhiskercounter		;copy rightwhiskercounter into mpr register
	

	ldi XL, LOW(LCDLn2Addr)			;Set the X register to point to address of line2 of LCD display
	ldi XH, HIGH(LCDLn2Addr)
		
	st X, mpr						;store mpr into x register
	rcall Bin2ASCII					;call to Bin2ASCII

	ret

;----------------------------------------------------------------
; Sub:	ClrRight
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
ClrRight:
	push mpr						;push mpr onto stack
	rcall LCDClrLn1					;LCD driver clear line 1 of the display
	
	clr rightwhiskercounter			;clear rightwhiskercounter
	clr mpr							;clear mpr
	
	ldi XL, LOW(LCDLn1Addr)			;Set the X register to point to address of line1 of LCD display
	ldi XH, HIGH(LCDLn1Addr)
		
	st X, mpr						;store mpr into x register
	rcall Bin2ASCII					;call to Bin2ASCII
	
	ldi mpr, 0b00001111		;Setting the respective bit places to 1 resets the flag register contents
	out EIFR, mpr			;this is to avoid queued interrupts
	pop mpr					;pop mpr to restore value
	
	ret
	
	
;----------------------------------------------------------------
; Sub:	ClrLeft
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
ClrLeft:
	push mpr						;push mpr onto stack
	rcall LCDClrLn2					;LCD driver clear line 2 of the display
	
	clr leftwhiskercounter			;clear leftwhiskercounter
	clr mpr							;clear mpr

	ldi XL, LOW(LCDLn2Addr)			;Set the X register to point to address of line2 of LCD display
	ldi XH, HIGH(LCDLn2Addr)
		
	st X, mpr						;store mpr into x register
	rcall Bin2ASCII					;call to Bin2ASCII
	
	
	ldi mpr, 0b00001111		;Setting the respective bit places to 1 resets the flag register contents
	out EIFR, mpr			;this is to avoid queued interrupts
	pop mpr					;pop mpr to restore value
	
	
	ret


;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

