;
; Alexander_Uong_Lab7_sourcecode.asm
;
; Created: 3/1/2021 4:58:37 PM
; Author : Alex Uong
;

;***********************************************************
;*
;*	Alexander_Uong_Lab7_sourcecode.asm
;*
;*	Utilizes Timers/Counters configure and use the 8-bit Timer/Counters on the ATmega128
;* to generate pulse-width modulation (PWM) signals.
;*
;*	This is the skeleton file for Lab 7 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Alexander Uong
;*	   Date: 3/1/2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	speedLevels = r17		; register keeping track of speedLevel
.def	flagRegister = r18		;register to clear interrupt flags
.def	speedRegister = r19		;register to handle speed functions


.equ	minimum = 0				;defined as 0 for 0% duty cycle
.equ	maximum = 255			;defined as 255 for 0% duty cycle

.equ	EngEnR = 4				; right Engine Enable Bit
.equ	EngEnL = 7				; left Engine Enable Bit
.equ	EngDirR = 5				; right Engine Direction Bit
.equ	EngDirL = 6				; left Engine Direction Bit

.equ	initialOutput = 0b01101111	;initial configuration of LED's


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

.org	$0002
		rcall decreaseSpeed		//calls decreaseSpeed subroutine if pin0 is pressed
		reti
		
.org	$0004
		rcall increaseSpeed		//calls increaseSpeed subroutine if pin1 is pressed
		reti

.org	$0006
		rcall lowestSpeed		//calls lowestSpeed subroutine if pin2 is pressed
		reti

.org	$0008
		rcall maximumSpeed		//calls maximumSpeed subroutine if pin3 is pressed
		reti

.org	$0046					; end of interrupt vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:



		; Initialize the Stack Pointer
		LDI mpr, LOW(RAMEND)	; Low Byte of End SRAM Address
		OUT SPL, mpr			; Write Byte to SPL
		LDI mpr, HIGH(RAMEND)	; High Byte of End SRAM Address
		OUT SPH, mpr			; Write Byte to SPH


		; Configure I/O ports

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

		; Configure External Interrupts, if needed

		; Set the Interrupt Sense Control to falling edge 
		ldi mpr,0b10101010
		sts EICRA, mpr

		; Configure the External Interrupt Mask
		ldi mpr, 0b00001111		//masks pins 0 through 3
		out EIMSK, mpr

		; Configure 8-bit Timer/Counters
		ldi mpr, 0b01101001		//fast pwm mode, non inverting mode, no prescaling
		out TCCR0, mpr
		out TCCR2, mpr

		;initially set OCR0 AND OCR2 equal to zero, or 0% duty cycle (this means full speed)
		clr mpr					
		out OCR0, mpr
		out OCR2, mpr

		; initial LED output (pins 0-3, 5-6 are illuminated)
		ldi mpr, initialOutput
		out PORTB, mpr


		; Enable global interrupts (if any are used)
		sei

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		; poll Port D pushbuttons (if needed)

								; if pressed, adjust speed
								; also, adjust speed indication

		rjmp	MAIN			; return to top of MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func:	decreaseSpeed
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;----------------------------------------------------------

decreaseSpeed:

	cpi speedRegister, maximum	//compare speedRegister with value 255
	breq minSpeed				//if speedRegister is equal to 255, that means its already at min speed or 100% duty cycle. so branch to minSpeed if equal
	dec speedLevels				//decrement speedLevel
	
	ldi mpr, 17					//load 17 into mpr. Each level differs by 17
	add speedRegister, mpr		//add 17 to speedRegister

	out OCR0, speedRegister		//store register to OCR0 and OCR2 to modify pin 4 and pin 7 brightness
	out OCR2, speedRegister
	
	in mpr, PORTB				//load PORTB into mpr
	andi mpr, 0b11110000		//logical and with immediate to mask out pins of PORTB
	or mpr, speedLevels			//logical or between mpr and speedLevels
	out PORTB, mpr				//store register to output PORTB
	
	;clear flag register
	ldi flagRegister, 0b00001111		//Setting the respective bit places to 1 resets the flag register contents
	out EIFR, flagRegister				//this is to avoid queued interrupts


minSpeed:

	clr mpr
	ret


;-----------------------------------------------------------
; Func:	increaseSpeed
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
increaseSpeed:

	cpi speedRegister, minimum	//compare speedRegister with value 0
	breq maxSpeed				//if speedRegister is equal to 0, that means its already at max speed or 0% duty cycle. so branch to maxSpeed if equal
	inc speedLevels				//increment speedLevels otherwise
	
	ldi mpr, 17					//load 17 into mpr. Each level differs by 17
	sub speedRegister, mpr		//subtract 17 from speedRegister. This lowers brightness 

	out OCR0, speedRegister		//store register to OCR0 and OCR2 to modify pin 4 and pin 7 brightness
	out OCR2, speedRegister
	
	in mpr, PORTB				//load PORTB into mpr
	andi mpr, 0b11110000		//logical and with immediate to mask out pins of PORTB
	or mpr, speedLevels			//logical or between mpr and speedLevels
	out PORTB, mpr				//store register to output PORTB
	
	
	;clear flag register
	ldi flagRegister, 0b00001111		//Setting the respective bit places to 1 resets the flag register contents
	out EIFR, flagRegister				//this is to avoid queued interrupts
	
maxSpeed:

	clr mpr
	ret



;-----------------------------------------------------------
; Func:	lowestSpeed
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
lowestSpeed:

	ldi speedRegister, maximum		//100% duty cycle->halt. load into speedRegister
	out OCR0, speedRegister			//store into OCR0 and OCR2
	out OCR2, speedRegister

	in mpr, PORTB			//load PORTB into mpr
	andi mpr, 0b11110000	//logical and with immediate to mask out pins of PORTB
	ori mpr, 0				//logical or between mpr and 0
	out PORTB, mpr			//store register to output PORTB
	

	;clear flag register
	ldi flagRegister, 0b00001111		//Setting the respective bit places to 1 resets the flag register contents
	out EIFR, flagRegister				//this is to avoid queued interrupts


	ret



;-----------------------------------------------------------
; Func:	maximumSpeed
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
maximumSpeed:
	ldi speedRegister, minimum		//0% duty cycle->full speed. load into speedRegister
	out OCR0, speedRegister			//store into OCR0 and OCR2
	out OCR2, speedRegister

	
	
	in mpr, PINB					//load PORTB into mpr
	andi mpr, 0b11110000			//AND to mask out first 4 bits of PORTB
	ori mpr, 15						//logical or between mpr and 15
	out PORTB, mpr					//store register to output PORTB
	

	;clear flag register
	ldi flagRegister, 0b00001111		;Setting the respective bit places to 1 resets the flag register contents
	out EIFR, flagRegister				;this is to avoid queued interrupts


	ret



;***********************************************************
;*	Stored Program Data
;***********************************************************
		; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
		; There are no additional file includes for this program
