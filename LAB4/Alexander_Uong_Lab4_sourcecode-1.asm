;
; Lab4.asm
;
; Created: 2/1/2021 11:17:13 PM
; Author : Alex Uong
;

;***********************************************************
;*
;*	main.asm
;*
;*	Transfers strings from program memory to data memory, displaying the contents of the strings initially 
;*	in program memory, onto the LCD Display of the ATMEGA128 Board
;*
;*	This is the skeleton file for Lab 4 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Alexander Uong
;*	   Date: 2/1/2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file


;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register is
								; required for LCD Driver

.def counterRegister = r24		;multipurpose register to serve as counter


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		
		; Initialize Stack Pointer
		LDI R16, LOW(RAMEND)	; Low Byte of End SRAM Address
		OUT SPL, R16			; Write Byte to SPL
		LDI R16, HIGH(RAMEND)	; High Byte of End SRAM Address
		OUT SPH, R16			; Write Byte to SPH
		
		; Initialize Port D for input
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State


		; Initialize LCD Display
		rcall LCDInit ; Initializes LCD peripheral interface
		
		; Move strings from Program Memory to Data Memory

		ldi ZL, LOW(STRING_NAME_BEG << 1); Z register pointer points to low byte of string data in program memory
		ldi ZH, HIGH(STRING_NAME_BEG << 1); Z register pointer points to high byte of string data in program memory

		ldi YL, LOW(LCDLn1Addr)			  ; Set Y register to point to low byte destination of Line 1 of the LCD Display. From LCDDriver
		ldi YH, HIGH(LCDLn1Addr)		  ; Set Y register to point to high byte destination of Line 1 of the LCD Display. From LCDDriver
		
		ldi counterRegister, 14					 ;load the constant value 14 into register r24, with 14 being the # of characters in my name

LINE1:	//This loads the contents of STRING_NAME_BEG one character at a time
		lpm mpr, Z+ ; load data from z register into register r16. Post increment Z to point to next character in string
		st Y+,mpr   ; store data from r16 into data memory (LCD Display line1). Post increment to move to next location in data memory
		DEC counterRegister	; decrement loop counter	
		BRNE LINE1	; branch if not equal, remains in LOOP if counter is still greater than 0

		//Setting the Z register to point to the hello world string now that the first string has been loaded into the LCD

		ldi ZL, LOW(HELLOWORLD_BEG << 1); Z register pointer points to lower byte of string data in program memory
		ldi ZH, HIGH(HELLOWORLD_BEG << 1); Z register pointer points to higher bytes of string data in program memory

		ldi YL, LOW(LCDLn2Addr)			; Set Y register to point to low byte destination of Line 2 of the LCD Display. From LCDDriver
		ldi YH, HIGH(LCDLn2Addr)		; Set Y register to point to high byte destination of Line 2 of the LCD Display. From LCDDriver

		ldi counterRegister, 12					;load the constant value 12 into register r24, with 12 being the # of characters in the string "Hello, World"

LINE2:
		lpm mpr, Z+ ; load data from z register into register r16. Post increment Z to point to next character in string
		st Y+,mpr   ; store data from r16 into data memory (LCD Display line2). Post increment to move to next location in data memory
		DEC counterRegister	; decrement loop counter	
		BRNE LINE2	; branch if not equal, remains in LOOP if counter is still greater than 0


		
		; NOTE that there is no RET or RJMP from INIT, this
		; is because the next instruction executed is the
		; first instruction of the main program

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program
		; Display the strings on the LCD Display
		
		IN mpr,PIND			; Get whisker input from PORT D
		andi mpr, (0b11111110|0b11111101 |0b01111111)	//logical and
		CPI	mpr, (0b11111110)	;Check if PD0 is pressed
		BRNE NEXT				;Continue with next check
		rcall INITIALIZE		;Copy strings through moving the strings from program to data memory. This is to copy the strings to the LCD after LCDClr
		rcall LCDWrite			;Writes contents to the LCD Display
		rjmp  MAIN				;Continue with the program
NEXT:
		
		CPI	mpr, (0b11111101)	;Check if PD1 is pressed
		BRNE NEXT2				;continue with next2 check
		rcall SWAPSTRINGS		;Calls subroutine swapstrings
		rcall LCDWrite			;Writes contents to the LCD Display
		rjmp  MAIN				;Continue with the program
NEXT2:
		CPI mpr, (0b01111111)		;Check if PD7 is pressed
		BRNE MAIN					;Continues back to main
		rcall LCDClr				;Clears display 
		rjmp MAIN					;Continue with the program



;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: SWAPSTRINGS
; Desc: Swaps line 1 and line 2 on lcd display by copying the strings in reverse order
;		
;-----------------------------------------------------------
SWAPSTRINGS:							; Begin a function with a label
		; Save variables by pushing them to the stack
		; Execute the function here
		; Restore variables by popping them from the stack,
		; in reverse order
		rcall LCDClr					;Clears both lines of data and lines on LCD display
		ldi ZL, LOW(HELLOWORLD_BEG << 1); Z register pointer points to low byte of string data in program memory
		ldi ZH, HIGH(HELLOWORLD_BEG << 1); Z register pointer points to high byte of string data in program memory

		ldi YL, LOW(LCDLn1Addr)			  ; Set Y register to point to low byte destination of Line 1 of the LCD Display. From LCDDriver
		ldi YH, HIGH(LCDLn1Addr)		  ; Set Y register to point to high byte destination of Line 1 of the LCD Display. From LCDDriver
		
		ldi counterRegister, 12			  ;load the constant value 12 into register r24, with 12 being the # of characters in the string "Hello, World"

LINEONE:	//This loads the contents of HELLOWORLD_BEG one character at a time
		lpm mpr, Z+ ; load data from z register into register r16. Post increment Z to point to next character in string
		st Y+,mpr   ; store data from r16 into data memory (LCD Display line1). Post increment to move to next location in data memory
		DEC counterRegister	; decrement loop counter	
		BRNE LINEONE	; branch if not equal, remains in LOOP if counter is still greater than 0

		//Setting the Z register to point to the hello world string now that the first string has been loaded into the LCD

		ldi ZL, LOW(STRING_NAME_BEG << 1); Z register pointer points to lower byte of string data in program memory
		ldi ZH, HIGH(STRING_NAME_BEG << 1); Z register pointer points to higher bytes of string data in program memory

		ldi YL, LOW(LCDLn2Addr)			; Set Y register to point to low byte destination of Line 2 of the LCD Display. From LCDDriver
		ldi YH, HIGH(LCDLn2Addr)		; Set Y register to point to high byte destination of Line 2 of the LCD Display. From LCDDriver

		ldi counterRegister, 14			;load the constant value 14 into register r24, with  being the # of characters in my name
		

LINETWO:
		lpm mpr, Z+ ; load data from z register into register r16. Post increment Z to point to next character in string
		st Y+,mpr   ; store data from r16 into data memory (LCD Display line2). Post increment to move to next location in data memory
		DEC counterRegister	; decrement loop counter	
		BRNE LINETWO	; branch if not equal, remains in LOOP if counter is still greater than 0

		ret						; End a function with RET
;----------------------------------------------------------------
; Sub:	INITIALIZE
; Desc:	Copies the strings from program memory into data memory
;		
;----------------------------------------------------------------

INITIALIZE:
		rcall LCDClr					 ;Clears both lines of data and lines on LCD display
		ldi ZL, LOW(STRING_NAME_BEG << 1); Z register pointer points to low byte of string data in program memory
		ldi ZH, HIGH(STRING_NAME_BEG << 1); Z register pointer points to high byte of string data in program memory

		ldi YL, LOW(LCDLn1Addr)			  ; Set Y register to point to low byte destination of Line 1 of the LCD Display. From LCDDriver
		ldi YH, HIGH(LCDLn1Addr)		  ; Set Y register to point to high byte destination of Line 1 of the LCD Display. From LCDDriver
		
		ldi counterRegister, 14					 ;load the constant value 14 into register r24, with 14 being the # of characters in my name

LINE1_1:	//This loads the contents of STRING_NAME_BEG one character at a time
		lpm mpr, Z+ ; load data from z register into register r16. Post increment Z to point to next character in string
		st Y+,mpr   ; store data from r16 into data memory (LCD Display line1). Post increment to move to next location in data memory
		DEC counterRegister	; decrement loop counter	
		BRNE LINE1_1	; branch if not equal, remains in LOOP if counter is still greater than 0

		//Setting the Z register to point to the hello world string now that the first string has been loaded into the LCD

		ldi ZL, LOW(HELLOWORLD_BEG << 1); Z register pointer points to lower byte of string data in program memory
		ldi ZH, HIGH(HELLOWORLD_BEG << 1); Z register pointer points to higher bytes of string data in program memory

		ldi YL, LOW(LCDLn2Addr)			; Set Y register to point to low byte destination of Line 2 of the LCD Display. From LCDDriver
		ldi YH, HIGH(LCDLn2Addr)		; Set Y register to point to high byte destination of Line 2 of the LCD Display. From LCDDriver

		ldi counterRegister, 12					;load the constant value 12 into register r24, with 12 being the # of characters in the string "Hello, World"

LINE2_1:
		lpm mpr, Z+ ; load data from z register into register r16. Post increment Z to point to next character in string
		st Y+,mpr   ; store data from r16 into data memory (LCD Display line2). Post increment to move to next location in data memory
		DEC counterRegister	; decrement loop counter	
		BRNE LINE2_1	; branch if not equal, remains in LOOP if counter is still greater than 0

		ret
		

;***********************************************************
;*	Stored Program Data
;***********************************************************

;-----------------------------------------------------------
; An example of storing a string. Note the labels before and
; after the .DB directive; these can help to access the data
;-----------------------------------------------------------
STRING_NAME_BEG:
.DB		"Alexander Uong"		; Declaring string name data in ProgMem
STRING_NAME_END:

HELLOWORLD_BEG:
.DB		"Hello, World"		; Declaring string name data in ProgMem
HELLOWORLD_END:



;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver
