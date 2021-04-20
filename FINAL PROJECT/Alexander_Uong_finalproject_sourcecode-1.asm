;***********************************************************
;*	This is the final project template for ECE375 Winter 2021
;***********************************************************
;*	 Author: Alexander Uong
;*   Date: 3/13/2021
;***********************************************************
.include "m128def.inc"			; Include definition file
;***********************************************************
;*	Internal Register Definitions and Constants
;*	(feel free to edit these or add others)
;***********************************************************
.equ QuotientAddress = $0E00
.equ VelocityAddress = $0E03
.equ ProductAddress = $0E05
.equ PeriodAddress = $0E12


.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	A = r2					; A variable
.def	B = r3					; Another variable

.def	zero = r4				; Zero register, set to zero in INIT, useful for calculations

.def lowRadiusRegister = r5
.def highRadiusRegister = r6

.def GMByte1 = r7
.def GMByte2 = r8
.def GMByte3 = r9
.def GMByte4 = r10

.def selectedPlanetRegister = r11
.def squareRoot = r12
.def plusOne = r13


.def	mpr = r16				; Multipurpose register
.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter
.def	dataptr = r19			; data ptr

.def temp1 = r22
.def temp2 = r23
.def temp3 = r24
.def temp4 = r25



;***********************************************************
;*	Data segment variables
;*	(feel free to edit these or add others)
;***********************************************************
.dseg
.org	$0100						; data memory allocation for operands
operand1:		.byte 10			; allocate 10 bytes for a variable named op1


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment
;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt
.org	$0046					; End of Interrupt Vectors
;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:	; The initialization routine
		
		; Initialize Stack Pointer
		LDI R16, LOW(RAMEND)	; Low Byte of End SRAM Address
		OUT SPL, R16			; Write Byte to SPL
		LDI R16, HIGH(RAMEND)	; High Byte of End SRAM Address
		OUT SPH, R16			; Write Byte to SPH
		
		ldi ZL, LOW(2* OrbitalRadius)				;obtain value of orbital radius and load into Z register
		ldi ZH, HIGH (2* OrbitalRadius)
		lpm lowRadiusRegister , Z+					;store low byte of orbital radius into r20
		lpm highRadiusRegister , Z					;store high byte of orbital radius into r21

		ldi ZL , LOW(2*SelectedPlanet)				;obtain value of selected planet and load into z register
		lpm selectedPlanetRegister, Z				;load into selectedPlanetRegister
		lpm r2, Z


		clr		zero					;clr zero register
		clr		r20



MAIN:
	rcall CheckRadius
	cpi r20, -1
	breq halt
	
	rcall getGM
	rcall checkGM
	cpi r20, -1
	breq halt

	rcall calculateQuotient
	rcall calculateSquareRoot
	rcall checkVelocity
	cpi r20, -1
	breq halt

	rcall obtainPeriodOperands
	rcall calculateProduct
	rcall calculateQuotientPeriod
	rcall calculateSquareRootPeriod
	rcall checkPeriod

halt:
	jmp Grading 	


	rjmp MAIN



	


;-----------------------------------------------------------
;	Procedures and Subroutines
;-----------------------------------------------------------
; your code can go here as well


;-----------------------------------------------------------
;	CheckRadius
;	Desc: Checks value of radius, if <= 1000, store -1 into velocity
;-----------------------------------------------------------
CheckRadius:
	push mpr;			
	clr mpr;
	
	ldi temp1, 0xE9						//load low byte of 1001 into temp1
	ldi temp2, 0x03						//load high byte of 1001 into temp2
	cp lowRadiusRegister, temp1			//compare value in loadradiusregister with temp1
	cpc highRadiusRegister, temp2		//compare with carry highradiusregister with temp2
	brlo setNegativeOne					//branch if highradiusregister and lowradiusregister is < 1001
	rjmp Exit							//if it isnt exit function

setNegativeOne:
	ldi mpr, 0xFF						//load value 0xff into mpr
	ldi YL, LOW(VelocityAddress)		//set y register to point to low byte address of velocity
	ldi YH, HIGH(VelocityAddress)		//set y register to point to high byte address of velocity
	st Y+, mpr							//store mpr into Y with post increment
	st Y, mpr			
	dec r20				

Exit:
	pop mpr
	ret


;-----------------------------------------------------------
;	getGM
;	Desc: Based on selectedPlanet, get correct planet GM value
;-----------------------------------------------------------

getGM:
		clr GMByte1						//clr registers storing gm value
		clr GMByte2
		clr GMByte3
		clr GMByte4

		ldi ZL, LOW(2*PlanetInfo)		//obtain address of planet info and have Z register point at the address
		ldi ZH, HIGH (2* PlanetInfo)	


iterateArray:
		cp selectedPlanetRegister, zero	//compare selectedPlanetRegister with  0
		breq loadGMValue				//if it is 0, branch to loadGMvalue
		adiw ZH:ZL, 4					//if it isn't 0, add 4 bytes to z register to point to next planet gm value
		dec selectedPlanetRegister		//decrement planetregister
		rjmp iterateArray				//keep repeating until planetregister is zero


loadGMValue:
		
		lpm GMByte1 , Z+				//load each byte of planetGM value into four respective GMByte registers using post increment
		lpm GMByte2 , Z+
		lpm GMByte3 , Z+
		lpm GMByte4 , Z

		ret

	
;-----------------------------------------------------------
;	checkGM
;	Desc: check if gmvalue is <= 1000
;-----------------------------------------------------------

checkGM:
	push mpr;		
	push temp1
	push temp2	
	
	ldi temp1, 0xE9					//load low byte of 1001 into temp1
	ldi temp2, 0x03					//load high byte of 1001 into temp2
	
	cp GMByte1, temp1				//compare value in loadradiusregister with temp1
	cpc GMByte2, temp2				//compare with carry highradiusregister with temp2

	brlo checkHighBytes				//branch if highradiusregister and lowradiusregister is < 1001
	rjmp Exit2						//if it isnt exit function

checkHighBytes:
	cp GMByte3, zero				//After checking 2 lower bytes, check higher bytes to make sure they're zero. IF they're not the value is greater than 1000
	cpc GMByte4, zero
	breq negativePeriod				//branch to negativePeriod if the high bytes are zero, else exit function
	rjmp Exit2					


negativePeriod:
	ldi mpr, 0xFF						//load value 0xff into mpr
	ldi YL, LOW(PeriodAddress)			//set y register to point to low byte address of Period
	ldi YH, HIGH(PeriodAddress)			//set y register to point to high byte address of Period
	
	st Y+, mpr							//store mpr into Y with post increment
	st Y+, mpr							
	st Y, mpr	
		
	dec r20			


Exit2:
	pop mpr								
	pop temp1
	pop temp2	
	ret									//return from subroutine



;-----------------------------------------------------------
;	calculateQuotient
;	Desc: Obtains quotient (GM/r) and stores to correct location in memory
;-----------------------------------------------------------
calculateQuotient:
/*
.def lowRadiusRegister = r5 .DB	0x64, 0x19	
.def highRadiusRegister = r6

.def GMByte1 = r7 .DB	0x08, 0x15, 0x06, 0x00
.def GMByte2 = r8
.def GMByte3 = r9
.def GMByte4 = r10
*/
push mpr
clr mpr

ldi		XL, $00				//16 bit representation of 0
ldi		XH, $00

ldi		YL, $01				//16 bit representation of 1
ldi		YH, $00

ldi		ZL, low(Quotient)		//Z register points to address where quotient is stored
ldi		ZH, high(Quotient)


loop:
	cp r9, zero				//compares high bytes and checks if they're zero.
	cpc r10, zero
	breq loop2				//if they are branch to loop2

	sub r7, r5				//subtracts 16 bit orbitalradius from GM value. 
	sbc r8, r6				//Go byte by byte and use subtract with carry to subtract and store updated value back into registers r7 through r10
	sbc r9, zero
	sbc r10, zero
	
	add XL, YL				//after subtraction has completed add 1 to 16 bit value stored in x register
	adc XH, YH				//add with carry for subsequent bytes, as quotient can be up to a 24 bit value
	adc mpr, zero

	rjmp loop				//repeat

loop2:
	cp r7, r5				//compares lower 2 bytes
	cpc r8, r6
	brlo remainder			//if the two lower bytes of gm value is < orbital radius, no more division can be done, branch to remainder

	sub r7, r5				//subtract orbital radius from lower 2 bytes of gm value until you can't anymore
	sbc  r8, r6

	add XL, YL				//add 1 to x to update quotient value
	adc XH, YH
	adc mpr, zero			//add carry bit
	
	rjmp loop2				//repeat until branch statement is satisfied

remainder:
	add r7, r7				//method of determining whether to round up or down. 
	adc r8, r8				//add lower bytes of gm value to itself
	
	cp r7, r5				//if the new value is greater than the orbital radius, we know that it was more than 1/2 of the remainder
	cpc r8, r6
	brge increment			//branch to increment if this is the case
	
	rjmp store				//otherwise jump to store


increment:
	add XL, YL				//add 1 to quotient value as remainder was greater than 1/2 of radius
	adc XH, YH
	adc mpr, zero			//add carry bit



store:
	st Z+, XL				//store quotient to z register, which is pointing at quotient in data memory
	st Z+, XH
	//brcc done
	st Z, mpr
	
done:
	pop mpr
	ret



;-----------------------------------------------------------
;	calculateSquareRoot
;	Desc: take square root of 24 bit number
;-----------------------------------------------------------
calculateSquareRoot:		
/*
.def temp1 = r22
.def temp2 = r23
.def temp3 = r24
.def temp4 = r25
*/

		clr r16							//clr registers 
		clr r17
		
		clr r18
		clr r19

		clr temp1					//clr registers result of multiplication will be stored
		clr temp2
		clr temp3
		clr temp4
		
		clr zero

	

		ldi	XL, $00						//16 bit representation of counter to keep track of square root value
		ldi	XH, $00
		
			
		ldi	YL, low(Quotient)			//Y register points to quotient in data memory
		ldi	YH, high(Quotient)	

		ld	r16, Y+						//load values of quoetient into registers 16-18
		ld	r17, Y+
		ld	r18, Y+

		ldi	ZL, low(Velocity)			//Z register points at address of velocity in data memory
		ldi	ZH, high(Velocity)


		
multiply:
		cp temp1, r16					//checks the 24 bit result of multiplication and compares to quotient value
		cpc temp2, r17
		cpc temp3, r18
		brge storeVelocity				//if it's greater than the quotient value, we can determine the square root
	
	
		;start of 16 bit multiplication routine

        mul XH,XH						//multiplies high byte of square root value
        movw temp4:temp3,rhi:rlo		//result is stored in r1 and r0. Move vlaues to high bytes of multiplication result

        mul XL,XL						//multiplies low bytes of square root value
        movw temp2:temp1,rhi:rlo		//result is stored in r1 and r0. Move vlaues to low bytes of multiplication result

        mul XH,XL						//multiplies high byte with low byte of square root value
        add temp2,rlo					//add the result existing value
        adc temp3,rhi					//add with carry
        adc temp4, zero					//add carry bit if available
        mul XH,XL						//multiplies high byte with low byte of square root value
        add temp2,rlo					//add the result existing value
        adc temp3,rhi					//add with carry
        adc temp4, zero					//add carry bit if available

		adiw XH:XL, 1				//add one to square root value
		rjmp multiply				//keep repeating until condition is satisfied

storeVelocity:
		sbiw XH:XL, 2			//subtract 2 from x due to adiw adding an extra 2 to the correct square root answer
		st Z+, XL				//store value where Z register, pointing at velocity in data memory
		st Z, XH
		ret						; End a function with RET


;-----------------------------------------------------------
;	checkVelocity
;	Desc: checks if computed velocity is 0. If it is, store -2 into velocity
;---------------------------------------------------------
checkVelocity:
		
	push temp1
	push temp2	
	
	clr temp1
	clr temp2
	
	ldi ZL, low(Velocity)			//Z registers points to velocity in data memory
	ldi ZH, high(Velocity)
	
	lpm temp1, Z+					//loads values into temp registers
	lpm temp2, Z

	cp temp1, zero					//compares the values in temp registers to zero
	cpc temp2, zero
	breq zeroVelocity				//branch if equal to zero
	rjmp Exit3						//otherwise exit routine

zeroVelocity:
	ldi temp1, 0xFE					//if velocity is zero, load 0xfffe into temp2:temp1
	ldi temp2, 0xFF

	st Z+, temp1					//store into velocity in data memory
	st Z, temp2
	dec r20


Exit3:

	pop temp1
	pop temp2
	ret



;-----------------------------------------------------------
;	obtainPeriodOperands
;	Desc: multiply 40*r, r*r, and store into data memory
;---------------------------------------------------------
obtainPeriodOperands:
		//clr r5	->orbital radius low byte
		//clr r6	->orbital radius high byte
		
		clr r16					//stores r
		clr r17

		clr rhi
		clr rlo
		clr temp1				//result of multiplication
		clr temp2
		clr temp3
		clr temp4
		
		clr zero

		ldi r16, $28				//40 in hexadecimal
		ldi r17, $00

		ldi		ZL, low($0E15)		//stores values in data memory for access later
		ldi		ZH, high($0E15)

		//16 bit multiplication

		mul r17,r6						//multiplies high byte of square root value
        movw temp4:temp3,rhi:rlo		//result is stored in r1 and r0. Move vlaues to high bytes of multiplication result

        mul r16,r5						//multiplies low bytes of square root value
        movw temp2:temp1,rhi:rlo		//result is stored in r1 and r0. Move vlaues to low bytes of multiplication result

        mul r17,r5						//multiplies high byte with low byte of square root value
        add temp2,rlo					//add the result existing value
        adc temp3,rhi					//add with carry
        adc temp4, zero					//add carry bit if available

        mul r6,r16						//multiplies high byte with low byte of square root value
        add temp2,rlo					//add the result existing value
        adc temp3,rhi					//add with carry
        adc temp4, zero					//add carry bit if available

		st Z+, temp1					//store result of multiplication 40*r into data memory where z is pointing
		st Z+, temp2
		st Z+, temp3
		st Z+, temp4

		//do exact same for r*r. r5 and r6 currently store r

		clr rhi
		clr rlo
		clr temp1
		clr temp2
		clr temp3
		clr temp4


		ldi		ZL, low($0E19)			//another address in data memory
		ldi		ZH, high($0E19)

		mul r6,r6						//multiplies high byte of square root value
        movw temp4:temp3,rhi:rlo		//result is stored in r1 and r0. Move vlaues to high bytes of multiplication result

        mul r5,r5						//multiplies low bytes of square root value
        movw temp2:temp1,rhi:rlo		//result is stored in r1 and r0. Move vlaues to low bytes of multiplication result

        mul r6,r5						//multiplies high byte with low byte of square root value
        add temp2,rlo					//add the result existing value
        adc temp3,rhi					//add with carry
        adc temp4, zero					//add carry bit if available

        mul r6,r5						//multiplies high byte with low byte of square root value
        add temp2,rlo					//add the result existing value
        adc temp3,rhi					//add with carry
        adc temp4, zero					//add carry bit if available

		st Z+, temp1					//store result of multiplication r*r into data memory where z is pointing
		st Z+, temp2
		st Z+, temp3
		st Z+, temp4



		ret


;-----------------------------------------------------------
;	calculateProduct
;	Desc: take 40*r and r*r out of data memory and multiply them together, storing result into product location. 
;---------------------------------------------------------
calculateProduct:
		//registers to store 40*r
		clr r5 
		clr r6 
		clr r7 
		clr r8 
		
		//registers to store r*r
		clr r9 
		clr r10
		clr r11
		clr r12

		//registers to store result of 40*r^3
		clr r14 
		clr r15 
		clr r16 
		clr r17 
		clr r18
		clr r19
		clr r20

		
		ldi	ZL, low($0E15)		//Z points to 40*r in data memory
		ldi	ZH, high($0E15)

		ld	r5, Z+				//load 40*r into r5 through r8
		ld	r6, Z+
		ld	r7, Z+
		ld	r8, Z+

		ldi	ZL, low($0E19)		//Z points to r*r in data memory
		ldi	ZH, high($0E19)

		ld r9, Z+				//load r*r into r5 through r8
		ld r10, Z+
		ld r11, Z+
		ld r12, Z+

		//32 bit multiplication using operands 40*r and r*r

		mul	r5,r9			//multiply each byte of first operand by lowest byte of second operand
		movw R15:R14, r1:r0
		
		mul	r6,r9			//add with carry each value accross bytes of result
		add	r15,rlo
		adc	r16,rhi
		
		mul	r7,r9
		add	r16,rlo			//add with carry each value accross bytes of result
		adc	r17,rhi
		
		mul	r8,r9			
		add	r17,rlo			//add with carry each value accross bytes of result
		adc	r18,rhi
		////////////////////////

		mul	r5,r10			//multiply each byte of first operand by second lowest byte of second operand
		add	r15,rlo			//add with carry each value accross bytes of result
		adc	r16,rhi
		adc	r17, zero
		adc	r18,zero
		adc	r19,zero

		mul	r6,r10
		add	r16,rlo			//add with carry each value accross bytes of result
		adc	r17,rhi
		adc	r18,zero
		adc	r19,zero

		mul	r7,r10
		add	r17,rlo			//add with carry each value accross bytes of result
		adc	r18,rhi
		adc	r19,zero

		mul	r8,r10
		add	r18,rlo			//add with carry each value accross bytes of result
		adc	r19,rhi
		//////////////////////////////////////

		mul	r5,r11			//multiply each byte of first operand by second highest byte of second operand
		add	r16,rlo
		adc	r17,rhi			//add with carry each value accross bytes of result
		adc	r18,zero
		adc	r19,zero
		adc	r20,zero

		mul	r6,r11
		add	r17,rlo			//add with carry each value accross bytes of result
		adc	r18,rhi
		adc	r19,zero
		adc	r20,zero

		mul	r7,r11
		add	r18,rlo			//add with carry each value accross bytes of result
		adc	r19,rhi
		adc	r20,zero

		mul	r8,r11
		add	r19,rlo			//add with carry each value accross bytes of result
		adc	r20,rhi
		////////////////////////////////////////
		
		mul	r5,r12			//multiply each byte of first operand by highest byte of second operand
		add	r17,rlo
		adc	r18,rhi			//add with carry each value accross bytes of result
		adc	r19,zero
		adc	r20,zero
		adc	r21,zero

		mul	r6,r12
		add	r18,rlo			//add with carry each value accross bytes of result
		adc	r19,rhi
		adc	r20,zero
		adc	r21,zero

		mul	r7,r12
		add	r19,rlo			//add with carry each value accross bytes of result
		adc	r20,rhi
		adc	r21,zero

		mul	r8,r12
		add	r20,rlo			//add with carry each value accross bytes of result
		adc	r21,rhi
		///////////////////////////////////////////////
		
		ldi	ZL, low(Product)		//Z points to product in data memory
		ldi	ZH, high(Product)

		st Z+, r14						//store resulting registers to Product in data memory
		st Z+, r15
		st Z+, r16
		st Z+, r17
		st Z+, r18
		st Z+, r19
		st Z+, r20
		
		ret



;-----------------------------------------------------------
;	calculateQuotientPeriod
;	Desc: Same logic using subtraction and counter to obtain quotient
;---------------------------------------------------------

calculateQuotientPeriod:
	clr GMByte1
	clr GMByte2
	clr GMByte3
	clr GMByte4

	ldi ZL, LOW(2*PlanetInfo)		//obtain address of planet info and have Z register point at the address
	ldi ZH, HIGH (2* PlanetInfo)	


iterateArray2:
	cp r2, zero						//compare selectedPlanetRegister with  0
	breq loadGMValue2				//if it is 0, branch to loadGMvalue
	adiw ZH:ZL, 4					//if it isn't 0, add 4 bytes to z register to point to next planet gm value
	dec r2							//decrement planetregister
	rjmp iterateArray2				//keep repeating until planetregister is zero


loadGMValue2:
	lpm GMByte1 , Z+				//load each byte of planetGM value into four respective GMByte registers using post increment
	lpm GMByte2 , Z+
	lpm GMByte3 , Z+
	lpm GMByte4 , Z


	clr r2
	clr r3
	clr r12
	clr r13

	clr selectedPlanetRegister
///////////////////////////////////////////////////////////Above is identical to getGm.



	ldi		XL, $00			
	ldi		XH, $00

	ldi		YL, $01				//16 bit representation of 1
	ldi		YH, $00
	

	ldi		ZL, low($0E1D)		//Z register points to address where quotient is stored
	ldi		ZH, high($0E1D)
	
	/*
	.def GMByte1 = r7
	.def GMByte2 = r8
	.def GMByte3 = r9
	.def GMByte4 = r10
	*/


	/*registers to store result 
	clr r14 
	clr r15 
	clr r16 
	clr r17 
	clr r18
	clr r19
	clr r20
	*/

	
loop1_1:
	cp r14, r7				//compares low 4 bytes and checks if < GM
	cpc r15, r8
	cpc r16, r9
	cpc r17, r10
	cpc r18, zero			//compares high bytes and checks if they're zero.
	cpc r19, zero
	cpc r20, zero

	brlo remainder2			//if they are branch to remainder


	sub r14, r7				//subtracts 32bit GM value from GM 4pir^3. 
	sbc r15, r8				//Go byte by byte and use subtract with carry to subtract and store updated value back into registers r14 through r20
	sbc r16, r9
	sbc r17, r10
	sbc r18, zero
	sbc r19, zero
	sbc r20, zero
	
	add XL, YL				//add 1 to the counter
	adc XH, YH
	adc r2, zero			//add carry bit
	adc r3, zero
	adc r12, zero
	adc r13, zero

	rjmp loop1_1			//repeat



remainder2:
	add r14, r14				//method of determining whether to round up or down. 
	adc r15, r15				//add lower bytes of 4pir^3 value to itself
	adc r16, r16				
	adc r17, r17
	adc r18, zero

	cp r14, r7				//if the new value is greater than GM, we know that it was more than 1/2 of GM
	cpc r15, r8
	cpc r16, r9
	cpc r17, r10
	cpc r18, zero

	brge increment2			//branch to increment if this is the case
	
	rjmp store2				//otherwise jump to store


increment2:
	add XL, YL				//add 1 to quotient value as remainder was greater than 1/2 of GM
	adc XH, YH
	adc r2, zero			//add carry bit
	adc r3, zero
	adc r12, zero
	adc r13, zero

store2:
	st Z+, XL				//store counter registers to quotient in data memory
	st Z+, XH
	st Z+, r2
	st Z+, r3
	st Z+, r12
	st Z+, r13
	ret



;-----------------------------------------------------------
;	calculateSquareRootPeriod
;	Desc: Takes the square root of the quotient of period
;---------------------------------------------------------
calculateSquareRootPeriod:
		
		clr r3
		inc r3

		//32 bits for operand 1
		clr r5 
		clr r6 
		clr r7 
		clr r8 
		
		//32 bits for operand 2
		clr r9 
		clr r10
		clr r11
		clr r12


		//Store RESULT OF QUOTIENT

		ldi	ZL, low($0E1D)		//Z register points to address where quotient is stored
		ldi	ZH, high($0E1D)
	
		ld	r21, Z+				//loads quotient into registers r21-r26
		ld	r22, Z+
		ld	r23, Z+
		ld	r24, Z+
		ld	r25, Z+
		ld	r26, Z+


multiply2:
		// store result of 32 bit multiplication to compare with quotient
		clr r14				//registers that store result of multiplication
		clr r15 
		clr r16 
		clr r17 
		clr r18
		clr r19
		clr r20

		mul	r5,r9				//multiply each byte of first operand by lowest byte of second operand
		movw R15:R14, r1:r0

		mul	r6,r9
		add	r15,rlo				//add with carry each value accross bytes of result
		adc	r16,rhi

		mul	r7,r9
		add	r16,rlo				//add with carry each value accross bytes of result
		adc	r17,rhi

		mul	r8,r9
		add	r17,rlo				//add with carry each value accross bytes of result
		adc	r18,rhi
		//////////////////////////////////////
		mul	r5,r10				//multiply each byte of first operand by second lowest byte of second operand
		add	r15,rlo
		adc	r16,rhi
		adc	r17, zero			//add with carry each value accross bytes of result
		adc	r18,zero
		adc	r19,zero

		mul	r6,r10
		add	r16,rlo
		adc	r17,rhi				//add with carry each value accross bytes of result
		adc	r18,zero
		adc	r19,zero

		mul	r7,r10
		add	r17,rlo
		adc	r18,rhi				//add with carry each value accross bytes of result
		adc	r19,zero

		mul	r8,r10
		add	r18,rlo
		adc	r19,rhi
		/////////////////////////////////
		mul	r5,r11					//multiply each byte of first operand by second highest byte of second operand
		add	r16,rlo
		adc	r17,rhi
		adc	r18,zero				//add with carry each value accross bytes of result
		adc	r19,zero
		adc	r20,zero

		mul	r6,r11
		add	r17,rlo
		adc	r18,rhi					//add with carry each value accross bytes of result
		adc	r19,zero
		adc	r20,zero

		mul	r7,r11
		add	r18,rlo
		adc	r19,rhi					//add with carry each value accross bytes of result
		adc	r20,zero

		mul	r8,r11
		add	r19,rlo
		adc	r20,rhi					//add with carry each value accross bytes of result
		////////////////////////////////////
		mul	r5,r12					//multiply each byte of first operand by highest byte of second operand
		add	r17,rlo
		adc	r18,rhi
		adc	r19,zero
		adc	r20,zero
		adc	r21,zero

		mul	r6,r12
		add	r18,rlo
		adc	r19,rhi					//add with carry each value accross bytes of result
		adc	r20,zero
		adc	r21,zero

		mul	r7,r12
		add	r19,rlo
		adc	r20,rhi					//add with carry each value accross bytes of result
		adc	r21,zero

		mul	r8,r12
		add	r20,rlo
		adc	r21,rhi					//add with carry each value accross bytes of result
		///////////////////////////////////
		
		cp r14, r21					//compare the result of the 32 bit multiplication with the result of the quotient
		cpc r15, r22
		cpc r16, r23
		cpc r17, r24
		cpc r18, r25
		cpc r19, r26
		brge storePeriod			//if it's greater than quotient branch to store period and stop multiplying
		
		add r5, r3					//increment operand 1
		adc r6, zero
		adc r7, zero
		adc r8, zero

		add r9, r3					//increment operand 2
		adc r10, zero
		adc r11, zero
		adc r12, zero


		rjmp multiply2				//repeat process

storePeriod:
		sub r5, r3					//subtract 1 from operand 1
		sbc r6, zero
		sbc r7, zero
		sbc r8, zero


		ldi ZL, low(Period)			//store operand1 into period, as it serves as the square root of the quotient
		ldi ZH, high(Period)
		st Z+, r5
		st Z+, r6
		st Z+, r7
		ret



		
;-----------------------------------------------------------
;	checkPeriod
;	Desc: checks if period is < 25 seconds
;---------------------------------------------------------
checkPeriod:
	ldi ZL, low(Period)			//Z pointing at period in data memory
	ldi ZH, high(Period)

	clr r16
	clr r17
	clr r18

	ldi r16, $19				//$19 equivalent to 25 in decimal
	ldi r17, $00
	ldi r18, $00

	cp r5, r16					//compare period with $19
	cpc r6, r17
	cpc r7, r18
	brlo lessThan				//if its less, branch
	rjmp notlessThan			//else jump to notlessthan

lessThan:
	ldi r16, $FE				//load -2 into registers
	ldi r17, $FF
	ldi r18, $FF

	st Z+, r16					//store -2 into period
	st Z+, r17
	st Z+, r18
	
	

notlessThan:
	ret





;***********************************************************
;*	Custom stored data
;*	(feel free to edit these or add others)
;***********************************************************
SomeConstant:	.DB	0x86, 0xA4
SquareRootCount: .DB 0x03, 0x00
SquareRootCount2: .DB 0x03,0x00



;***end of your code***end of your code***end of your code***end of your code***end of your code***
;*************************** Do not change anything below this point*******************************
;*************************** Do not change anything below this point*******************************
;*************************** Do not change anything below this point*******************************

Grading:
		nop					; Check the results in data memory begining at address $0E00 (The TA will set a breakpoint here)
rjmp Grading


;***********************************************************
;*	Stored program data that you cannot change
;***********************************************************

; Contents of program memory will be changed during testing
; The label names (OrbitalRadius, SelectedPlanet, PlanetInfo, MercuryGM, etc) are not changed
; NOTE: All values are provided using the little-endian convention.
OrbitalRadius:	.DB	 0xe9, 0x03; the radius that should be used during computations (in kilometers)	0x64, 0x19 0xe9, 0x03
											; in this example, the value is 6,500 kilometers
											; the radius will be provided as a 16 bit unsigned value (unless you are
											; completing the extra credit, in which case the radius is an unsigned 24 bit value)

SelectedPlanet:	.DB	0x01, 0x00				; This is how your program knows which GM value should be used.
											; SelectedPlanet is an unsigned 8 bit value that provides you with the
											; index of the planet (and hence, tells you which GM value to use).
											; Note: only the first byte is used. The second byte is just for padding.
											; In this example, the value is 2. If we check the planet at index 2, (from the data below)
											; that corresponds to Earth.
											; if the value was 7, that would correspond to the planet Neptune

PlanetInfo:									; Note that these values will be changed during testing!
MercuryGM:		.DB	0x0E, 0x56, 0x00, 0x00	; Gravitational parameters will be provided as unsigned 32 bit integers (little-endian)
VenusGM:		.DB	0x24, 0xF5, 0x04, 0x00	; the units are in: (km * km * km)/(sec * sec)
EarthGM:		.DB	0x08, 0x15, 0x06, 0x00	; <-- note that this is 398,600			 0x08, 0x15, 0x06, 0x00
MarsGM:			.DB	0x4E, 0xA7, 0x00, 0x00
JupiterGM:		.DB	0x30, 0x13, 0x8D, 0x07	; A word of advice... treat these like an array, where each element
SaturnGM:		.DB	0xF8, 0xC7, 0x42, 0x02	; occupies 4 bytes of memory.
UranusGM:		.DB	0xD0, 0x68, 0x58, 0x00	; Mercury is at index 0, Venus is at index 1, ...and the final planet is at index 8.
NeptuneGM:		.DB	0x38, 0x4B, 0x68, 0x00
FinalGM:		.DB	0xFF, 0xFF, 0xFF, 0xFF


;***********************************************************
;*	Data Memory Allocation for Results
;*	Your answers need to be stored into these locations (using little-endian representation)
;*	These exact variable names will be used when testing your code!
;***********************************************************
.dseg
.org	$0E00						; data memory allocation for results - Your grader only checks $0E00 - $0E14
Quotient:		.byte 3				; This is the intermediate value that is generated while you are computing the satellite's velocity.
									; It is a 24 bit unsigned value.
Velocity:		.byte 2				; This is where you will store the computed velocity. It is a 16 bit signed number. 
									; The velocity value is normally positive, but it can also be -1 or -2 in case of error
									; (see "Special Cases" in the assignment documentation).
Product:		.byte 7				; This is the intermediate product that is generated while you are computing the orbital period.
Period:			.byte 3				; This is where the orbital period of the satellite will be placed.
									; It is a 24 bit signed value.
									; The period value is normally positive, but it can also be -1 or -2 in case of error
									; (see "Special Cases" in the assignment documentation).

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
