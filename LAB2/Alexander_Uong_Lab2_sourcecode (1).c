/*
 * Alexander_Uong_Lab2_sourcecode.c
 *
 * Created: 1/18/2021 4:26:59 PM
 * Author : Alex Uong
 */ 

/*
This code will cause a TekBot connected to the AVR board to
move forward and when it touches an obstacle, it will reverse
and turn away from the obstacle and resume forward motion.

PORT MAP
Port B, Pin 4 -> Output -> Right Motor Enable
Port B, Pin 5 -> Output -> Right Motor Direction
Port B, Pin 7 -> Output -> Left Motor Enable
Port B, Pin 6 -> Output -> Left Motor Direction
Port D, Pin 1 -> Input -> Left Whisker
Port D, Pin 0 -> Input -> Right Whisker
*/

#define F_CPU 16000000
#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

int main(void)
{
	DDRB = 0b11110000;	//configure Port B pins for input/output
	PORTB = 0b01100000;	//set initial value for Port B outputs 
	
	while (1) // loop forever
	{
		if(PIND == 0b11111110){		//if right whisker is hit, do the following
			PORTB = 0b00000000;		//move backward
			_delay_ms(1000);		//wait 1 second
			PORTB = 0b00100000;		//turn left
			_delay_ms(1000);		//wait 1 second
			PORTB = 0b01100000;		// move forward
		}
		if (PIND == 0b11111101){	//if left whisker is hit, do the following
			PORTB = 0b00000000;		//move backward
			_delay_ms(1000);		//wait 1 second
			PORTB = 0b01000000;		//turn right
			_delay_ms(1000);		//wait 1 second
			PORTB = 0b01100000;		// move forward
		}
		if(PIND == 0b11111100){		//if both left and right whisker are hit, do the following
			PORTB = 0b00000000;		//move backward
			_delay_ms(1000);		//wait 1 second
			PORTB = 0b00100000;		//turn left
			_delay_ms(1000);		//wait 1 second
			PORTB = 0b01100000;		//move forward
		}
		
	}
}

