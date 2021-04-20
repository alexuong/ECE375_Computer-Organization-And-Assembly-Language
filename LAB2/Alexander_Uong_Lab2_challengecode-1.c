/*
 * lab2challengecode.c
 *
 * Created: 1/18/2021 5:58:24 PM
 * Author : Alex Uong
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
			PORTB = 0b01100000;		//move forward 
			_delay_ms(500);			//wait half a second
			PORTB = 0b00000000;		//moves backwards slightly
			_delay_ms(500);			//wait half a second
			PORTB = 0b01000000;		// turn slightly toward object (in this case turn right)
			_delay_ms(500);			//wait half a second
			PORTB = 0b01100000;		//move forward again
		}
		if (PIND == 0b11111101){	//if left whisker is hit, do the following
			PORTB = 0b01100000;		//move forward
			_delay_ms(500);			//wait half a second
			PORTB = 0b00000000;		//moves backwards slightly
			_delay_ms(500);			//wait half second
			PORTB = 0b00100000;		//turn slightly toward object (in this case turn left)
			_delay_ms(500);			//wait half a second
			PORTB = 0b01100000;		// move forward
		}
		if(PIND == 0b11111100){		//if both left and right whisker are hit, do the following
			PORTB = 0b01100000;		//move forward
			_delay_ms(500);			//wait half a second
			PORTB = 0b00000000;		//moves backwards slightly
			_delay_ms(500);			//wait half a second
			PORTB = 0b01000000;		// turn slightly toward object (in this case turn right)
			_delay_ms(500);			//wait half a second
			PORTB = 0b01100000;		//move forward again
		}
		
	}
}
