Floating-LED-Message-Fan
========================

Code for PIC16 series microcontrollers (specifically, model f1826) to display a message that appears to "float" in the air, using a 7 LED bar attached to a fan with once per revolution interrupts for timing. I've been using the falling edge of a photosensor as it passes over a tab attached to the fan's frame as this interrupt source.

Circuit
-------

The LEDs should be attached to PORTB[1:7], the interrupt source should be attached to the CCP pin, which is currently mapped to RB0.

Operational Details
-------------------

The main loop repeatedly grabs the current column being displayed and outputs it to PORTB.

There are two ISRs which modify the current column. The newcol ISR queries the font table for the next column of the current letter, and also checks whether the letter is completely printed (the font tables define each letter's width using the maxcols macro). The newcol ISR is triggered by TMR2 at intervals set by the nextrev ISR.

The nextrev ISR is triggered by the CCP1 module, which is set to "capture" on falling edge in config.  It computes the time since the last nextrev interrupt, divides by 2^6, and places the result into PR2 (TMR2 interrupt period). The division by 2^6 is a bit weird: CCP1 is a 2 byte counter, so dividing by 2^6 is implemented by shifting the high byte right once. 2^6 was chosen because it allowed for 2^8=256 columns (there is a factor of 4 difference in the speeds of the CCP1 counter and TMR2, and 2^6 * 4 = 2^8).
