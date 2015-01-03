#include p16f1826.inc

;************CONSTANTS**************
WORKING   		equ 	70h ;Temporary store for w register during interrupts
CURRENT 		equ 	71h ;current column being displayed
PREVTIME 		equ 	73h ;last time high byte (msb)
TEMP			equ 	74h ;general temp register (used when PCLATH needs to be loaded)
COL				equ 	75h ;number of column currently being displayed
CURRENT_LETTER 	equ 	76h ;number of letter currently being displayed (1-16)
MAX_COLS		equ 	77h ;Max number of columns in the current letter

NUM_CHARS		equ		d'30'

;*****MAXCOLS MACRO*****
maxcols		macro	num

			movlw	num
			movwf	MAX_COLS
			endm

;*****START OF PROGRAM*****
org	00h
	goto 	start

;*******INTERRUPT SERVICE ROUTINE*******
org 04h
intserv:
	movwf 	WORKING
	
	btfsc	PIR1,1 ;set on on TMR2 = PR2
	call 	newcol
	
	btfsc 	PIR1,2 ;CCP1 because you need to "capture" TMR2 on interrupt. set on CCP pin (RB0? see APFCON0)
	call	nextrev
	
	banksel	PORTB
	movfw 	WORKING
	retfie

start:
	__CONFIG _CONFIG1, _FOSC_INTOSC & _WDTE_OFF & _PWRTE_OFF & _MCLRE_ON & _CP_OFF & _CPD_OFF & _BOREN_OFF & _CLKOUTEN_OFF & _IESO_OFF & _FCMEN_OFF
	__CONFIG _CONFIG2, _WRT_OFF & _PLLEN_OFF & _STVREN_OFF & _LVP_ON
	
	banksel	INTCON
	bsf 	INTCON,7 ;Global interrupt enable
	bsf 	INTCON,6 ;Peripheral interrupt enable
	bcf 	INTCON,5 ;TMR0 overflow interrupt
	bcf 	INTCON,4 ;External interrupt enable
	bcf 	INTCON,3 ;PORTB interrupt enable
	
	banksel	CCP1CON
	movlw	b'00000100' ;capture on every falling edge (<3:0>=0100, other bits only used for PWM)
	movwf	CCP1CON
	
	banksel APFCON0
	bsf		APFCON0,CCP1SEL ;CCP1 on RB0 (I think)
	
	banksel	T1CON
	bsf		T1CON,0 ;enable TMR1
	bcf		T1CON,7 ;use instruction clock
	bcf		T1CON,6 ;use instruction clock
	bsf		T1CON,5 ;1:8 prescale
	bsf		T1CON,4 ;1:8 prescale
	bcf		T1CON,3 ;dedicated oscillator circuit (off)
	banksel	TMR1H
	clrf	TMR1H
	clrf	TMR1L
	
	banksel	T2CON
	bsf		T2CON,2 ;timer2 on
	bsf		T2CON,1 ;prescale = 1:16
	bcf		T2CON,0

	banksel	PIE1
	clrf	PIE1
	bsf 	PIE1,2 ;CCP1 interrupt
	bsf 	PIE1,1 ;TMR2 to PR2 match interrupt
	
	banksel	OSCCON
	bcf		OSCCON,7 ;PLL disabled
	
	bsf 	OSCCON,6 ;set frequency to 8Mhz
	bsf 	OSCCON,5 ;set frequency to 8Mhz
	bsf 	OSCCON,4 ;set frequency to 8Mhz
	bcf		OSCCON,3 ;freq = 1Mhz
	
	bsf		OSCCON,1 ;int osc
	bcf		OSCCON,0 ;int osc
	
	banksel	ANSELB
	clrf	ANSELB

	banksel	TRISA
	movlw 	0x00
	movwf 	TRISA ;setup PORTA as all output 
	movwf 	TRISB
	bsf		TRISB,0
	
	banksel	PIR1

	bcf		PIR1,2

;******MAIN PROGRAM*******
	movlw 	d'80' ;low period for debugging
	banksel PR2
	movwf 	PR2
	banksel	CURRENT
	movlw 	0x55 ;alternating bit pattern for testing
	movwf 	CURRENT

	clrf	CURRENT_LETTER
	clrf	COL

main:
;	banksel	PR2
;	movfw	PR2
;	banksel PORTB
	movfw	CURRENT
	movwf	PORTB

	goto 	main

;*********NEXT REVOLUTION INTERRUPT********
nextrev:
	;start back at letter 0, column 0
	clrf	CURRENT_LETTER
	clrf 	COL
	;clear tmr2
	clrf	TMR2
	;subtract previous time from current
	movfw 	PREVTIME
	banksel	CCPR1H
	subwf 	CCPR1H, W 	;now, the difference is in the working register
	banksel	PR2
	movwf 	PR2 		;now the difference is in PR2
	rrf 	PR2, F		;adjust for prescaler differences on TMRs 1 and 2
	bcf 	PR2, 7		;This bit should be cleared after the rrf, but may not be if the carry flag was set from the subtract
	banksel	CCPR1H
	movfw 	CCPR1H 	;put current time into PREVTIME
	movwf 	PREVTIME
	banksel	PIR1
	bcf 	PIR1, 2
	return

;********NEW COLUMN INTERRUPT*********
newcol:
	;call message_table, which will put the next letter's ascii in w
	movlw	HIGH	message_table
	movwf	PCLATH
	movfw 	CURRENT_LETTER
	call 	message_table
	;call letter_table, which will call the appropriate letter, which will put col to display in w
	movwf	TEMP
	movlw	HIGH	letter_table
	movwf	PCLATH
	movfw	TEMP
	call	letter_table
	movwf 	CURRENT
	
	incf	COL, F
	;Check if COL = MAX_COLS. If so, clear it (set it back to 0) and increment CURRENT_LETTER.
	movfw	MAX_COLS
	subwf	COL, W
	btfsc	STATUS,Z
	call	col_overflow
	
	banksel	PIR1
	bcf 	PIR1,1
	return	

;*****COL OVERFLOW******
col_overflow:
	clrf	COL
	incf	CURRENT_LETTER, F
	movlw	NUM_CHARS ;check if CURRENT_LETTER went over NUM_CHARS
	subwf	CURRENT_LETTER, W
	btfsc	STATUS,0
	clrf	CURRENT_LETTER
	return

;*****LETTER TABLE******
org	0x0300
letter_table: ;w will contain ascii code. jump to font table, which will return from subroutine with current col in w
	sublw	0x1F
	xorlw	0xFF
	addwf	PCL, F
	goto	getSpace
	goto	getExcl ;!
	goto	getDoubleQuote ;"
	goto	get# ;#
	goto	getDollar ;$
	goto	getPct ;%
	goto	getAnd ;&
	goto	getSingleQuote ;'
	goto	getParenLeft ;(
	goto	getParenRight ;)
	goto	getAsterisk ;*
	goto	getPlus ;+
	goto	getComma ;,
	goto	getDash ;-
	goto	getPeriod ;.
	goto	getFrontslash ;/
	goto	getZero ;0
	goto	get1 ;1
	goto	get2 ;2
	goto	get3 ;3
	goto	get4 ;4
	goto	get5 ;5
	goto	get6 ;6
	goto	get7 ;7
	goto	get8 ;8
	goto	get9 ;9
	goto	getColon ;:
	goto	getSemicolon ;;
	goto 	getLessThan ;<
	goto	getEqu ;=
	goto	getGreaterThan ;>
	goto	getQuestion ;?
	goto	getAt ;@
	goto	getA
	goto	getB
	goto	getC
	goto	getD
	goto	getE
	goto	getF
	goto	getG
	goto	getH
	goto	getI
	goto	getJ
	goto	getK
	goto	getL
	goto	getM
	goto	getN
	goto	getO
	goto	getP
	goto	getQ
	goto	getR
	goto	getS
	goto	getT
	goto	getU
	goto	getV
	goto	getW
	goto	getX
	goto	getY
	goto	getZ
	goto	getBracketLeft ;[
	goto	getBackslash ;\
	goto	getBracketRight ;]
	goto	getCarat ;^
	goto	getUnderscore ;_
	goto	getAccentGrave ;`
	;*Lowercase Letters* (Currently mapped to uppercase)
	goto 	getA
	goto	getB
	goto	getC
	goto	getD
	goto	getE
	goto	getF
	goto	getG
	goto	getH
	goto	getI
	goto	getJ
	goto	getK
	goto	getL
	goto	getM
	goto	getN
	goto	getO
	goto	getP
	goto	getQ
	goto	getR
	goto	getS
	goto	getT
	goto	getU
	goto	getV
	goto	getW
	goto	getX
	goto	getY
	goto	getZ
	goto	getBraceLeft ;{
	goto	getBar ;|
	goto	getBraceRight ;}
	goto	getTilde ;~

;*****FONT TABLES*****
getSpace:
	maxcols 0x08
	retlw	0x00

getExcl:
	movlw	HIGH	getExcl
	movwf	PCLATH
	maxcols	D'4'
	movfw	COL
	addwf	PCL, F
	retlw	0xFA ;11111010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getDoubleQuote:
	movlw	HIGH	getDoubleQuote
	movwf	PCLATH
	maxcols	D'07'
	movfw	COL
	addwf	PCL, F
	retlw	b'11000000'
	retlw	0x00
	retlw	0x00
	retlw	b'11000000'
	retlw	0x00
	retlw	0x00
	retlw	0x00

get#:
	movlw	HIGH	get#
	movwf	PCLATH
	maxcols	D'11'
	movfw	COL
	addwf	PCL, F
	retlw	0x48 ;01001000
	retlw	0x00
	retlw	0xFC ;11111100
	retlw	0x00
	retlw	0x48 ;01001000
	retlw	0x00
	retlw	0xFC ;11111100
	retlw	0x00
	retlw	0x48 ;01001000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getDollar:
	movlw	HIGH	getDollar
	movwf	PCLATH
	maxcols	D'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x24 ;00100100
	retlw	0x00
	retlw	0x54 ;01010100
	retlw	0x00
	retlw	0xD6 ;11010110
	retlw	0x00
	retlw	0x54 ;01010100
	retlw	0x00
	retlw	0x48 ;01001000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getPct:
	movlw	HIGH	getPct
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	b'00000010'
	retlw	b'01000100'
	retlw	b'00001000'
	retlw	b'00010000'
	retlw	b'00100000'
	retlw	b'01000010'
	retlw	b'10000000'
	retlw	0x00
	retlw	0x00
	retlw	0x00

getAnd:
	movlw	HIGH	getAnd
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x6C ;01101100
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x6A ;01101010
	retlw	0x00
	retlw	0x04 ;00000100
	retlw	0x00
	retlw	0x0A ;00001010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getSingleQuote:
	movlw	HIGH	getSingleQuote
	movwf	PCLATH
	maxcols	d'04'
	movfw	COL
	addwf	PCL, F
	retlw	b'11000000'
	retlw	0x00
	retlw	0x00
	retlw	0x00

getParenLeft:
	movlw	HIGH	getParenLeft
	movwf	PCLATH
	maxcols	d'7'
	movfw	COL
	addwf	PCL, F
	retlw	0x38 ;00111000
	retlw	0x44 ;01000100
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getParenRight:
	movlw	HIGH	getParenRight
	movwf	PCLATH
	maxcols	d'7'
	movfw	COL
	addwf	PCL, F
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x44 ;01000100
	retlw	0x38 ;00111000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getAsterisk:
	movlw	HIGH	getAsterisk
	movwf	PCLATH
	maxcols	D'12'
	movfw	COL
	addwf	PCL, F
	retlw	B'00100000'
	retlw	B'00000000'
	retlw	B'00101000'
	retlw	B'00010000'
	retlw	B'11100000'
	retlw	B'00010000'
	retlw	B'00101000'
	retlw	B'00000000'
	retlw	B'00100000'
	retlw	B'00000000'
	retlw	B'00000000'
	retlw	B'00000000'

org	0x0400

getPlus:
	movlw	HIGH	getPlus
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x20 ;00100000
	retlw	0x00
	retlw	0x20 ;00100000
	retlw	0x00
	retlw	0xF8 ;11111000
	retlw	0x00
	retlw	0x20 ;00100000
	retlw	0x00
	retlw	0x20 ;00100000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getComma:
	movlw	HIGH	getComma
	movwf	PCLATH
	maxcols	d'6'
	movfw	COL
	addwf	PCL, F
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x04 ;00000100
	retlw	0x00
	retlw	0x00
	retlw	0x00

getDash:
	movlw	HIGH	getDash
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getPeriod:
	movlw	HIGH	getPeriod
	movwf	PCLATH
	maxcols	d'4'
	movfw	COL
	addwf	PCL, F
	retlw	b'00000100'
	retlw	0x00
	retlw	0x00
	retlw	0x00

getFrontslash:
	movlw	HIGH	getFrontslash
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x02 ;00000010
	retlw	0x04 ;00000100
	retlw	0x08 ;00001000
	retlw	0x10 ;00010000
	retlw	0x20 ;00100000
	retlw	0x40 ;01000000
	retlw	0x80 ;10000000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getZero:
	movlw	HIGH	getZero
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x7C ;01111100
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x7C ;01111100
	retlw	0x00
	retlw	0x00
	retlw	0x00

get1:
	movlw	HIGH	get1
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x42 ;01000010
	retlw	0x00
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

get2:
	movlw	HIGH	get2
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x46 ;01000110
	retlw	0x00
	retlw	0x8A ;10001010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x62 ;01100010
	retlw	0x00
	retlw	0x00

get3:
	movlw	HIGH	get3
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x44 ;01000100
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x6C ;01101100
	retlw	0x00
	retlw	0x00
	retlw	0x00

get4:
	movlw	HIGH	get4
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	b'00110000'
	retlw	0x00
	retlw	b'01010000'
	retlw	0x00
	retlw	b'10010000'
	retlw	0x00		
	retlw	b'11111110'
	retlw	0x00
	retlw	0x00
	retlw	0x00

get5:
	movlw	HIGH	get5
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	b'11100100'
	retlw	0x00
	retlw	b'10100010'
	retlw	0x00
	retlw	b'10100010'
	retlw	0x00
	retlw	b'10111100'
	retlw	0x00
	retlw	0x00
	retlw	0x00

get6:
	movlw	HIGH	get6
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	b'01111100'
	retlw	0x00
	retlw	b'10010010'
	retlw	0x00
	retlw	b'10010010'
	retlw	0x00
	retlw	b'00001100'
	retlw	0x00
	retlw	0x00
	retlw	0x00

get7:
	movlw	HIGH	get7
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x80 ;10000000
	retlw	0x02 ;00000010
	retlw	0x84 ;10000100
	retlw	0x08 ;00001000
	retlw	0x90 ;10010000
	retlw	0x20 ;00100000
	retlw	0xC0 ;11000000
	retlw	0x00
	retlw	0x00
	retlw	0x00

get8:
	movlw	HIGH	get8
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x6C ;01101100
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x6C ;01101100
	retlw	0x00
	retlw	0x00
	retlw	0x00

get9:
	movlw	HIGH	get9
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	b'01100000'
	retlw	0x00
	retlw	b'10010010'
	retlw	0x00
	retlw	b'10010010'
	retlw	0x00
	retlw	b'01111100'
	retlw	0x00
	retlw	0x00
	retlw	0x00

getColon:
	movlw	HIGH	getColon
	movwf	PCLATH
	maxcols	d'4'
	movfw	COL
	addwf	PCL, F
	retlw	0x44 ;01000100
	retlw	0x00
	retlw	0x00
	retlw	0x00

getSemicolon:
	movlw	HIGH	getSemicolon
	movwf	PCLATH
	maxcols	d'6'
	movfw	COL
	addwf	PCL, F
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x44 ;01000100
	retlw	0x00
	retlw	0x00
	retlw	0x00

org	0x0500

getLessThan:
	movlw	HIGH	getLessThan
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0x28 ;00101000
	retlw	0x00
	retlw	0x44 ;01000100
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getEqu:
	movlw	HIGH	getEqu
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x28 ;00101000
	retlw	0x00
	retlw	0x28 ;00101000
	retlw	0x00
	retlw	0x28 ;00101000
	retlw	0x00
	retlw	0x28 ;00101000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getGreaterThan:
	movlw	HIGH	getGreaterThan
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x44 ;01000100
	retlw	0x00
	retlw	0x28 ;00101000
	retlw	0x00
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getQuestion:
	movlw	HIGH	getQuestion
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x40 ;01000000
	retlw	0x00
	retlw	0x9A ;10011010
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x60 ;01100000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getAt:
	movlw	HIGH	getAt
	movwf	PCLATH
	maxcols	d'14'
	movfw	COL
	addwf	PCL, F
	retlw	0x7C ;01111100
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0xBA ;10111010
	retlw	0x00
	retlw	0xAA ;10101010
	retlw	0x00
	retlw	0xBA ;10111010
	retlw	0x00
	retlw	0x8A ;10001010
	retlw	0x70 ;01110000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getA:
	movlw	HIGH	getA
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x7E ;01111110
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x7E ;01111110
	retlw	0x00
	retlw	0x00
	retlw	0x00

getB:
	movlw	HIGH	getB
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x6C ;01101100
	retlw	0x00
	retlw	0x00
	retlw	0x00

getC:
	movlw	HIGH	getC
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x7C ;01111100
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x44 ;01000100
	retlw	0x00
	retlw	0x00
	retlw	0x00

getD:
	movlw	HIGH	getD
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x7C ;01111100
	retlw	0x00
	retlw	0x00
	retlw	0x00

getE:
	movlw	HIGH	getE
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00	
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getF:
	movlw	HIGH	getF
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x80 ;10000000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getG:
	movlw	HIGH	getG
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x7C ;01111100
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x5E ;01011110
	retlw	0x00
	retlw	0x00
	retlw	0x00

getH:
	movlw	HIGH	getH
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x00
	retlw	0x00

getI:
	movlw	HIGH	getI
	movwf	PCLATH
	maxcols	d'8'
	movfw	COL
	addwf	PCL, F
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

org	0x0600

getJ:
	movlw	HIGH	getJ
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x04 ;00000100
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x80 ;10000000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getK:
	movlw	HIGH	getK
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0x28 ;00101000
	retlw	0x00
	retlw	0x44 ;01000100
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getL:
	movlw	HIGH	getL
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getM:
	movlw	HIGH	getM
	movwf	PCLATH
	maxcols	d'14'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x40 ;01000000
	retlw	0x20 ;00100000
	retlw	0x10 ;00010000
	retlw	0x08 ;00001000
	retlw	0x10 ;00010000
	retlw	0x20 ;00100000
	retlw	0x40 ;01000000
	retlw	0x00
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x00
	retlw	0x00

getN:
	movlw	HIGH	getN
	movwf	PCLATH
	maxcols	d'13'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x40 ;01000000
	retlw	0x20 ;00100000
	retlw	0x10 ;00010000
	retlw	0x08 ;00001000
	retlw	0x04 ;00000100
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x00
	retlw	0x00

getO:
	movlw	HIGH	getO
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x7C ;01111100
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x7C ;01111100
	retlw	0x00
	retlw	0x00
	retlw	0x00

getP:
	movlw	HIGH	getP
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x60 ;01100000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getQ:
	movlw	HIGH	getQ
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	B'01111000'
	retlw	0x00
	retlw	B'10000100'
	retlw	0x00
	retlw	B'10000100'
	retlw	0x00
	retlw	B'10000100'
	retlw	0x00
	retlw	B'01111010'
	retlw	0x00
	retlw	0x00
	retlw	0x00

getR:
	movlw	HIGH	getR
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x90 ;10010000
	retlw	0x00
	retlw	0x6E ;01101110
	retlw	0x00
	retlw	0x00
	retlw	0x00

getS:
	movlw	HIGH	getS
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x62 ;01100010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0x8C ;10001100
	retlw	0x00
	retlw	0x00
	retlw	0x00

getT:
	movlw	HIGH	getT
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x80 ;10000000
	retlw	0x00
	retlw	0x80 ;10000000
	retlw	0x00
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x80 ;10000000
	retlw	0x00
	retlw	0x80 ;10000000
	retlw	0x00
	retlw	0x00
	retlw	0x00

getU:
	movlw	HIGH	getU
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0xFC ;11111100
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0xFC ;11111100
	retlw	0x00
	retlw	0x00
	retlw	0x00

getV:
	movlw	HIGH	getV
	movwf	PCLATH
	maxcols	d'14'
	movfw	COL
	addwf	PCL, F
	retlw	b'11000000'
	retlw	b'00100000'
	retlw	b'00010000'
	retlw	b'00001000'
	retlw	b'00000100'
	retlw	b'00000010'
	retlw	b'00000100'
	retlw	b'00001000'
	retlw	b'00010000'
	retlw	b'00100000'
	retlw	b'11000000'	
	retlw	0x00
	retlw	0x00
	retlw	0x00

org 0x0700

getW:
	movlw	HIGH	getW
	movwf	PCLATH
	maxcols	d'14'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x04 ;00000100
	retlw	0x08 ;00001000
	retlw	0x10 ;00010000
	retlw	0x20 ;00100000
	retlw	0x10 ;00010000
	retlw	0x08 ;00001000
	retlw	0x04 ;00000100
	retlw	0x00
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x00
	retlw	0x00

getX:
	movlw	HIGH	getX
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	b'10000010'
	retlw	b'01000100'
	retlw	b'00101000'
	retlw	b'00010000'
	retlw	b'00101000'
	retlw	b'01000100'
	retlw	b'10000010'
	retlw	0x00
	retlw	0x00
	retlw	0x00

getY:
	movlw	HIGH	getY
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	b'10000000'
	retlw	b'01000000'
	retlw	b'00100000'
	retlw	b'00010000'
	retlw	b'00001110'
	retlw	b'00010000'
	retlw	b'00100000'
	retlw	b'01000000'
	retlw	b'10000000'
	retlw	0x00
	retlw	0x00
	retlw	0x00

getZ:
	movlw	HIGH	getZ
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x86 ;10000110
	retlw	0x00
	retlw	0x8A ;10001010
	retlw	0x00
	retlw	0x92 ;10010010
	retlw	0x00
	retlw	0xA2 ;10100010
	retlw	0x00
	retlw	0xC2 ;11000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getBracketLeft:
	movlw	HIGH	getBracketLeft
	movwf	PCLATH
	maxcols	d'6'
	movfw	COL
	addwf	PCL, F
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getBackslash:
	movlw	HIGH	getBackslash
	movwf	PCLATH
	maxcols	d'10'
	movfw	COL
	addwf	PCL, F
	retlw	0x80 ;10000000
	retlw	0x40 ;01000000
	retlw	0x20 ;00100000
	retlw	0x10 ;00010000
	retlw	0x08 ;00001000
	retlw	0x04 ;00000100
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getBracketRight:
	movlw	HIGH	getBracketRight
	movwf	PCLATH
	maxcols	d'6'
	movfw	COL
	addwf	PCL, F
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0xFE ;11111110
	retlw	0x00
	retlw	0x00
	retlw	0x00
	
getCarat:
	movlw	HIGH	getCarat
	movwf	PCLATH
	maxcols	D'12'
	movfw	COL
	addwf	PCL, F
	retlw	B'00100000'
	retlw	B'00000000'
	retlw	B'01000000'
	retlw	B'00000000'
	retlw	B'10000000'
	retlw	B'00000000'
	retlw	B'01000000'
	retlw	B'00000000'
	retlw	B'00100000'
	retlw	B'00000000'
	retlw	B'00000000'
	retlw	B'00000000'

getUnderscore:
	movlw	HIGH	getUnderscore
	movwf	PCLATH
	maxcols	d'12'
	movfw	COL
	addwf	PCL, F
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x02 ;00000010
	retlw	0x00
	retlw	0x00
	retlw	0x00
	
getAccentGrave:
	movlw	HIGH	getAccentGrave
	movwf	PCLATH
	maxcols	D'5'
	movfw	COL
	addwf	PCL, F
	retlw	B'10000000'
	retlw	B'01000000'
	retlw	B'00000000'
	retlw	B'00000000'
	retlw	B'00000000'

getBraceLeft:
	movlw	HIGH	getBraceLeft
	movwf	PCLATH
	maxcols	d'6'
	movfw	COL
	addwf	PCL, F
	retlw	0x10 ;00010000
	retlw	0x6C ;01101100
	retlw	0x82 ;10000010
	retlw	0x00
	retlw	0x00
	retlw	0x00

getBar: ;bar is printed as 2 bars with gap inbetween vertically
	movlw	HIGH	getBar
	movwf	PCLATH
	maxcols	d'4'
	movfw	COL
	addwf	PCL, F
	retlw	0xEE ;11101110
	retlw	0x00
	retlw	0x00
	retlw	0x00

getBraceRight:
	movlw	HIGH	getBraceRight
	movwf	PCLATH
	maxcols	d'6'
	movfw	COL
	addwf	PCL, F
	retlw	0x82 ;10000010
	retlw	0x6C ;01101100
	retlw	0x10 ;00010000
	retlw	0x00
	retlw	0x00
	retlw	0x00
	
getTilde:
	movlw	HIGH	getTilde
	movwf	PCLATH
	maxcols	D'10'
	movfw	COL
	addwf	PCL, F
	retlw	B'01000000'
	retlw	B'00000000'
	retlw	B'10000000'
	retlw	B'00000000'
	retlw	B'01000000'
	retlw	B'00000000'
	retlw	B'10000000'
	retlw	B'00000000'
	retlw	B'00000000'
	retlw	B'00000000'

;****MESSAGE TABLE****
message_table: ;when called, will have number of letter in w
	addwf	PCL, F
	dt	" *VYASSA L. BARATHAM*                       "

end
