;    Last change:  MWL  21 Jul 99    1:34 pm

;**** A P P L I C A T I O N   N O T E   A V R 2 0 1 ***************************
;*
;* Title		: 16bit multiply routines using hardware multiplier
;* Version		: V1.0
;* Last updated		: 5 Apr, 1999
;* Target		: ATmega83/161/163
;*
;* Support email	: avr@atmel.com
;*
;* DESCRIPTION
;* 	This application note shows a number of examples of how to implement
;*	16bit multiplication using hardware multiplier. Refer to each of the
;*	funtions headers for details. The functions included in this file
;*	are :
;*
;*	mul16x16_16	- Multiply of two 16bits numbers with 16bits result.
;*	mul16x16_32	- Unsigned multiply of two 16bits numbers with 32bits
;*			  result.
;*	muls16x16_32	- Signed multiply of two 16bits numbers with 32bits
;*			  result.
;*	mac16x16_32	- Signed multiply accumulate of two 16bits numbers
;*			  with a 32bits result.
;*	fmuls16x16_32	- Signed fractional multiply of two 16bits numbers
;*			  with 32bits result.
;*	fmac16x16_32	- Signed fractional multiply accumulate of two 16bits
;*			  numbers with a 32bits result.
;*
;******************************************************************************


;******************************************************************************
;*
;* FUNCTION
;*	mul16x16_16
;* DECRIPTION
;*	Multiply of two 16bits numbers with 16bits result.
;* USAGE
;*	r17:r16 = r23:r22 * r21:r20
;* STATISTICS
;*	Cycles :	9 + ret
;*	Words :		6 + ret
;*	Register usage: r0, r1 and r16 to r23 (8 registers)
;* NOTE
;*	Full orthogonality i.e. any register pair can be used as long as
;*	the result and the two operands does not share register pairs.
;*	The routine is non-destructive to the operands.
;*
;******************************************************************************

mul16x16_16:
	mul	r22, r20		; al * bl
	movw	r17:r16, r1:r0
	mul	r23, r20		; ah * bl
	add	r17, r0
	mul	r21, r22		; bh * al
	add	r17, r0
	ret


;******************************************************************************
;*
;* FUNCTION
;*	mul16x16_32
;* DECRIPTION
;*	Unsigned multiply of two 16bits numbers with 32bits result.
;* USAGE
;*	r19:r18:r17:r16 = r23:r22 * r21:r20
;* STATISTICS
;*	Cycles :	17 + ret
;*	Words :		13 + ret
;*	Register usage: r0 to r2 and r16 to r23 (11 registers)
;* NOTE
;*	Full orthogonality i.e. any register pair can be used as long as
;*	the 32bit result and the two operands does not share register pairs.
;*	The routine is non-destructive to the operands.
;*
;******************************************************************************

mul16x16_32:
	clr	r2
	mul	r23, r21		; ah * bh
	movw	r19:r18, r1:r0
	mul	r22, r20		; al * bl
	movw	r17:r16, r1:r0
	mul	r23, r20		; ah * bl
	add	r17, r0
	adc	r18, r1
	adc	r19, r2
	mul	r21, r22		; bh * al
	add	r17, r0
	adc	r18, r1
	adc	r19, r2
	ret


;******************************************************************************
;*
;* FUNCTION
;*	muls16x16_32
;* DECRIPTION
;*	Signed multiply of two 16bits numbers with 32bits result.
;* USAGE
;*	r19:r18:r17:r16 = r23:r22 * r21:r20
;* STATISTICS
;*	Cycles :	19 + ret
;*	Words :		15 + ret
;*	Register usage: r0 to r2 and r16 to r23 (11 registers)
;* NOTE
;*	The routine is non-destructive to the operands.
;*
;******************************************************************************

muls16x16_32:
	clr	r2
	muls	r23, r21		; (signed)ah * (signed)bh
	movw	r19:r18, r1:r0
	mul	r22, r20		; al * bl
	movw	r17:r16, r1:r0
	mulsu	r23, r20		; (signed)ah * bl
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2
	mulsu	r21, r22		; (signed)bh * al
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2
	ret


;******************************************************************************
;*
;* FUNCTION
;*	mac16x16_32
;* DECRIPTION
;*	Signed multiply accumulate of two 16bits numbers with
;*	a 32bits result.
;* USAGE
;*	r19:r18:r17:r16 += r23:r22 * r21:r20
;* STATISTICS
;*	Cycles :	23 + ret
;*	Words :		19 + ret
;*	Register usage: r0 to r2 and r16 to r23 (11 registers)
;*
;******************************************************************************

mac16x16_32:
	clr	r2

	muls	r23, r21		; (signed)ah * (signed)bh
	add	r18, r0
	adc	r19, r1

	mul	r22, r20		; al * bl
	add	r16, r0
	adc	r17, r1
	adc	r18, r2
	adc	r19, r2

	mulsu	r23, r20		; (signed)ah * bl
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2

	mulsu	r21, r22		; (signed)bh * al
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2

	ret

mac16x16_32_method_B:			; uses two temporary registers
					; (r4,r5), but reduces cycles/words
					; by 1
	clr	r2

	muls	r23, r21		; (signed)ah * (signed)bh
	movw	r5:r4,r1:r0

	mul	r22, r20		; al * bl

	add	r16, r0
	adc	r17, r1
	adc	r18, r4
	adc	r19, r5

	mulsu	r23, r20		; (signed)ah * bl
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2

	mulsu	r21, r22		; (signed)bh * al
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2

	ret


;******************************************************************************
;*
;* FUNCTION
;*	fmuls16x16_32
;* DECRIPTION
;*	Signed fractional multiply of two 16bits numbers with 32bits result.
;* USAGE
;*	r19:r18:r17:r16 = ( r23:r22 * r21:r20 ) << 1
;* STATISTICS
;*	Cycles :	20 + ret
;*	Words :		16 + ret
;*	Register usage: r0 to r2 and r16 to r23 (11 registers)
;* NOTE
;*	The routine is non-destructive to the operands.
;*
;******************************************************************************

fmuls16x16_32:
	clr	r2
	fmuls	r23, r21		; ( (signed)ah * (signed)bh ) << 1
	movw	r19:r18, r1:r0
	fmul	r22, r20		; ( al * bl ) << 1
	adc	r18, r2
	movw	r17:r16, r1:r0
	fmulsu	r23, r20		; ( (signed)ah * bl ) << 1
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2
	fmulsu	r21, r22		; ( (signed)bh * al ) << 1
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2
	ret

	
;******************************************************************************
;*
;* FUNCTION
;*	fmac16x16_32
;* DECRIPTION
;*	Signed fractional multiply accumulate of two 16bits numbers with
;*	a 32bits result.
;* USAGE
;*	r19:r18:r17:r16 += (r23:r22 * r21:r20) << 1
;* STATISTICS
;*	Cycles :	25 + ret
;*	Words :		21 + ret
;*	Register usage: r0 to r2 and r16 to r23 (11 registers)
;*
;******************************************************************************

fmac16x16_32:
	clr	r2

	fmuls	r23, r21		; ( (signed)ah * (signed)bh ) << 1
	add	r18, r0
	adc	r19, r1

	fmul	r22, r20		; ( al * bl ) << 1
	adc	r18, r2
	adc	r19, r2
	add	r16, r0
	adc	r17, r1
	adc	r18, r2
	adc	r19, r2

	fmulsu	r23, r20		; ( (signed)ah * bl ) << 1
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2

	fmulsu	r21, r22		; ( (signed)bh * al ) << 1
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2

	ret

fmac16x16_32_method_B:			; uses two temporary registers
					; (r4,r5), but reduces cycles/words
					; by 2
	clr	r2

	fmuls	r23, r21		; ( (signed)ah * (signed)bh ) << 1
	movw	r5:r4,r1:r0
	fmul	r22, r20		; ( al * bl ) << 1
	adc	r4, r2

	add	r16, r0
	adc	r17, r1
	adc	r18, r4
	adc	r19, r5

	fmulsu	r23, r20		; ( (signed)ah * bl ) << 1
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2
	fmulsu	r21, r22		; ( (signed)bh * al ) << 1
	sbc	r19, r2
	add	r17, r0
	adc	r18, r1
	adc	r19, r2

	ret


;***************************************************************************
;*
;* "div24x24_24" - 24/24 Bit Unsigned Division
;*
;* This subroutine divides the two 24-bit numbers 
;* "r24:r23:r22" (dividend) and "r21:r20:r19" (divisor). 
;* The result is placed in "r24:r23:r22" and the remainder in
;* "r18:r17:r16".
;*  
;* Number of words	:
;* Number of cycles	:
;* Low registers used	:3 (r16,r17 r18)
;* High registers used  :6 (r19/r22, r20/r23, r21/r24,r19,r20,r21)
;* r24:r23:r22 = r24:r23:r22 / r21:r20:r19
;***************************************************************************

;***** Subroutine Register Variables
div24x24_24:
;***** Code

	clr	r16	;clear remainder Low byte
	clr r17
	sub	r18,r18;clear remainder High byte and carry
	ldi	r25,25	;init loop counter
d24u_1:	rol	r22		;shift left dividend
	rol r23
	rol	r24
	dec	r25		;decrement counter
	brne	d24u_2		;if done
	ret			;    return
d24u_2:	rol	r16	;shift dividend into remainder
	rol r17
	rol	r18
	sub	r16,r19	;remainder = remainder - divisor
	sbc r17,r20 ;
	sbc	r18,r21	;
	brcc	d24u_3		;if result negative
	add	r16,r19	;    restore remainder
	adc r17,r20
	adc	r18,r21
	clc			;    clear carry to be shifted into result
	rjmp	d24u_1		;else
d24u_3:	sec			;    set carry to be shifted into result
	rjmp	d24u_1
	
	ret
	

;***************************************************************************
;*
;* "div8u" - 8/8 Bit Unsigned Division
;*
;* This subroutine divides the two register variables "dd8u" (dividend) and 
;* "dv8u" (divisor). The result is placed in "dres8u" and the remainder in
;* "drem8u".
;*  
;* Number of words	:14
;* Number of cycles	:97
;* Low registers used	:1 (drem8u)
;* High registers used  :3 (dres8u/dd8u,dv8u,dcnt8u)
;* r21 = r21/r22
;***************************************************************************

;***** Subroutine Register Variables

;r20		;remainder
;r21		;result
;r21		;dividend
;r22		;divisor
;r23		;loop counter

;***** Code

div8u:	sub	r20,r20	;clear remainder and carry
		ldi	r23,9	;init loop counter
d8u_1:	rol	r21		;shift left dividend
		dec	r23		;decrement counter
		brne	d8u_2		;if done
		ret			;    return
d8u_2:	rol	r20		;shift dividend into remainder
		sub	r20,r22	;remainder = remainder - divisor
		brcc	d8u_3		;if result negative
		add	r20,r22	;    restore remainder
		clc			;    clear carry to be shifted into result
		rjmp	d8u_1		;else
d8u_3:	sec			;    set carry to be shifted into result
		rjmp	d8u_1



;***************************************************************************
;*
;* Mecheng 313
;*
;* 
;*
;* 
;* 
;*  
;* 
;*
;* 
;* 
;* 	
;***************************************************************************


.equ	ClockTick_Task = 2
.equ	UpTime = -1	

System_Monitor:
				rcall div24x24_24
				rcall div8u
				ret

.MACRO Start_Task ; Start macro definition
		sbi DDRC,(PC4 + @0)
		sbi PORTC,(PC4 + @0)
.ENDMACRO

.MACRO End_Task ; Start macro definition
		cbi PORTC,(PC4 +@0)
.ENDMACRO


;**** End of File ****
	
