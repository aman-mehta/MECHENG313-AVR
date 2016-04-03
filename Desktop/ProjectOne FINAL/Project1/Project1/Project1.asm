/*
 * lab2.asm
 *
 *  Created: 24/08/2015 4:20:02 p.m.
 *   Author: ameh428, dalv414
 */ 

 .nolist					;Turn listfile generation Off
.include "m8def.inc"		;All defines(Ports, Interrupt Table, etc...) needed for the Atmega8

.list						;Turn listfile generation On

;Define Bits for indicator Data
.equ	RSTATUS = 0
.equ	LSTATUS = 1
.equ	RFLASH = 2
.equ	LFLASH = 3
.equ    RCOND = 4
.equ    LCOND = 5
.equ	RPSTATE = 6
.equ	LPSTATE	= 7

;Define Bits for car door Data
.equ	CDLEDON = 0
.equ	CDSTATE	= 1	

;Define Bits for r28 (The Priority Status Register)
.equ    MTASK_HOLD = 6
.equ    MTASK_QUEUE = 7

;Define threshold force
.equ	THRESHOLDFORCE = 4

;Define Reserved Registers
.def	ISREG = r16

;Define indicator overflow (clock = 1.25ms)
.equ	LONGFLASH = 400		
.equ	SHORTFLASH = 200		

.dseg 						; Start data segment
.org 0x67 					; Set SRAM address to hex 67

pulseWidth:				.byte 1		
fuelInjectionCounts:	.byte 1
rightIndCounterH:		.byte 1
rightIndCounterL:		.byte 1
leftIndCounterH:		.byte 1
leftIndCounterL:		.byte 1
tempCelsius:			.byte 1
waterLevelLitres:		.byte 1 

.cseg
.org $00000					;Setting Origin Address
		rjmp Main 			;Reset vector
.org OVF0addr				;Setting Origin Address
		rjmp ClockTick 		;ClockTick vector
.org ADCCaddr						;Setting Origin Address
		rjmp Collision_Detection

.org   0x0100               ;table address engine speed (RPM) and load
;Load/RPM('000)
RPMLoad_Lookup:			;1/1	2/1		3/1		4/1		1/2		2/2 	3/2		4/2		1/3		2/3		3/3		4/3		1/4		2/4		3/4		4/4   
	        .db			0x01,	0x02,	0x03,	0x04,	0x02,	0x04,	0x06,	0x08,	0x03,	0x06,	0x09,	0x0C,	0x04,	0x08,	0x0C,	0x10
;FactorA * 10
FactorA_Lookup:			;0		25		50		75		
			.db			0x0c,	0x0b,	0x0a,	0x09	
;FactorB * 4				
FactorB_Lookup:			;1		2		3		4		
			.db			0x04,	0x04,	0x04,	0x03	
	
.org $00200					;Setting Origin Address
.include "MECHENG313A.inc"	;Functions needed for MECHENG313

;***************** Start of Main *****************                                
Main: 

		;*******Initialise Stack Pointer*******
		ldi r16,LOW(RAMEND) 	;Loading Lower Ram end address in to r16
		out SPL,r16				;Init Stack Pointer Lower Bytes
		ldi r16,HIGH(RAMEND)	;Loading Higher Ram end address in to r16
		out SPH,r16				;Init Stack Pointer Higher Bytes
		
		;*******I/O Setup*******
		ldi r16, 0xFF
		out DDRB,r16  ; PORT B is Output
		out PORTB,r16 ; initialise LEDs as OFF
		out DDRC, r16 ; PORT C is Output

		;********* ADC ********
		; set MUX to channel 2, left adjust the result, AREF taken from AVCC
		ldi r16, (1<<MUX0) ; ADMUX channel 2, AREF from AVCC PORTC
		out ADMUX,r16
		; switch AD conversion on, start conversion, divider rate = 16
		ldi r16, (1<<ADEN)|(1<<ADSC)|(1<<ADPS2)|(1<<ADFR)|(1<<ADIE) ; switch AD conversion on
		out ADCSRA,r16


		cbi DDRC,PC1		;PC1 is used for ADC

		;********* ClockTick 8-bit Timer/Counter 0 *******      
		ldi r16, (1<<CS01)|(1<<CS00)            
      	out TCCR0, r16			; Timer Clock = Sys Clock (1MHz) / 64 (prescaler)
		ldi r16, (1<<TOIE0)            
		out TIMSK, r16			; Enable Timer Overflow interrupt

		ldi r16, 217		; MaxValue = TOVck (1.5ms or your Cal time) * Pck (1MHz) / 8 (prescaler)
		out TCNT0, r16			; TCNT0Value = 255 - MaxValue
	
		sei ; enable interrupts and off we go!



		;*******Counters and Indicators Setup*******
		;Initialise indicator counters to 0 and Fuel Injection Counter to 1
		ldi r16, $00
		ldi r17, $00
		sts rightIndCounterH, r17
		sts	rightIndCounterL, r16
		sts	leftIndCounterH, r17
		sts	leftIndCounterL, r16

		ldi r16, $01			
		sts fuelInjectionCounts, r16
		sts pulseWidth, r16

		ldi ISREG, $ff ;set all flags to 1
		sbr r28, (1<<CDLEDON)|(1<<CDSTATE) ;initialise LED to off in
		
		;********* Main infinite loop ********
forever:
		
		sbr r28, (1<<MTASK_HOLD) ;While asynchronous tasks do not excute Monitoring Tasks

		rcall Left_Status_Switch
		rcall Right_Status_Switch
		rcall Left_Condition_Toggle
		rcall Right_Condition_Toggle
		rcall Left_LED_Toggle
		rcall Right_LED_Toggle
		rcall Car_Door_Indicator

		cbr r28, (1<<MTASK_HOLD) ;While the button is pressed do not excute Monitoring Tasks

		rjmp forever 
;***************** End of program *****************

;******* Start of Clock Tick *******
ClockTick:
		sbi PORTC, PC4
		
		sei ; Allow interrupts to give ADC interrupt priority

		;Push registers icluding SREG to prevent overwriting data 
		push r17
		push r18
		push r19
		push r20
		push r21
		push r22
		push r23
		push r24
		push r25
		push r26
		push r27
		push r29
		push r30
		push r31
		in r17, SREG
		push r17
		
		; reset counter
		ldi r18, 217
		out TCNT0, r18 
		
		;Start synchronous tasks

		rcall Fuel_Injection_Counter	;Run the Fuel Injection Calculation if the Clock Tick count is correct
		rcall Left_Indicator_Manager	;Run Left Indicator Manager
		rcall Right_Indicator_Manager	;Run Right Indicator Manager

		; Check if higher priority tasks are running before executing Monitoring Tasks
		bst r28, MTASK_HOLD 
		brts runMonitoringTasks
		addToQueue:							;If a higher priority task is running, add it to a queue
			sbr r28, (1<<MTASK_QUEUE) 
			rjmp endMonitoringTasks
		runMonitoringTasks:					;Else run Monitoring Tasks
			rcall Monitoring_Tasks
		endMonitoringTasks:
		
		;Push registers icluding SREG to restore data
		pop r17
		out SREG, r17
		pop r31
		pop r30
		pop r29
		pop r27
		pop r26
		pop r25
		pop r24
		pop r23
		pop r22
		pop r21
		pop r20
		pop r19
		pop r18
		pop r17
		
		cbi PORTC, PC4	;Turn output indicator pin Off
		RETI						;Return from Interurpt
;******* End of Clock Tick *******

;******* Start of FuelInjectionTimingCalc *******
Fuel_Injection_Timing_Calc:
		sbi PORTC, PC5;Turn output indicator pin On

;This task contains the algorithms to determine the fuel indjection pulseWidth.
;The task reads an input from the ADCL register to determine the operating
;conditions and calculates pulseWidth using the following formula:
;pulseWidth = (basePulseWidth * factorA(scaled) * factorB(scaled)) / 40
		
		push ISREG	;store indicator SREG on stack to free r16 for use

		in r16, ADCL
		clr r21
		clr r22

		; extract RPM Level into r21
		bst r16, 7
		bld r21, 1
		bst r16, 6
		bld r21, 0

		; extract Load Level into r22
		bst r16, 5
		bld r22, 1
		bst r16, 4
		bld r22, 0

		; algorithmForLookup = (loadLevel * 4) + rpmLevel
		ldi r23, $04
		mul r22, r23
		mov r22, r0
		add r22, r21

		; initialise Z pointer
		ldi ZH, HIGH(RPMLoad_Lookup*2)
		ldi ZL, LOW(RPMLoad_Lookup*2)
		ADC ZL, r22

		; store basePulseWidth into r24
		lpm r24, Z
		
		clr r21
		; extract coolantTemp into r21
		bst r16, 3
		bld r21, 1
		bst r16, 2
		bld r21, 0

		; initialise Z pointer
		ldi ZH, HIGH(FactorA_Lookup*2)
		ldi ZL, LOW(FactorA_Lookup*2)
		ADC ZL, r21

		; store factorA into r25
		lpm r25, Z
		
		; extract oxygenLevel into r21
		mov r21, r16
		andi r21, $03

		; initialise Z pointer
		ldi ZH, HIGH(FactorB_Lookup*2)
		ldi ZL, LOW(FactorB_Lookup*2)
		ADC ZL, r21

		; store factorB into r19
		lpm r19, Z

		; factorA * factorB
		mul r19, r25
		mov r22, r0
		
		; result * basePulseWidth
		mul r22, r24
		mov r22, r0
		mov r23, r1
		clr r24
		
		;result / 40
		ldi r19, $28
		clr r20
		clr r21

		rcall div24x24_24

		mov r20, r16
		mov r21, r22

		; check to round-up or round-down
		ldi r18, $14
		rcall RoundValue
		
		; store pulseWidth into SRAM
		sts pulseWidth, r21



		pop ISREG
		
		cbi PORTC, PC5

		RET
;******* End FuelInjectionTimingCalc *******

;******* Start Fuel Injection Counter *******
Fuel_Injection_Counter:
		
		push ISREG	;store indicator SREG on stack to free r16 for use

		lds r16, FuelInjectionCounts; Load fuel injection counts
		lds r21, pulseWidth; load in required pulsewidth
		ldi r23, $02
		ldi r22, $05

		;implement pulseWidth*2/5
		mul r23, r21
		mov r21, r0

		rcall div8u
		
		add r21, r20 

		; check current counts and required pulsewidth
		cp r16, r21;
		breq calculateTiming
		doNotCalculate:
			inc r16
			sts FuelInjectionCounts, r16
			rjmp EndTimingCheck
		calculateTiming:
			ldi r16, $01
			sts FuelInjectionCounts, r16
			rcall Fuel_Injection_Timing_Calc ;if the count meets the pulsewidth, recalculate the pulsewidth
		EndTimingCheck:

		pop ISREG

		RET
;******* End Fuel Injetcion Counter *******

;******* Start of Monitoring_Tasks *******
Monitoring_Tasks:	
	sbi PORTC, PC3
	

;This module reads data from ADCL. In the first case it reads in the data as car temp in fahrenheit,
;converts it to celcius and stores the result into SRAM. It then reads in the ADCL as the water level
;in ounces and converts this value to litres, and stores the result into SRAM. 
		
	push ISREG	;store indicator SREG on stack to free r16 for use

;******* Temperature Monitoring_Start *******
		
		in r21, ADCL	;Take Temp reading (in Farenheit) from ADCL and load in r16
		
		;Formula for Fahrenheit to Celsius:   C = ((F-32)*5)/9 
		
		ldi r19, $05    ;store 10 in r18
		ldi r22, $09	;store 18 in r19
		ldi r18, $05	;load value to check for rounding into r18
		
		bst r21,7		;store MSB of ADCL into T-flag
		subi r21, $20	;subi sets Nflag if result has a 1 in MSB

		brts largeNumber	;check whether the first bit of ADCL is 1 (i.e the number is large)
		smallNumber:
			rjmp endclear
		largeNumber:
			cln				;if the number was initially large clear the Nflag because its not negative
		endclear:

		brpl  positiveResult
		negativeResult:
			neg r21			;convert to positive
			rcall Mul_Frac
			rcall RoundValue
			neg r21			;convert back to negative
			rjmp endNegCheck
		positiveResult:
			rcall Mul_Frac
			rcall RoundValue
		endNegCheck:

		sts TempCelsius, r21 
	;******* Temperature Monitoring_End *******

	;******* Water Level Monitoring_Start *******
		
		in r21, ADCL	;take water level reading (in ounces) from ADCL and load into r16
		
		;Formula for ounces to litres L = Oz/33.8. This is implemented as L = (Oz*5)/169.
		ldi r19, $a9	;store 169 (33.8*5) into r19
		ldi r22, $05	;store scale number(5) in r22
		mul r21, r22	; Oz*5
		
		;Set registers for div24x24
		mov r22, r0		; move LByte of product to r22
		mov r23, r1		; move HByte of product to r23
		clr r24
		clr r20
		clr r21

		rcall div24x24_24 ; (Oz*5)/169

		;Set registers for Round Value
		ldi r18, $55 	;load value to check for rounding (85) into r18
		mov r21, r22
		mov r20, r16

		rcall RoundValue
		
		sts WaterLevelLitres, r21
;******* Water Level Monitoring_End *******
		
		cbr r28, (1<<MTASK_QUEUE)	;clear queue once Monitoring Tasks finishes
		pop ISREG

		cbi PORTC, PC3
RET
;******* End Monitoring_Tasks *******

;******* Start Car Door Indicator *******
Car_Door_Indicator: 

		sbi PORTC, PC2

;This module checks the state of the car door (whether it is open or closed) and switches on an
;LED in the case of it being open, and off in the case of it being closed.

		in r18, PinD
		bst r18, PD5	;Store bit 5 of Pin D (State of car door) in the TFlag

		;For the following code State refers to the bits. On the STK500 a state of 0 is ON and 1 is OFF

		;Toggle state of LED if current state = 1 and previous state = 0 (switch release)
		brts CurrentState1
		CurrentState0:
			rjmp endCurrentStateCheck
		CurrentState1:
			bst r28, CDState
			brtc PreviousState0
			PreviousState1:
				rjmp endPreviousStateCheck
			PreviousState0:
				bst r28, CDLEDOn
				brts CarDoorLEDOn
				CarDoorLEDOff:				; LED On -> LED Off
					sbi PORTB, PB5	
					sbr r28,(1<<CDLEDON)
					rjmp endCarDoorLED
				CarDoorLEDOn:				; LED Off -> LED On
					cbi PORTB, PB5
					cbr r28,(1<<CDLEDON)
				endCarDoorLED:
			endPreviousStateCheck:
		endCurrentStateCheck:
		
		;Update current state as previous state for next loop
		bst r18, 5
		bld r28, CDState

		;Check if Monitoring Tasks is in queue, if it is, execute
		sbrc r28, MTASK_QUEUE
		rcall Monitoring_Tasks

		cbi PORTC, PC2
		RET

;******* End Car Door Indicator *******

;******* Start of Left Indicator Manager *******
Left_Indicator_Manager:

;This module is responsible for setting up and calling the left counter to ensure
;the time interval between each flash is correct.

	; Check if the indicator switch is pressed (indicator is on)
	bst ISREG, LSTATUS
	brtc lIndicatorOn
	lIndicatorOff:			; Do nothing
		sbr ISREG, (1<<LFLASH)
		rjmp endLIndicator
	lIndicatorOn:			
		; If left indicator is on check if working or broken
		bst ISREG, LCOND
		brtc lIndicatorBroken
		lIndicatorWorking:
			; If it is working initialise long flash
			ldi r18, LOW(LONGFLASH)
			ldi r19, HIGH(LONGFLASH)
			rjmp endLIndicatorBroken
		lIndicatorBroken:
			; If it is broken initialise short flash
			ldi r18, LOW(SHORTFLASH)
			ldi r19, HIGH(SHORTFLASH)
		endLIndicatorBroken:
			rcall Left_Indicator_Counter
	endLIndicator:
	
ret
;******* End of Left Indicator Manager *******

;******* Start of Right Indicator Manager *******
Right_Indicator_Manager:
	
;This module is responsible for setting up and calling the right counter to ensure
;the time interval between each flash is correct.
	
	; Check if the indicator switch is pressed (indicator is on)
	bst ISREG, RSTATUS
	brtc rIndicatorOn
	rIndicatorOff:			; Do nothing
		sbr ISREG, (1<<RFLASH)
		rjmp endRIndicator
	rIndicatorOn:			
		; If right indicator is on check if working or broken
		bst ISREG, RCOND
		brtc rIndicatorBroken
		rIndicatorWorking:
			; If it is working initialise long flash
			ldi r18, LOW(LONGFLASH)
			ldi r19, HIGH(LONGFLASH)
			rjmp endRIndicatorBroken
		rIndicatorBroken:
			; If it is broken initialise short flash
			ldi r18, LOW(SHORTFLASH)
			ldi r19, HIGH(SHORTFLASH)
		endRIndicatorBroken:
			rcall Right_Indicator_Counter
	endRIndicator:

ret
;******* End of Right Indicator Manager *******

;******* Start of Left Indicator Counter *******
Left_Indicator_Counter:

;This module is responisible for incrementing and resetting the the left counter
;which determines when teh LED gets turned on and off.

		; Load counter values into register
		lds r24, LeftIndCounterL
		lds r25, LeftIndCounterH
		; r18 has been initialised from parent function
		; r19 has been initialised from parent function

		; Check if counter has reached overflow
		rcall compare16
		
		; Increment counter if overflow not reached
		brbs SREG_Z, lOverflowReached
		lOverflowNotReached:		; Increment counter
			adiw r25:r24, $01
			rjmp endLeftCounter
		lOverflowReached:			; Else reset counter
			clr r25
			clr r24
			rcall LFLASH_Toggle		; Toggle LFLASH
		endLeftCounter:
		
		; Store counter values back into memory
		sts LeftIndCounterL, r24
		sts LeftIndCounterH, r25
		
ret
;******* End of Left Indicator Counter *******

;******* Start of Right Indicator Counter *******
Right_Indicator_Counter:

;This module is responisible for incrementing and resetting the the right counter
;which determines when teh LED gets turned on and off.

		; Load counter values into registers
		lds r24, RightIndCounterL
		lds r25, RightIndCounterH
		; r18 has been initialised from parent function
		; r19 has been initialised from parent function

		; Check if counter has reached overflow
		rcall compare16
		
		; Increment counter if overflow not reached
		brbs SREG_Z, rOverflowReached
		rOverflowNotReached:		; Increment counter
			adiw r25:r24, $01
			rjmp endRightCounter
		rOverflowReached:			; Else reset counter
			clr r25
			clr r24
			rcall RFLASH_Toggle		; Toggle RFLASH
		endRightCounter:
		
		; Store counter values back into memory
		sts RightIndCounterL, r24
		sts RightIndCounterH, r25

ret
;******* End of Right Indicator Counter *******

;******* Start of Left Indicator Status Switch *******
Left_Status_Switch:

;This module polls the left switch to check whether the indicator is turned on or off.

	; Load PIND into r17
	in r17, PIND
	
	; Check if Left Indicator Switch is on / off
	bst r17, PD3
	brtc leftIndicatorOn
	leftIndicatorOff:						;If it is off, set LSTATUS in register
		sbr ISREG, (1<<LSTATUS)
		rjmp endLeftIndicatorStatusToggle
	LeftIndicatorOn:						;If it is on, clear LSTATUS in register
		cbr ISREG, (1<<LSTATUS)
	endLeftIndicatorStatusToggle:
	
ret
;******* End of Left Indicator Status Switch *******

;******* Start of Right Indicator Status Switch *******
Right_Status_Switch:

;This module polls the right switch to check whether the indicator is turned on or off.

	; Load PIND into r17
	in r17, PIND
	
	; Check if Right Indicator Switch is on / off
	bst r17, PD2
	brtc rightIndicatorOn
	rightIndicatorOff:						; If it is off, set RSTATUS in register
		sbr ISREG, (1<<RSTATUS)
		rjmp endRightIndicatorStatusToggle
	RightIndicatorOn:						; If it is on, clear RSTATUS in register
		cbr ISREG, (1<<RSTATUS)
	endRightIndicatorStatusToggle:

ret
;******* End of Right Indicator Status Switch *******

;******* Start of Left Condition Toggle *******
Left_Condition_Toggle:		
		
;This module toggles the state of the indicator between working and broken each time the left switch is
;released.		
		
		; Load data into registers
		in r17, PinD
		
		bst r17, PD4 ; Store bit 4 of PinD (Left Indicator Condition) in the TFlag

		; For the following code State refers to the bits. On the STK500 a state of 0 is ON and 1 is OFF
		
		; Check current state of switch
		brts leftCurrentState1
		leftCurrentState0:					; If switch is on, do nothing
			rjmp endLeftCurrentStateCheck
		leftCurrentState1:					; If switch is off, check previous state
			bst ISREG, LPSTATE
			brtc leftPreviousState0
			leftPreviousState1:				; If switch was off, do nothing
				rjmp endLeftPreviousStateCheck
			leftPreviousState0:				; If switch was on, toggle condition
				bst ISREG, LCOND
				brts leftWorking
				leftBroken:					; Working -> Broken	
					sbr ISREG,(1<<LCOND)
					rjmp endToggleLeftCondition
				leftWorking:				; Broken -> Working 
					cbr ISREG,(1<<LCOND)
				endToggleLeftCondition:
					ldi r18, $00
					sts LeftIndCounterH, r18
					sts LeftIndCounterL, r18
			endLeftPreviousStateCheck:
		endLeftCurrentStateCheck:
		
		; Update new switch state
		bst r17, PD4
		bld ISREG, LPSTATE

ret
;******* End of Left Condition Toggle *******

;******* Start of Right Condition Toggle *******
Right_Condition_Toggle:

;This module toggles the state of the indicator between working and broken each time the right switch is
;released.

		; Load data into registers
		in r17, PinD
		
		bst r17,PD1 ; Store bit 1 of PinD (Right Indicator Condition) in the TFlag

		; For the following code State refers to the bits. On the STK500 a state of 0 is ON and 1 is OFF
		
		; Check current state of switch
		brts rightCurrentState1
		rightCurrentState0:						; If switch is on, do nothing
			rjmp endRightCurrentStateCheck
		rightCurrentState1:						; If switch is off, check previous state
			bst ISREG, RPSTATE
			brtc rightPreviousState0
			rightPreviousState1:				; If switch was off, do nothing
				rjmp endRightPreviousStateCheck
			rightPreviousState0:				; If switch was on, toggle condition
				bst ISREG, RCOND
				brts rightWorking
				rightBroken:					; Working -> Broken	
					sbr ISREG,(1<<RCOND)
					rjmp endToggleRightCondition
				rightWorking:					; Broken -> Working 
					cbr ISREG,(1<<RCOND)
				endToggleRightCondition:
					ldi r18, $00
					sts RightIndCounterH, r18
					sts RightIndCounterL, r18
			endRightPreviousStateCheck:
		endRightCurrentStateCheck:
		
		; Update new switch state
		bst r17, PD1
		bld ISREG, RPSTATE

ret
;******* End of Right Condition Toggle *******

;******* Start of Flash Left LED *******
Left_LED_Toggle:

;This module turns the left indicator LED on and off.
	
	; Check if LED should be on / off
	bst ISREG, LFLASH
	brtc leftLEDOn
	leftLEDOff:					; turn it off
		sbi PORTB, PB3
		rjmp endLeftLEDToggle
	leftLEDOn:					; turn it on
		cbi	PORTB, PB3
	endLeftLEDToggle:

ret
;******* End of Flash Left LED *******

;******* Start of Flash Right LED *******
Right_LED_Toggle:

;This module turns the right indicator LED on and off.

	; Check if LED should be on / off
	bst ISREG, RFLASH
	brtc rightLEDOn
	rightLEDOff:					; turn it off
		sbi PORTB, PB2
		rjmp endRightLEDToggle
	rightLEDOn:						; turn it on
		cbi	PORTB, PB2
	endRightLEDToggle:

ret
;******* End of Flash Right LED *******

;******* Start of Flash Left LED *******
LFLASH_Toggle:

;This module toggles the LFLASH bit each time the counter is reset
	
	; Check if LFLASH is set / clear
	bst ISREG, LFLASH
	brtc LFLASHIsClear
	LFLASHIsSet:					; LFLASH(set -> clear)
		cbr ISREG, (1<<LFLASH)
		rjmp endLFLASHToggle
	LFLASHIsClear:					; LFLASH(clear -> set)
		sbr ISREG, (1<<LFLASH)
	endLFLASHToggle:
	
ret
;******* End of Flash Left LED *******

;******* Start of Flash Right LED *******
RFLASH_Toggle:

;This module toggles the RFLASH bit each time the counter is reset

	; Check if RFLASH is set / clear
	bst ISREG, RFLASH
	brtc RFLASHIsClear
	RFLASHIsSet:					; RFLASH(set -> clear)
		cbr ISREG, (1<<RFLASH)
		rjmp endRFLASHToggle
	RFLASHIsClear:					; RFLASH(clear -> set)
		sbr ISREG, (1<<RFLASH)
	endRFLASHToggle:
	
ret
;******* End of Flash Right LED *******

;******* Start of External Interrupt *******
Collision_Detection:   

	sbi PORTC, PC6

		

;This module reads the input from the ADC, the input is a force measurement in G's. This force
;measurement is constantly monitored and if the force measurement exceeds 4G, an assigned LED on
;the board lights up to indicate this.

		push ISREG
		push r17
		push r25
		in r16, SREG
		push r16

		clr r16
		clr r17
		clr r25
		
		sbi DDRB, PB0
		; Make sure output pin doesnt clash
		
		in r17, ADCL
		in r16, ADCH
		
		; store force in r25
		bst r16, 1
		bld r25, 3
		bst r16, 0
		bld r25, 2
		bst r17, 7
		bld r25, 1
		bst r17, 6
		bld r25, 0
		
		; check if force reading exceeds threshold
		cpi r25, thresholdForce
		brsh isGreaterThanThreshold
		isLessThanThreshold:
			sbi PORTB, PB0
			rjmp endThreshold
		isGreaterThanThreshold:
			cbi PORTB, PB0
		endThreshold:

		pop r16
		out SREG, r16
		pop r25
		pop r17
		pop ISREG
		
		cbi PORTC, PC6

		RETI			;Return from Interurpt
;******* End External Interrupt *******

;******* Start of Rounding Module *******
RoundValue:

;Compares r20 to r18. If it is great or equal, increments r20 by 1.

		cp r20, r18
		brsh isGreater
		isLess:
		rjmp endRound
		isGreater:
		inc r21
		endRound:
		
		RET			;Return from Module
;******* End Rounding Module *******

;******* Multiply Fraction *******
Mul_Frac:

;Calculates (r21*r19)/(r22). Remainder stored in r20, result in r21.
	
	push r25
	push r24

	rcall div8u ; r21/r22
	
	mul r21, r19;  Result1*r19
	mov r26, r0 ; move product1 to r26
	
	mul r20, r19; remainder1*r19
	mov r21, r0; move product2 to r21
	
	rcall div8u; (product2)/r22
	
	add r21,r26;  Result2 + Product1

	pop r24
	pop r25
	RET
;******* End Multiply Fraction *******

;******* Start of 16-bit Compare *******
compare16:	
		
		cp	r24,r18	;Compare low byte
		cpc	r25,r19	;Compare high byte
		
		RET			;Return from Module
;******* End 16-bit Compare *******