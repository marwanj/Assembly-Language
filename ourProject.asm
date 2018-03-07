		title "Maze Solver" ; Program title (optional)
		list p=16f84A ; Identifies device
		radix HEX ; Set default radix
		include "p16f84A.inc" ; Includes symbol definitions


COUNT1			EQU	d'12'
COUNT2			EQU d'13'
COUNT3			EQU d'14'		;COUNTERS USED IN DELAYS
COUNT_LCD		EQU	d'15'		
LCD_Adress		EQU	d'16'		

FlagAddress		EQU	d'17'		
FlagMenu		EQU	d'18'		;USED IN MENU SCREEN FOR POINTER
FlagGEN			EQU	d'19'
;Flag to determine which mode we are in:
;bit0: Menu
;bit1: Mode1
;bit2: Mode2
;bit3: Mode3

OBSTACLES		EQU	d'20' 		;OBSTACLES COUNTER
S_Counter		EQU d'21'
		
E_Counter		EQU d'23'
Mode_2_counter	EQU d'24'
TMR0_COUNT		EQU	d'25'
COUNT4			EQU d'26'  		;used as temporary register
LINE_NUMBER		EQU	d'27'		
;To know on which line we are
;bit0: 1st line
;bit1: 2nd line

COUNT5			EQU	d'28'
I_1				EQU	d'51'		;used in printer iteration function
I_2				EQU	d'52'

COUNT8			EQU	d'55'		;used in reset memory
FIRST_POS_FLAG	EQU	d'60'		;flag to know if we are at the first position of line1, or first position of line2
go_menu			EQU	d'59'		;flag to return to menu after algorithm is over






		ORG	0x00

			Goto MAIN


		ORG	0x04		

				BTFSC	INTCON ,RBIF	;enabling interrupts
				GOTO	RBIFINT
				BTFSC	INTCON, T0IF
				GOTO	TMR0INT

RBIFINT			CALL	DELAY1m			;interrupt handler
				BTFSS	PORTB,4
				GOTO	MOVE
				BTFSS	PORTB,5
				GOTO	CONFIRM
				BTFSS	PORTB,6
				GOTO	START_OL
				BTFSS	PORTB,7
				GOTO	END_OL
FOUNDIT			BCF		INTCON,0
				RETFIE



;***********************************MAIN***************************************
MAIN	BSF		STATUS,RP0					;bank0
		CLRF	TRISA						;PORTA
		MOVLW	b'11110000' ;1 is inputs
		MOVWF	TRISB
		MOVLW	b'10000111'
		MOVWF	OPTION_REG
		BCF		STATUS,RP0
		BCF		PORTB,0
		BCF		PORTB,2
		BCF		PORTB,3
		CLRF	PORTA
		MOVLW	0x20
		MOVWF	TMR0_COUNT					;INITILIAZE TIMER0 COUNT TO 32 INSTRUCTIONS
;******************************STARTING THE SCREEN*****************************

		MOVLW	B'00010' ;Function set
		CALL	LETTERS
		MOVLW	B'00010'
		CALL	LETTERS
		MOVLW	B'01000'
		CALL	LETTERS

		MOVLW	B'00000';Display on off
		CALL	LETTERS
		MOVLW	B'01100'
		CALL	LETTERS

		MOVLW	B'00000';Clear Display
		CALL	LETTERS
		MOVLW	B'00001'
		CALL	LETTERS

		MOVLW	B'00000';Entry Mode set
		CALL	LETTERS
		MOVLW	B'00110'
		CALL	LETTERS



;*******************************MAZE SOLVER*************************************
		Call	L_SPACE
		Call	L_SPACE
		Call	L_M
		Call	L_A
		Call	L_Z
		Call	L_E
		Call	L_SPACE
		Call	L_S
		Call	L_O
		Call	L_L
		Call	L_V
		Call	L_E
		Call	L_R
		Call	DELAY2s
		Call	CLEAR_DISPLAY


;*************************************MENU****************************************
Menu	BCF		go_menu,0
		MOVLW	d'5'
		MOVWF	OBSTACLES			;INITIALIZED MAX NUMBER OF OBSTACLES
		MOVLW	d'1'
		MOVWF	S_Counter			
		MOVWF	E_Counter
		MOVLW	d'0'
		MOVWF	I_1					;I_1 and I_2 to print number of iterations
		MOVLW	d'1'
		MOVwf	I_2
		Call	Default_Line		;setting cursor before Default
		Call	Menu_Flag
		Call	S_Pointer
		Call	L_D
		Call	L_E
		Call	L_F
		Call	L_A
		Call	L_U
		Call	L_L
		Call	L_T
		Call	Obstacle_Line		;setting cursor before Obstacle
		Call	L_SPACE
		Call	L_O
		Call	L_B
		Call	L_S
		Call	L_T
		Call	L_A
		Call	L_C
		Call	L_L
		Call	L_E
		Call	Maze_Line			;setting cursor before Maze
		Call	L_SPACE
		Call	L_M
		Call	L_A
		Call	L_Z
		Call	L_E
		Call	Menu_Flag
		Call	Default_Flag
		Call	INT_RB				;to enable the interrupts

infloop	BTFSC	go_menu,0			;stay in infinite loop or go back to Menu if algorithm is done
		GOTO	Menu
		Goto	infloop


;************************MODE1 START***************************
;FSR:
;first position first line: FSR d'30'
;last position first line: FSR d'39
;first position second line: FSR d'49'
;last position second line: FSR d'40'


MODE1
		MOVLW	d'30'					;setting FSR to first position on screen				
		MOVWF	FSR
		CALL	CURSOR_ON
		Call	PRINT_DASHES_TEMP		;print all the dashes at the beginning
		MOVLW	d'30'
		MOVWF	FSR
		CALL	CURSOR_right_position	;set cursor at position corresponding to the FSR
		CALL	OBS_TEMP				;insert obstacle
		MOVLW	d'35'
		MOVWF	FSR
		CALL	CURSOR_right_position
		CALL	S_TEMP					;insert Start
		MOVLW	d'40'
		MOVWF	FSR
		CALL	CURSOR_right_position
		CALL	OBS_TEMP
		INCF	FSR,F
		CALL	CURSOR_right_position
		CALL	OBS_TEMP
		MOVLW	d'45'
		MOVWF	FSR
		CALL	CURSOR_right_position
		CALL	OBS_TEMP
		INCF	FSR,F
		CALL	CURSOR_right_position
		CALL	OBS_TEMP
		INCF	FSR,F
		INCF	FSR,F
		CALL	CURSOR_right_position
		CALL	OBS_TEMP
		INCF	FSR,F
		CALL	CURSOR_right_position
		CALL	E_TEMP					;insert End
		CALL	O_ADDRESS				
		CALL	L_SPACE
		CALL	L_S
		CALL	L_SPACE
		CALL 	L_@
		CALL	R_ADDRESS
		CALL	L_SPACE
		CALL	L_SPACE
		CALL	L_S
		CALL	S_COMMA
		CALL	N_5

		RETURN
;*************************MODE1 END****************************





;************************MODE2 START***************************
MODE2

		MOVLW	d'30'
		MOVWF	FSR
		CALL	CURSOR_ON

		Call	PRINT_DASHES_TEMP
		MOVLW	d'30'
		MOVWF	FSR
		CALL	CURSOR_right_position
		CALL	S_TEMP
		MOVLW	d'40'
		MOVWF	FSR
		CALL	CURSOR_right_position
		CALL	E_TEMP
		MOVLW	d'31'					;setting cursor at position after Start
		MOVWF	FSR

		CALL	O_ADDRESS
		CALL	L_SPACE
		CALL	L_S
		CALL	L_SPACE
		CALL 	L_@
		CALL	R_ADDRESS
		CALL	L_SPACE
		CALL	L_SPACE
		CALL	L_S
		CALL	S_COMMA
		CALL	N_0
		CALL	CURSOR_right_position


		RETURN
;*************************MODE2 END****************************







;************************MODE3 START***************************

MODE3	MOVLW	d'30'
		MOVWF	FSR
		CALL	CURSOR_ON
		Call	PRINT_DASHES_TEMP
		MOVLW	d'30'
		MOVWF	FSR
		CALL	CURSOR_right_position
		BSF		INTCON, T0IE			;enable Timer0 interrupt
		call	PRINT_OBS				;print the number of remaining obstacles
		Return


;*************************MODE3 END****************************






;*******************TO ENABLE INTERRUPTS***********************
INT_RB	MOVLW	b'10001000'	
		MOVWF	INTCON
		Return




;************************Screen TOGGLE*************************
ET		BSF	PORTB,1
		NOP
		BCF	PORTB,1
		Call	DELAY40m
		Return





;***************************DELAYS*****************************
DELAY40m	MOVLW	H'07'
			MOVWF	COUNT1
			MOVLW	H'27'
			MOVWF	COUNT2
LOOP40m		NOP
			INCFSZ	COUNT1,F
			GOTO	LOOP40m
			NOP
			DECFSZ	COUNT2,F
			GOTO	LOOP40m
			RETURN



DELAY1m		MOVLW	H'07'
			MOVWF	COUNT3
LOOP1m		NOP
			INCFSZ	COUNT3,F
			GOTO	LOOP1m
			Return



DELAY2s		MOVLW		H'00'
			MOVWF		COUNT3
			MOVWF		COUNT2
			MOVLW		0X0A
			MOVWF		COUNT1
LOOP		INCFSZ		COUNT3,F
			GOTO		LOOP
			INCFSZ		COUNT2,F
			GOTO		LOOP
			DECFSZ		COUNT1,F
			GOTO		LOOP
			RETURN


TMR0INT		DECFSZ	TMR0_COUNT, F
			GOTO	RET
			CALL	Move_Mode_3
			MOVLW	0x20
			MOVWF	TMR0_COUNT
			CLRF	TMR0
RET			BCF		INTCON, 2
			RETFIE




;****************************MENU****************************

Default_Line	MOVLW	B'01000';Shift to default line set adress
				CALL	LETTERS
				MOVLW	B'00011'
				CALL	LETTERS
				Return
Obstacle_Line	MOVLW	B'01100';Shift to obstacle line set adress
				CALL	LETTERS
				MOVLW	B'00000'
				CALL	LETTERS
				Return
Maze_Line		MOVLW	B'01100';Shift to maze line set adress
				CALL	LETTERS
				MOVLW	B'01011'
				CALL	LETTERS
				Return

;SETS POINTER BEFORE REQUIRED WORD
Default_Menu	Call	Default_Line
				Call	L_SPACE
				Call	Obstacle_Flag
				Call	Obstacle_Line
				Call	S_Pointer
				RETURN
Obstacle_Menu	Call	Obstacle_Line
				Call	L_SPACE
				Call	Maze_Flag
				Call	Maze_Line
				Call	S_Pointer
				RETURN
Maze_Menu		Call	Maze_Line
				Call	L_SPACE
				Call	Default_Flag
				Call	Default_Line
				Call	S_Pointer
				RETURN



;***********************USEFUL FUNCTIONS*************************
CLEAR_DISPLAY	MOVLW	B'00000'	;Clear Display
				CALL	LETTERS
				MOVLW	B'00001'
				CALL	LETTERS
				return

CURSOR_ON		MOVLW	B'00000'	;cursor & blink on
				CALL	LETTERS
				MOVLW	B'01111'	;cursor & blink on
				CALL	LETTERS
				RETURN
CURSOR_OFF		MOVLW	B'00000'	;cursor & blink off
				CALL	LETTERS
				MOVLW	B'01100'	;cursor & blink off
				CALL	LETTERS
				RETURN


SHORT_BUZZ		BSF		PORTB,0
				CALL	DELAY40m
				BCF		PORTB,0
				RETURN

LONG_BUZZ		BSF		PORTB,0
				CALL	DELAY2s
				BCF		PORTB,0
				RETURN




;***************************FLAGS**********************************
Menu_Flag		MOVLW	b'00000000'
				MOVWF	FlagMenu
				MOVLW	b'00000001'
				MOVWF	FlagGEN
				return




;**************************MENU FLAGS******************************
Default_Flag	MOVLW	b'00000001'
				MOVWF	FlagMenu
				return
Obstacle_Flag	MOVLW	b'00000010'
				MOVWF	FlagMenu
				return
Maze_Flag		MOVLW	b'00000100'
				MOVWF	FlagMenu
				return




;****************************MODE FLAGS****************************
Default_Set_Flag	MOVLW	b'00000010'
					MOVWF	FlagGEN
					return
Obstacle_Set_Flag	MOVLW	b'00000100'
					MOVWF	FlagGEN
					return
Maze_Set_Flag		MOVLW	b'00001000'
					MOVWF	FlagGEN
					return





;**********************MOVE PUSH BUTTON*****************************
MOVE			BTFSC	FlagGEN,0
				CALL	Move_Menu
				BTFSC	FlagGEN,2
				Call	Move_Mode_2
				GOTO	FOUNDIT					;return from interrupt

Move_Menu		BTFSC	FlagMenu,0
				GOTO	Default_Menu
				BTFSC	FlagMenu,1
				GOTO	Obstacle_Menu
				BTFSC	FlagMenu,2
				GOTO	Maze_Menu
				RETURN

Move_Mode_2		MOVLW	d'49'					
				MOVWF	COUNT4
				MOVF	FSR,W
				SUBWF	COUNT4,W
				BTFSC	STATUS,2
				GOTO	ALG_START				;start algorithm if we reach last position
				MOVLW	d'39'
				MOVWF	COUNT4
				MOVF	FSR,W
				SUBWF	COUNT4,F
				BTFSC	STATUS,2
				INCF	FSR,F					
				INCF	FSR,F					;increment FSR twice to jump over the End 
				CALL	CURSOR_right_position	;moving on the screen by incrementing FSR and setting cursor to the right position

				GOTO	FOUNDIT



Move_Mode_3		MOVLW	d'49'
				MOVWF	COUNT4
				MOVF	FSR,W
				SUBWF	COUNT4,F
				BTFSC	STATUS,2
				GOTO	FSR_30					;if we reach the last position Move should take us back to first position
				INCF	FSR,F
				CALL	CURSOR_right_position
				return

FSR_30			MOVLW	d'30'					;sets the cursor at first position
				MOVWF	FSR
				CALL	CURSOR_right_position
				return


;**********************START & END PUSH BUTTONS*************************
;used only in Mode3
START_OL		BTFSC	FlagGEN,3
				CALL	Set_Start
				GOTO	FOUNDIT

END_OL			BTFSC	FlagGEN,3
				CALL	Set_End
				GOTO	FOUNDIT




;************************CONFIRM PUSH BUTTON***************************
CONFIRM			BTFSC	FlagGEN,0
				GOTO	Confirm_Menu
				BTFSC	FlagGEN,1
				GOTO	Confirm_Mode_1
				BTFSC	FlagGEN,2
				GOTO	Confirm_Mode_2
				BTFSC	FlagGEN,3
				GOTO	Confirm_Mode_3
				GOTO	FOUNDIT


Confirm_Menu	MOVLW	d'30'
				MOVWF	FSR
				MOVLW	b'00000000'
				MOVWF	FlagAddress
				Call	CLEAR_DISPLAY
				BTFSC	FlagMenu,0
				GOTO	Confirm_Default_Menu
				BTFSC	FlagMenu,1
				CALL	Confirm_Obstacle_Menu
				BTFSC	FlagMenu,2
				CALL	Confirm_Maze_Menu
				GOTO	FOUNDIT

;Confirm push button inside the Menu
Confirm_Default_Menu	BSF		PORTB,2				;turn red led on
						Call	Default_Set_Flag
						CALL	MODE1
						GOTO	FOUNDIT

Confirm_Obstacle_Menu	BSF		PORTB,3				;turn green led on
						Call	Obstacle_Set_Flag
						MOVLW	b'00001'
						MOVWF	LCD_Adress
						MOVLW	d'18'
						MOVWF	Mode_2_counter
						CALL	MODE2
						return

Confirm_Maze_Menu		BSF		PORTB,2
						BSF		PORTB,3
						Call	Maze_Set_Flag			
						CALL	MODE3
						return

;Confirm push button inside corresponding mode
Confirm_Mode_1	GOTO	ALG_START

Confirm_Mode_2	GOTO	INSERT_BLOCK
				return

Confirm_Mode_3	GOTO	INSERT_BLOCK
				return




;in Mode3, if number of obstacles is 0, wait untill user inserts Start and End to execute algorithm
INSERT_BLOCK			MOVLW	d'0'
						XORWF	OBSTACLES,0
						BTFSC	STATUS,2
						GOTO	TEMP_S_E_PRIME
						BTFSC	INDF,7
						CALL	SHORT_BUZZ				;short buzz if already obstacle
						BTFSC	INDF,7
						GOTO	FOUNDIT
						CALL	OBS_TEMP
						BTFSC	FlagGEN,2
						CALL	FIXING_FSR2				;if last position on first line, cursor will surpass the End to insert obstacle
						BTFSC	FlagGEN,2
						CALL	FIXING_FSR22			;if first position second line, we keep cursor at this position inserting the block, waiting for user to press Move to execute algorithm
						BTFSC	FlagGEN,3
						GOTO	FIXING_FSR3				;if first position second line, setting obstacle will set FSR back to first position, first line
						INCF	FSR,F
						CALL	CURSOR_right_position					
						BTFSC	FlagGEN,2
						CALL	SHORT_BUZZ				;short buzz used in Mode2 when we put an obstacle
CONTINUE_FOR_BLOCK		DECFSZ	OBSTACLES,1
						GOTO	BEFORE_FOUNDIT
						GOTO	TEMP_S_E


BEFORE_FOUNDIT			BTFSC	FlagGEN,3				;if # of obstacles not zero, print # of remaining obstacles
						call	PRINT_OBS
						GOTO	FOUNDIT					;return from interrupt

TEMP_S_E_PRIME			CALL	SHORT_BUZZ				;Mode3: short buzz if user tries to insert more than 5 obstacles
						GOTO	TEMP_S_E

TEMP_S_E				BTFSC	FlagGEN,2				
						GOTO	ALG_START				;if # obstacles = 0, start algorithm
						CALL	PRINT_OBS
						BTFSS	E_Counter,0
						GOTO	CHECK_FOR_S				;if 5 obstacles and End are set, wait for user to insert Start, then execute algorithm
						BTFSS	S_Counter,0

						GOTO	CHECK_FOR_E				;if 5 obstacles and Start are set, wait for user to insert End, then execute algorithm
						GOTO	FOUNDIT


CHECK_FOR_S				BTFSS	S_Counter,0
						GOTO	ALG_START
						GOTO	FOUNDIT


CHECK_FOR_E				BTFSS	E_Counter,0
						GOTO	ALG_START
						GOTO	FOUNDIT





FIXING_FSR2				MOVLW	d'39'				;check above for explanation
						MOVWF	COUNT4
						MOVF	FSR,W
						SUBWF	COUNT4,F
						BTFSC	STATUS,2
						INCF	FSR,F
						return


FIXING_FSR22			MOVLW	d'49'				;check above for explanation
						MOVWF	COUNT4		
						MOVF	FSR,W
						SUBWF	COUNT4,F
						BTFSC	STATUS,2
						DECF	FSR,F
						return


FIXING_FSR3				MOVLW	d'49'				;check above for explanation
						MOVWF	COUNT4
						MOVF	FSR,W
						SUBWF	COUNT4,F
						BTFSC	STATUS,2
						GOTO	STARTING_POINT
						GOTO	CONTINUE_FOR_BLOCK

STARTING_POINT			DECF	OBSTACLES,F			;if user sets obstacle at first position second line, cursor goes back to first position first line
						call	PRINT_OBS
						INCF	OBSTACLES,F
						MOVLW	d'30'
						MOVWF	FSR
						CALL	CURSOR_right_position
						DECFSZ	OBSTACLES,1			;if # obstacles is 0, execute algorithm
						GOTO	FOUNDIT
						GOTO	ALG_START









;************************S PUSH BUTTON****************************
Set_Start			BTFSS	S_Counter,0				;short buzz if user sets more than 1 Start
					CALL	SHORT_BUZZ
					BTFSS	S_Counter,0
					Return

					BTFSC	INDF,7
					Call	SHORT_BUZZ				;short buzz when inserting Start
					BTFSS	INDF,7
					GOTO 	Set_Start_Minion
					RETURN

Set_Start_Minion    BCF		S_Counter,0				;if insert Start, check for End and 5 obstacles before executing
					CALL	S_TEMP
					BCF		PORTB,2
					BTFSS	E_Counter,0
					call	CHECK_FOR_OBSTACLES
					GOTO	FIXING_FSR_S_E




CONTINUE_FOR_S_E		INCF 	FSR,F
						CALL	CURSOR_right_position
                    	GOTO	FOUNDIT

CHECK_FOR_OBSTACLES		MOVLW	d'0'				;if # of obstacles inserted is 5, execute algorithm
						XORWF	OBSTACLES,0
						BTFSC	STATUS,2
						GOTO	ALG_START
						return


FIXING_FSR_S_E			MOVLW	d'49'				;if insert S or E at first position, second line, cursor goes to first position first line
						MOVWF	COUNT4
						MOVF	FSR,W
						SUBWF	COUNT4,F
						BTFSC	STATUS,2
						GOTO	STARTING_POINT_S_E
						GOTO	CONTINUE_FOR_S_E

STARTING_POINT_S_E		MOVLW	d'30'
						MOVWF	FSR
						CALL	CURSOR_right_position
						GOTO	FOUNDIT


;***********************E PUSH BUTTON***********************
Set_End				BTFSS	E_Counter,0				;short buzz if insert more than 1 End
					CALL	SHORT_BUZZ				;code executes similarly to S push button above
					BTFSS	E_Counter,0
					RETURN
					BTFSC	INDF,7
					Call	SHORT_BUZZ
					BTFSS	INDF,7
					GOTO	Set_End_Minion
					return

Set_End_Minion		BCF		E_Counter,0
					BCF		PORTB,3
					CALL	E_TEMP
					BTFSS	S_Counter,0
					call	CHECK_FOR_OBSTACLES
					GOTO	FIXING_FSR_S_E

;SETTINGS FOR SPECIFIC ADDRESSES ON THE SCREEN
E_ADDRESS		MOVLW	B'01100'
				CALL	LETTERS
				MOVLW	B'01001'
				CALL	LETTERS
				Return

O_ADDRESS		MOVLW	B'01000'
				CALL	LETTERS
				MOVLW	B'01100'
				CALL	LETTERS
				Return

R_ADDRESS		MOVLW	B'01100'
				CALL	LETTERS
				MOVLW	B'01011'
				CALL	LETTERS
				Return

I_ADDRESS		MOVLW	B'01000'
				CALL	LETTERS
				MOVLW	B'01011'
				CALL	LETTERS
				Return

UNDER_I_ADDRESS	MOVLW	B'01100'
				CALL	LETTERS
				MOVLW	B'01011'
				CALL	LETTERS
				Return



;*****************************LETTERS***************************
LETTERS			MOVWF	PORTA
				Call	ET
				return


L_SPACE	MOVLW	B'10010';Space
		CALL	LETTERS
		MOVLW	B'10000'
		CALL	LETTERS
		Return
L_M		MOVLW	B'10100';M
		CALL	LETTERS
		MOVLW	B'11101'
		CALL	LETTERS
		Return
L_A		MOVLW	B'10100';A
		CALL	LETTERS
		MOVLW	B'10001'
		CALL	LETTERS
		Return
L_Z		MOVLW	B'10101';Z
		CALL	LETTERS
		MOVLW	B'11010'
		CALL	LETTERS
		Return
L_E		MOVLW	B'10100';E
		CALL	LETTERS
		MOVLW	B'10101'
		CALL	LETTERS
		Return
L_S		MOVLW	B'10101';S
		CALL	LETTERS
		MOVLW	B'10011'
		CALL	LETTERS
		Return
L_O		MOVLW	B'10100';O
		CALL	LETTERS
		MOVLW	B'11111'
		CALL	LETTERS
		Return
L_L		MOVLW	B'10100';L
		CALL	LETTERS
		MOVLW	B'11100'
		CALL	LETTERS
		Return
L_V		MOVLW	B'10101';V
		CALL	LETTERS
		MOVLW	B'10110'
		CALL	LETTERS
		RETURN
L_R		MOVLW	B'10101';R
		CALL	LETTERS
		MOVLW	B'10010'
		CALL	LETTERS
		Return
L_D		MOVLW	B'10100';D
		CALL	LETTERS
		MOVLW	B'10100'
		CALL	LETTERS
		Return
L_B		MOVLW	B'10100';B
		CALL	LETTERS
		MOVLW	B'10010'
		CALL	LETTERS
		Return
L_C		MOVLW	B'10100';C
		CALL	LETTERS
		MOVLW	B'10011'
		CALL	LETTERS
		Return
L_F		MOVLW	B'10100';F
		CALL	LETTERS
		MOVLW	B'10110'
		CALL	LETTERS
		Return
L_U		MOVLW	B'10101';U
		CALL	LETTERS
		MOVLW	B'10101'
		CALL	LETTERS
		Return
L_T		MOVLW	B'10101';T
		CALL	LETTERS
		MOVLW	B'10100'
		CALL	LETTERS
		Return
L_@		MOVLW	B'10100';R
		CALL	LETTERS
		MOVLW	B'10000'
		CALL	LETTERS
		Return
L_I		MOVLW	B'10100';I
		CALL	LETTERS
		MOVLW	B'11001'
		CALL	LETTERS
		Return

L_P		MOVLW	B'10101';P
		CALL	LETTERS
		MOVLW	B'10000'
		CALL	LETTERS
		Return
L_H		MOVLW	B'10100';H
		CALL	LETTERS
		MOVLW	B'11000'
		CALL	LETTERS
		Return
L_X		MOVLW	B'10101';X
		CALL	LETTERS
		MOVLW	B'11000'
		CALL	LETTERS
		Return
N_0		MOVLW	B'10011';Number 0
		CALL	LETTERS
		MOVLW	B'10000'
		CALL	LETTERS
		Return
N_5		MOVLW	B'10011';Number 5
		CALL	LETTERS
		MOVLW	B'10101'
		CALL	LETTERS
		Return
N_LINE	MOVLW	B'01100';Shift to second line set adress
		CALL	LETTERS
		MOVLW	B'00000'
		CALL	LETTERS
		Return
S_COMMA	MOVLW	B'10010';
		CALL	LETTERS
		MOVLW	B'11100'
		CALL	LETTERS
		Return
S_DASH	MOVLW	B'10101';
		CALL	LETTERS
		MOVLW	B'11111'
		CALL	LETTERS
		Return
S_BLOCK	MOVLW	B'11111';
		CALL	LETTERS
		MOVLW	B'11111'
		CALL	LETTERS
		Return
S_Pointer	MOVLW	B'10010';
		CALL	LETTERS
		MOVLW	B'11010'
		CALL	LETTERS
		Return




;*************************************ALGORITHM YO***********************************

; Let's agree on some conventions:
; dash= 00000001
; block=00000010
; S=    00000100
; E=    00001000
;Non-Dash=1xxxxxxx
;visited =01xxxxxx
; 30  31  32  33  34  35  36  37  38  39    <-FSR first line
; 49  48  47  46  45  44  43  42  41  40    <-FSR second line



;********** Algorithim ***********
;- - - - - - - - - -
;- - - - - - - - - -
;if (FSR=39 or 40 ) we need to end searching in the right
;if (FSR = 30 or 49) we need to end left searching
;if (FSR <40 we need not to search up)
;if (FSR >40 no need to seach down)
;   if (INDF,1=1) obstacle
;   if (INDF,2=1) start
;   if (INDF,3=1)We found destination

ALG_START					Call	CURSOR_OFF
							Call	ITR_FUNCTION
							BCF		INTCON,5
							CALL	FINDING_S
							CALL	CURSOR_right_position
							CALL	Terminal_state
							GOTO	The_Potato_End
we_shall_return				CALL	FINDING_S				;we_shall_return is label from The_Potato_End function below
							CALL	CURSOR_right_position
							GOTO	Go_Right


The_Potato_End						BTFSC	FIRST_POS_FLAG,1	;IF WE ARE AT THE FIRST POSITION OF SECOND LINE DON'T CHECK THE LEFT POSITION i.e. SKIP INCREMENT
									GOTO	SKIP_INC
									INCF	FSR,F
									BTFSS	INDF,7
									GOTO	we_shall_return

									DECF	FSR,F
SKIP_INC							BTFSC	FIRST_POS_FLAG,0	;IF WE ARE AT FIRST POS OF FIRST LINE DON'T CHECK LEFT AKA DONT DECREMENT
									GOTO	SKIP_DEC

									DECF	FSR,F
									BTFSS	INDF,7
									GOTO	we_shall_return
									INCF	FSR,F
SKIP_DEC							BTFSS	LINE_NUMBER,0			;if we are up we check down
									CALL	UP_FSR					;if 2nd line
									BTFSC	LINE_NUMBER,0
									CALL	DOWN_FSR				;if 1st line
									BTFSS	INDF,7
									GOTO	we_shall_return
									Call	PATHX	
									BCF		PORTB,3		;turn green led off
									BCF		PORTB,2		;turn red led off
									BSF		PORTB,2		;turn red led on
									Call	LONG_BUZZ

Potato_found						Call	CLEAR_DISPLAY
									BCF		PORTB,2		;turn red led off
									Call	CURSOR_OFF
									BSF		go_menu,0
									GOTO	FOUNDIT		;leave interrupt


									
;this function to not check left position if we already are at the first position of the line
Terminal_state		BCF		FIRST_POS_FLAG,0
					BCF		FIRST_POS_FLAG,1
					MOVLW	d'30'
					XORWF	FSR,0
					BTFSC	STATUS,2
					BSF		FIRST_POS_FLAG,0

					MOVLW	d'49'
					XORWF	FSR,0
					BTFSC	STATUS,2
					BSF		FIRST_POS_FLAG,1
					return

;**************************************RIGHT CHECK************************************
;LINE_NUMBER flag gives us the line we are at
;---------1st line, bit0=1
;---------2nd line, bit1=1

Go_Right			CALL	CURSOR_right_position
					BTFSS	LINE_NUMBER,1				;if we are up, we do the next instruction
					GOTO	Go_Right_first_line			;if 1st line
					GOTO	Go_Right_second_line		;if 2st line

;IF WE ARE ON THE FIRST LINE
Go_Right_first_line			CALL	CURSOR_right_position
							MOVLW	d'39'
							XORWF	FSR,0
							BTFSC	STATUS,2
							GOTO	Go_Left			;We're at the last position of the first line
							INCF	FSR,F
							BTFSC	INDF,3
							GOTO	Found_End
							BTFSC	INDF,7									;There's an OBSTACLE
							GOTO	R_Obstacle_after_increment				;There's an obtacle
							BTFSC	INDF,6									; Already visisted
							GOTO	R_Obstacle_after_increment				;We already visisted this!
							Call	Function_for_no_repition
							GOTO	Go_Right

R_Obstacle_after_increment	DECF	FSR,F		;we incremented the FSR above to check right position, here decrement to go back to original
							GOTO	Go_Left


;IF WE ARE ON THE SECOND LINE
Go_Right_second_line		CALL	CURSOR_right_position 
							MOVLW	d'40'
							XORWF	FSR,0
							BTFSC	STATUS,2
							GOTO	Go_Left			;We're at the last position of the second line
							DECF	FSR,F
							BTFSC	INDF,3
							GOTO	Found_End
							BTFSC	INDF,7									;There's an OBSTACLE
							GOTO	R_Obstacle_after_decrement				;There's an obtacle
							BTFSC	INDF,6									; Already visisted
							GOTO	R_Obstacle_after_decrement				;We already visisted this!
							Call	Function_for_no_repition
							GOTO	Go_Right

R_Obstacle_after_decrement		INCF	FSR,F		;we decremented the FSR above to check right position, here increment to go back to original
								GOTO	Go_Left





;**************************************LEFT CHECK************************************
;SIMILAR CODE TO RIGHT CHECK ABOVE

Go_Left						CALL	CURSOR_right_position
							BTFSS	LINE_NUMBER,1				;if we are up, we do the next instruction
							GOTO	Go_Left_first_line			;if 1st line
							GOTO	Go_Left_second_line;		if 2st line


;WE ARE ON THE FIRST LINE
Go_Left_first_line								CALL	CURSOR_right_position
												MOVLW	d'30'
												XORWF	FSR,0
												BTFSC	STATUS,2
												GOTO	Go_Up					;We're at the last position of the first line
												DECF	FSR,F
												BTFSC	INDF,3
												GOTO	Found_End
												BTFSC	INDF,7								;There's an OBSTACLE
												GOTO	L_Obstacle_after_decrement			; There's an obtacle
												BTFSC	INDF,6								; Already visisted
												GOTO	L_Obstacle_after_decrement			;We already visisted this!
												Call	Function_for_no_repition
												GOTO	Go_Left	

L_Obstacle_after_decrement						INCF	FSR,F
												GOTO	Go_Up


;WE ARE ON THE SECOND LINE
Go_Left_second_line								CALL	CURSOR_right_position 
												MOVLW	d'49'
												XORWF	FSR,0
												BTFSC	STATUS,2
												GOTO	Go_Up				
												INCF	FSR,F
												BTFSC	INDF,3
												GOTO	Found_End
												BTFSC	INDF,7								;There's an OBSTACLE
												GOTO	L_Obstacle_after_increment			; There's an obtacle
												BTFSC	INDF,6								; Already visisted
												GOTO	L_Obstacle_after_increment			;We already visisted this!
												Call	Function_for_no_repition
												GOTO	Go_Left

L_Obstacle_after_increment						DECF	FSR,F
												GOTO	Go_Up

Function_for_no_repition						MOVLW	b'01000000'							;to save space
												MOVWF	INDF								;inserting into INDF the corresponding bits
												CALL	CURSOR_right_position
												CALL	S_Pointer							;inserting pointer for visited
												CALL	CURSOR_right_position
												Return





;*************************************Up CHECK************************************
Go_Up						CALL	CURSOR_right_position
							BTFSS	LINE_NUMBER,0				;if we are up, we do the next instruction
							GOTO	Go_Up_Sudo					;if 2nd line
							GOTO	Go_Down						;if 1st line

Go_Up_Sudo					Call	UP_FSR
							Call	CURSOR_right_position
							BTFSC	INDF,3
							GOTO	Found_End
							BTFSC	INDF,7						;There's an OBSTACLE
							GOTO	U_Obstacle					; There's an obtacle
							BTFSC	INDF,6						; Already visisted
							GOTO	U_Obstacle					;We already visisted this!
							Call	Function_for_no_repition
							GOTO	Go_Right

U_Obstacle		Call	DOWN_FSR
				Call	CURSOR_right_position
				GOTO	INSERT_BLOCK_ALG





;**************************************Down CHECK*************************************
Go_Down						Call	DOWN_FSR
							Call	CURSOR_right_position
							BTFSC	INDF,3
							GOTO	Found_End
							BTFSC	INDF,7					;There's an OBSTACLE
							GOTO	D_Obstacle				; There's an obtacle
							BTFSC	INDF,6					; Already visisted
							GOTO	D_Obstacle				;We already visisted this!
							Call	Function_for_no_repition
							GOTO	Go_Right

D_Obstacle					Call	UP_FSR					;when we went down, we saw obstacle, so go up and insert block
							Call	CURSOR_right_position
							GOTO	INSERT_BLOCK_ALG


;*******************************FOUND END**************************************************
;When we find the End position, the algorithm is over
Found_End				
						BCF		PORTB,2		;turn red led off
						BSF		PORTB,3		;turn green led on
						GOTO	PATH_star
PATH_star_return		Call	DELAY2s
						Call	DELAY2s
						BCF		PORTB,3		;turn green led off
						GOTO	Potato_found





;*************************NEEDED FUNCTIONS FOR ALGORITHMS************************
RESET_MEMORY			MOVLW	d'20'				;loop over the 20 positions and delete pointers from visited locations
						MOVWF	COUNT8
						MOVLW	d'30'
						MOVWF	FSR
TEMP_MEMORY				BTFSC	INDF,6
						call	REMOVE_POINTER
						INCF	FSR,F
						DECFSZ	COUNT8,F
						GOTO	TEMP_MEMORY
						return
REMOVE_POINTER			MOVLW	b'00000001'
						MOVWF	INDF
						call	CURSOR_right_position
						call	S_DASH
						return


DOWN_FSR				MOVF	FSR,w				;permit us to set position at the Down position (relative to current position)
						MOVWF	COUNT4				; we did not call current position we just changed fsr
						MOVLW	d'30'
						SUBWF	COUNT4,F
						MOVLW	d'49'
						MOVWF	COUNT5
						MOVF	COUNT4,W
						SUBWF	COUNT5,W
						MOVWF	FSR
						return

UP_FSR					MOVLW	d'49'				; same goes for up same as down
						MOVWF	COUNT4
						MOVF	FSR,W
						SUBWF	COUNT4,F
						MOVF	COUNT4,W
						ADDLW	d'30'
						MOVWF	FSR
						return

INSERT_BLOCK_ALG		MOVLW	b'10000010'			;insert block in algorithm and updating value of INDF( corresponsing flags )
						MOVWF	INDF
						CALL	S_BLOCK
						call	RESET_MEMORY	
						GOTO	ALG_START



				
FINDING_S				MOVLW	d'30'				;Finding position of S by getting its correct FSR
						MOVWF	FSR
TEMP_CHECK				BTFSC	INDF,2
						return
						INCF	FSR,F
						GOTO	TEMP_CHECK
						return



CURSOR_right_position	MOVF	FSR,0		;setting cursor to the current position based on the FSR
						MOVWF	COUNT4		;;count4 has the value of the FSR
						MOVLW	d'40'
						SUBWF	COUNT4,1
						BTFSC	STATUS,0
						GOTO 	SECOND_LINE
						GOTO 	FIRST_LINE
FIRST_LINE				MOVLW	b'000000001'
						MOVWF	LINE_NUMBER
						MOVF	FSR,W
						MOVWF	COUNT4
						MOVLW	d'30'
						SUBWF	COUNT4,1
						MOVLW	B'01000';
						Call	LETTERS
						MOVF	COUNT4,W
						Call	LETTERS
						return
SECOND_LINE			    MOVLW	b'000000010'
						MOVWF	LINE_NUMBER
						MOVLW	d'49'
						MOVWF	COUNT4
						MOVF	FSR,W
						SUBWF	COUNT4,F
						MOVLW	B'01100';
						Call	LETTERS
						MOVF	COUNT4,W
						Call	LETTERS
						return


PRINT_DASHES_TEMP		CALL	CURSOR_right_position		;loop over 20 locations, sets dashes, updates values of flag in INDF
						MOVLW	b'00000001'
						MOVWF	INDF
						CALL	S_DASH
						MOVLW	d'49'
						MOVWF	COUNT4
						MOVF	FSR,W
						SUBWF	COUNT4,F
						BTFSC	STATUS,2
						return
						INCF	FSR,F
						GOTO	PRINT_DASHES_TEMP
						return

S_TEMP					MOVLW	b'10000100'					;setting Start and corresponsing flag in INDF
						MOVWF	INDF
						CALL	L_S
						return

E_TEMP					MOVLW	b'10001000'				;setting END and corresponsing flag in INDF
						MOVWF	INDF
						CALL	L_E
						return


OBS_TEMP				MOVLW	b'10000010'					;	;setting obstacle and corresponsing flag in INDF
						MOVWF	INDF
						CAll	CURSOR_right_position				
						CALL	S_BLOCK
						return



;Print number of remaining obstacles
PRINT_OBS			call	O_ADDRESS
					CALL	L_O
					call	L_B
					CALL	L_S
					CALL	R_ADDRESS
					CALL	L_R
					CALL	L_E
					CALL	L_M
					CALL	PRINT_NUMBER
					CALL	CURSOR_right_position
					return


PRINT_NUMBER		MOVLW	B'10011'
					CALL	LETTERS
					MOVF	OBSTACLES,W
					XORLW	b'10000'
					CALL	LETTERS
					Return

;Print number of iterations
ITR_FUNCTION		CALL	I_ADDRESS
					CALL	L_I
					CALL	L_T
					CALL	L_R
					CALL	PRINT_FIRST_NUMBER
					CALL	PRINT_SECOND_NUMBER
					CALL	UNDER_I_ADDRESS
					CALL	L_SPACE
					CALL	L_SPACE
					CALL	L_SPACE
					CALL	L_SPACE
					CALL	L_SPACE
					INCF	I_2,F
					MOVLW	d'10'
					SUBWF	I_2,w
					BTFSC	STATUS,2
					CALL	FIX_THEM
					return


PRINT_FIRST_NUMBER		MOVLW	B'10011';
						CALL	LETTERS
						MOVF	I_1,w
						XORLW	b'10000'
						CALL	LETTERS
						Return

PRINT_SECOND_NUMBER		MOVLW	B'10011';
						CALL	LETTERS
						MOVF	I_2,w
						XORLW	b'10000'
						CALL	LETTERS
						Return


FIX_THEM		MOVLW	d'0'
				MOVWF	I_2
				INCF	I_1,F
				return


;print PATH* if found, PATHX if not found
PATH			CALL	UNDER_I_ADDRESS
				CALL	L_P
				CALL	L_A
				CALL	L_T
				CALL	L_H
				return

PATH_star		CALL	PATH
				CALL	S_Pointer
				GOTO	PATH_star_return

PATHX			CALL	PATH
				CALL	L_X
				return


					END
