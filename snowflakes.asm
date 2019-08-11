;
; NOTE TO COMPILER -> ~ = newline
;


; settings
	.inesprg 1   ; 1x 16KB PRG code
	.ineschr 1   ; 1x  8KB CHR data
	.inesmap 0   ; mapper 0 = NROM, no bank swapping
	.inesmir 1   ; background mirroring

; defines:
d_DefinesBegin:

; snowflake
FDS_X_COORDINATES		= $0000	; f_DrawSnowflake x 		 parameter
FDS_Y_COORDINATES		= $0001 ; f_DrawSnowflake y 		 parameter
FDS_INDEX				= $0002 ; f_DrawSnowflake index 	 parameter
FDS_ATTR				= $0003 ; f_DrawSnowflake attributes parameter

; hero 
FDH_X_COORDINATES		= $0004	; f_DrawHero x 		    	parameter
FDH_Y_COORDINATES		= $0005 ; f_DrawHero y 		    	parameter
FDH_ATTR				= $0006 ; f_DrawHero attributes		parameter

; buffers aka additional registers
BUFFER_A				= $0007	; buffer for various operations 
BUFFER_B				= $0008	; buffer for various operations 
BUFFER_C				= $0009	; buffer for various operations 
BUFFER_D				= $000A	; buffer for various operations 

; global counter
GLOBAL_COUNTER			= $000B ; used as seed

; input
INPUT					= $0100
INPUT_A					= $0100
INPUT_B					= $0101
INPUT_START				= $0102
INPUT_RESET				= $0103
INPUT_U					= $0104
INPUT_D					= $0105
INPUT_L					= $0106
INPUT_R					= $0107

HERO_X 					= $0203
HERO_Y 					= $0200
HERO_ATTR 				= $0202
HERO_TILE 				= $0201

UL_SNOWFLAKE_X 			= $0207
UL_SNOWFLAKE_Y 			= $0204
UL_SNOWFLAKE_ATTR 		= $0206
UL_SNOWFLAKE_TILE 		= $0205

UR_SNOWFLAKE_X 			= $020B
UR_SNOWFLAKE_Y 			= $0208
UR_SNOWFLAKE_ATTR 		= $020A
UR_SNOWFLAKE_TILE 		= $0209

BL_SNOWFLAKE_X 			= $020F
BL_SNOWFLAKE_Y 			= $020C
BL_SNOWFLAKE_ATTR 		= $020E
BL_SNOWFLAKE_TILE 		= $020D

BR_SNOWFLAKE_X 			= $0213
BR_SNOWFLAKE_Y 			= $0210
BR_SNOWFLAKE_ATTR 		= $0212
BR_SNOWFLAKE_TILE 		= $0211

d_DefinesEnd:

	.bank 0
	.org $C000

; run once at startup code
	Init:
		sei        	; disable IRQs
		cld        	; disable decimal mode
		
		ldx #$40 ~ stx $4017    ; disable APU frame IRQ
		
		ldx #$FF	
		txs          ; Set up stack
		inx          ; now X = 0
		stx $2000    ; disable NMI
		stx $2001    ; disable rendering
		stx $4010    ; disable DMC IRQs
		
		VblankWait1:
			BIT $2002
			BPL VblankWait1
		EndVblankWait1:
		
		VblankWait2:
			BIT $2002
			BPL VblankWait2
		EndVblankWait2:
		
		LoadPalettes:
			LDA $2002                     ; read PPU status to reset the high/low latch to high
			
			LDA #$3F
			STA $2006                     ; write the high byte of $3F10 address
			
			LDA #$00
			STA $2006                     ; write the low byte of $3F10 address
			
			; Load the palette data
			LDX #$00
			LoadPalettesLoop:
				lda PaletteData, x            ; load data from address (PaletteData + value in x)
				sta $2007                     ; write to PPU
				inx                           ; (inc X)
				cpx #$20                      ; Compare X to $20 (decimal 32)
				bne LoadPalettesLoop          ; (when (not= x 32) (recur))
			EndLoadPalettesLoop:
		EndLoadPalettes:
		
		lda #%10000000 ~ sta $2000 ; enable NMI, sprites from pattern table 0
		lda #%00010000 ~ sta $2001 ; turn sprites on
	
		; initialize variables and stuff here:
		DataInit:
			; hero 
			lda #$80 ~ sta FDH_X_COORDINATES	; hero x coordinates (middle)
			lda #$D0 ~ sta FDH_Y_COORDINATES	; hero y coordinates (botton)
			lda #$0  ~ sta FDH_ATTR				; hero attributes 	 (use later for different colours)
					 ~ sta GLOBAL_COUNTER		; lda #$0!			 (used as seed) 
			
			; snowflakes
			SnowflakesBeginPositions:
				lda #$10 ~ sta UL_SNOWFLAKE_X ; first snowflake x-coordinates
				lda #$10 ~ sta UL_SNOWFLAKE_Y ; first snowflake y-coordinates
				
				ldx #$0
				SnowflakesBeginPositionsLoop:
					txa ~ asl a ~ asl a ~ asl a ~ asl a ~ tay ; current offset
					
					lda UL_SNOWFLAKE_X, y ~ adc #$25 ~ pha ; set x position
					tya ~ ora GLOBAL_COUNTER ~ jsr f_GenerateRandomByte ~ and #%00111111 ~ pha ; set y position
					
					inx ~ txa ~ asl a ~ asl a ~ asl a ~ asl a ~ tay ; next offset
					pla ~ sta UL_SNOWFLAKE_Y, y
					pla ~ sta UL_SNOWFLAKE_X, y
					cpx #$8 ~ beq EndSnowflakesBeginPositions ; exit loop
				jmp SnowflakesBeginPositionsLoop
			EndSnowflakesBeginPositions:
		EndDataInit:
	EndInit:

; infinite loop
	WaitForInterupt:
		jmp WaitForInterupt
	EndWaitForInterupt:

; run 60 times/second render code
	RenderFrame:
		;transfer RAM to PPU
		lda #$00 ~ sta $2003           ; set the low byte  (00) of the RAM address
		lda #$02 ~ sta $4014           ; set the high byte (02) of the RAM address
		
		RenderFrameCode:
			inc GLOBAL_COUNTER ; global_counter++
			
			; get user input
			Input:
				jsr f_GetInput
				
				lda INPUT_R ~ cmp #$41 ~ beq InputIsRight
				lda INPUT_L ~ cmp #$41 ~ beq InputIsLeft
				lda INPUT_D ~ cmp #$41 ~ beq InputIsDown
				lda INPUT_U ~ cmp #$41 ~ beq InputIsUp
				
				jmp EndInput
				
				InputIsRight:		
					inc FDH_X_COORDINATES ~ inc FDH_X_COORDINATES	; double speed
					jmp EndInput
				InputIsLeft:
					dec FDH_X_COORDINATES ~ dec FDH_X_COORDINATES	; double speed
					jmp EndInput
				InputIsDown:		
					inc FDH_Y_COORDINATES ~ inc FDH_Y_COORDINATES	; double speed
					jmp EndInput
				InputIsUp:
					dec FDH_Y_COORDINATES ~ dec FDH_Y_COORDINATES	; double speed
					jmp EndInput
			EndInput:
			
			lda #$0 ~ sta FDH_ATTR ; hero normal colour
			
			DrawSnowflakes:
				ldx #$0							;amount of snowflakes to draw aka counter
			
				SnowFlakeCalculations:
					
					txa ~ asl a ~ asl a ~ asl a ~ asl a ~ tay; y-reg = counter * 16
					lda UL_SNOWFLAKE_Y, y ~ cmp #$80 ~ bpl EndSnowFlakeCalculations
					lda GLOBAL_COUNTER ~ jsr f_GenerateRandomByte ; random() with global_counter as seed
					
					cmp UL_SNOWFLAKE_X 		; random - GLOBAL_COUNTER
					bmi MoveSnowFlakeLeft	; if random <  GLOBAL_COUNTER -> move left
					MoveSnowFlakeRight:		; if random >= GLOBAL_COUNTER -> move right
						lda UL_SNOWFLAKE_X, y ~ adc #$1 ~ sta UL_SNOWFLAKE_X, y
					jmp EndSnowFlakeCalculations
					MoveSnowFlakeLeft:		; if random <  GLOBAL_COUNTER -> move left
						lda UL_SNOWFLAKE_X, y ~ sbc #$1 ~ sta UL_SNOWFLAKE_X, y
					jmp EndSnowFlakeCalculations
				EndSnowFlakeCalculations: 
				
				lda UL_SNOWFLAKE_Y, y ~ adc #$1 ~ sta UL_SNOWFLAKE_Y, y ; movement down
				jsr f_DrawSnowflake ; render snowflake
				
				inx ~ cpx #$1
				beq CounterIsZero
				CounterIsNotZero: ; counter is not zero yet -> continue loop
					lda GLOBAL_COUNTER ~ adc #$1 ~ sta GLOBAL_COUNTER
				jmp SnowFlakeCalculations
				CounterIsZero: ; counter is zero -> end loop
					lda GLOBAL_COUNTER ~ adc #$2 ~ sta GLOBAL_COUNTER
				jmp EndDrawSnowflakes
			EndDrawSnowflakes:
			
			; draw hero
			
			jsr f_DrawHero
			
			ReturnRenderFrame: rti	; returns from interupt, every render must end with this call			
		EndRenderFrameCode:	
	EndRenderFrame:
	
; functions

	; FUNCTION f_DrawHero
	; goal : renders a snowflake.
	;		 IN: 	FDH_X_COORDINATES 			(x-coordinates)
	;		 IN: 	FDH_Y_COORDINATES 			(y-coordinates)
	;		 IN: 	FDH_HERO_ATTR	   			(attributes of snowflake)
	;
	;		 DESTROYS: a-reg
	f_DrawHero:
		lda FDH_X_COORDINATES ~ sta HERO_X
		lda FDH_Y_COORDINATES ~ sta HERO_Y
		lda #$01		  	  ~ sta HERO_TILE
		lda FDH_ATTR		  ~ sta HERO_ATTR
	rts
	
	; FUNCTION f_DrawSnowflake_
	; goal : renders a snowflake.
	;		 IN: 	FDS_X_COORDINATES 			(x-coordinates)
	;		 IN: 	FDS_Y_COORDINATES 			(y-coordinates)
	;		 IN: 	FDS_SNOWFLAKE_INDEX	   		(# of snowflake)
	;		 IN: 	FDS_SNOWFLAKE_ATTR	   		(attributes of snowflake)
	;
	;		 DESTROYS: y-reg & a-reg
	f_DrawSnowflake_:
		lda FDS_INDEX	; backup
		
		asl FDS_INDEX	; shift 4 times left  -  COUNTER <- (0000)
		asl FDS_INDEX	; shift 4 times left  -  COUNTER <- (0000)
		asl FDS_INDEX	; shift 4 times left  -  COUNTER <- (0000)
		asl FDS_INDEX	; shift 4 times left  -  COUNTER <- (0000)
		ldy FDS_INDEX
		
		sta FDS_INDEX
		
		lda FDS_X_COORDINATES
		
		sta UL_SNOWFLAKE_X, y		; same x coordinate for upper left and bottom left
		sta BL_SNOWFLAKE_X, y		; same x coordinate for upper left and bottom left
		
		; pulls y-coordinates
		lda FDS_Y_COORDINATES
		
		sta UL_SNOWFLAKE_Y, y		; same y coordinate for upper left and upper right
		sta UR_SNOWFLAKE_Y, y		; same y coordinate for upper left and upper right
		
		; upper right
		lda UL_SNOWFLAKE_X, y
		
		adc #$7
		sta UR_SNOWFLAKE_X, y		; same x coordinate for upper right and bottom right
		sta BR_SNOWFLAKE_X, y		; same x coordinate for upper right and bottom right
		
		; bottom left	
		lda UR_SNOWFLAKE_Y, y
			
		adc #$8 	
		sta BL_SNOWFLAKE_Y, y		; same y coordinate for bottom left and bottom right
		sta BR_SNOWFLAKE_Y, y		; same y coordinate for bottom left and bottom right
		
		; tile num
		lda #$00
		sta UL_SNOWFLAKE_TILE, y
		sta UR_SNOWFLAKE_TILE, y
		sta BL_SNOWFLAKE_TILE, y
		sta BR_SNOWFLAKE_TILE, y
		
		; attributes
		; or to combine mirroring constants & passed attributes
		lda #%00000000 ~ ora FDS_ATTR ~ sta UL_SNOWFLAKE_ATTR, y
		lda #%01000000 ~ ora FDS_ATTR ~ sta UR_SNOWFLAKE_ATTR, y
		lda #%10000000 ~ ora FDS_ATTR ~ sta BL_SNOWFLAKE_ATTR, y
		lda #%11000000 ~ ora FDS_ATTR ~ sta BR_SNOWFLAKE_ATTR, y
	rts
	
	; FUNCTION f_DrawSnowflake
	; goal : renders a snowflake.
	;		 IN: 	UL_SNOWFLAKE_X, y 			(x-coordinates)
	;		 IN: 	UL_SNOWFLAKE_Y, y 			(y-coordinates)
	;		 IN: 	y-reg	   				    (# of snowflake * 16)
	;
	;		 DESTROYS: a-reg
	f_DrawSnowflake:
		lda UL_SNOWFLAKE_X, y
		sta BL_SNOWFLAKE_X, y		; same x coordinate for upper left and bottom left

		lda UL_SNOWFLAKE_Y, y
		sta UR_SNOWFLAKE_Y, y		; same y coordinate for upper left and upper right
		
		; upper right
		lda UL_SNOWFLAKE_X, y
		
		adc #$7
		sta UR_SNOWFLAKE_X, y		; same x coordinate for upper right and bottom right
		sta BR_SNOWFLAKE_X, y		; same x coordinate for upper right and bottom right
		
		; bottom left	
		lda UR_SNOWFLAKE_Y, y
			
		adc #$8 	
		sta BL_SNOWFLAKE_Y, y		; same y coordinate for bottom left and bottom right
		sta BR_SNOWFLAKE_Y, y		; same y coordinate for bottom left and bottom right
		
		; tile num
		lda #$00
		sta UL_SNOWFLAKE_TILE, y
		sta UR_SNOWFLAKE_TILE, y
		sta BL_SNOWFLAKE_TILE, y
		sta BR_SNOWFLAKE_TILE, y
		
		; attributes
		lda #%00000000 ~ sta UL_SNOWFLAKE_ATTR, y
		lda #%01000000 ~ sta UR_SNOWFLAKE_ATTR, y
		lda #%10000000 ~ sta BL_SNOWFLAKE_ATTR, y
		lda #%11000000 ~ sta BR_SNOWFLAKE_ATTR, y
		
		
		CheckCollision:
			; check x coordinates of hero & snowflake
			lda FDH_X_COORDINATES ~ adc #$08 ~ cmp UL_SNOWFLAKE_X, y
			cmp #$f8 ~ bmi IsNotColliding ~ beq IsNotColliding
			
			~ sbc #$18 ~ cmp UL_SNOWFLAKE_X, y
			cmp #$10 ~ bpl IsNotColliding ~ beq IsNotColliding
			IsColliding: 	lda #%00000001 ~ sta FDH_ATTR ~ jmp EndCheckCollison
			IsNotColliding: lda #%00000000 ~ sta FDH_ATTR
		EndCheckCollison:
	rts
	
	; FUNCTION f_GenerateRandomByte
	; goal : generates a random number from 0 to 255 from seed.
	;		 IN: 	A_reg (seed)
	;		 OUT: 	A_reg (random byte)
	;
	;		 DESTROYS: a-reg
	f_GenerateRandomByte:
        beq doEor ; if seed == 0
        asl a	   ; shift left
        beq noEor ; if the input was $80, skip the EOR
        bcc noEor
		doEor:  	eor #$1d
		noEor:
	rts
				
	; FUNCTION f_GetInput
	; goal : Reads controller input.
	; 		 OUT: 	INPUT, INPUT_A, ..., INPUT_R (values of each button)
	; 
	;		 DESTROYS: y-reg & a-reg
	;
	; note : might be ineffective
	; example usage : lda INPUT_R -> cmp #$41 -> beq input_right
	f_GetInput:
		lda #$1 ~ sta $4016
		lda #$0 ~ sta $4016
		ldx #$0							; counter
		
		ReadLoop:
			lda $4016 ~ sta INPUT, x	; read button x
			inx 						; counter++
			cpx #$8						; if counter == 8, we have read all buttons
			bne ReadLoop				; if not loop again
		EndReadLoop:
	rts

; colours
	.bank 1
	.org $E000
	
	PaletteData:
		.db $0F,$35,$36,$37, $0F,$35,$36,$37, $0F,$39,$3A,$3B, $0F,$3D,$3E,$0F  ; background palette data
		.db $0F,$30,$14,$14, $30,$30,$30,$30, $0F,$1C,$15,$14, $0F,$02,$38,$3C  ; sprite palette data
	
; interupt handlers
	.org $FFFA
	.dw RenderFrame ; jump to RenderFrame on nmi interupt
	.dw Init     	; jump to Init on starutp
	
; imports
	.bank 2
	.org $0000
	.incbin "snowflake.chr"
	
;backup
;
;
;
; randomize a-reg
;lda GLOBAL_COUNTER ~ jsr f_GenerateRandomByte
;and #%00000011 ; randomize only last 2 bits
;
;; make desisions based on random.
;IfRandom:
;	; a-reg == 1?
;	cmp #%00000001
;	beq AccamulatorEqualsOne
;	
;	AccamulatorNotEqualsOne: ; a != 1
;		dec FDS_X_COORDINATES
;		dec FDS_X_COORDINATES
;	jmp EndIfRandom
;	
;	AccamulatorEqualsOne: ; a = 1
;		inc FDS_X_COORDINATES
;		inc FDS_X_COORDINATES	
;	jmp EndIfRandom
;EndIfRandom:
				