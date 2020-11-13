#	MIPS SNAKE game
#	Bitmap display options:
#	Unit Width in Pixels:	16
#	Unit Height in Pixels:	 16
#	Display Width in Pixels: 512
#	Display Height in Pixels: 512
#	Base address for display: $gp
#---------------------------------------------------------------------------
#	$s0	snakeaddress address
#	$s1	futureheadaddress
#	$s2	newFirstbody or oldheadaddress
#	$s3	bodylength
#	$s4	food.X
#	$s7	food.Y
#	$s5	(bool) going to eat
#	$s6	growup for 1; common for 0
#	$a3	input ASCII
##########################################################
.data
	outstring: .asciiz "Snake length= "
	#check_error: .asciiz	"OK\n"
	newline: .asciiz "\n"
	side: .half 32			#Give a length 32 of a bitmap
	black: .half 0x0
	growupflag: .half 1
	vertical:	.half 30
	horizontal:	.half 30
	check_error:	.half 	0
	contourcolor: .word 0xabcdef
	foodcolor: .word 0xCC0000
	snakecolor: .word 0xAAAAAAAA
	bodylength: .half 3
	startaddress: .word 0x10008000
	snakeaddress: .word 0x0	 	#snakeaddress array
	
	

	

##########################################################################	
.text
init:			
	li $t0, 0x1000819c		#Assign snake address to $t0
	sw $t0, snakeaddress		#Store snake address from $t0 to "snakeaddress"
	jal clear			#Call clear function
	lw $t0, startaddress		#Load display start address
	jal row				#Call the up row function
	jal columns			#Call the column function
	jal row				#Call the bottom row funtion
	jal snake			#Call the snake funtion
	jal food			#Call the food funtion	
	
################################################################	
Strategy:
	li $a0,60			#Decrease to increase difficulty; Increase to decrease difficulty
	li $v0,32			#Sleep for 60 milliseconds
	syscall
	lw $t0,0xffff0000 	
	blez $t0, Direction 
	lw $a3,0xffff0004
	
Direction:	
	beq $a3, 0x73,under		#Check if it is 's'
	beq $a3, 0x64,right		#Check if it is 'd'
	beq $a3, 0x61,left		#Check if it is 'a'
	beq $a3, 0x77, up		#Check if it is 'w'
	j Strategy				#Thread sleep and Key check loop
	
#################################################################	
#execution(NEW snake head address)
under:						
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address to $s2
	addi $s1,$s2,128		#New head address to $s1
	j judge			#goto judge	

right:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	addi $s1,$s2,4			#New head address
	j judge

left:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	subi $s1,$s2,4			#New head address
	j judge

up:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	subi $s1,$s2,128		#New head address
	j judge
	
##################################################################
judge:
	
	jal  check_final
	lw	$t0,($s1)		#Load color occupied by the next head address in ($s1)
	lw $t3,foodcolor		#Load foodcolor to $t3
	beq $t0,$t3,growup		#If I eat the food, I grow up
snake_move:	
	lw $t0,snakecolor		#Load snakecolour to $t0
	lw $s3,bodylength		#Load bodylength to $s3
	sw $t0,($s1)			#Colour new head
	sw $s1,($s0)			#Store new head address to "snakeaddress"
	jal snakebody_shift
	j  Strategy



final:
	li $s3,3			#Reset bodylength to 3
	sh $s3,bodylength		
	lw $a3,0x10010000
	li $a0,500			#Sleep for 0.5s when dead
	li $v0,32
	syscall
	j init			#Return to the game

#function
################################################################################	
#initialization use	function
################################################################################
#Clear the screen (initialize the bitmap)-----------------------------------------------------
clear:
	lw $t0,startaddress		#startaddress = 0x10008000
	lh $t1, side			#Load the side of the bitmap
	mul $t1,$t1,$t1			#Width Pixels * Height Pixels
	lh $t2, black
clearloop:					#Loop until every display address has the black number in it (zero)		
	sw $t2,($t0)			#Initialize all pixels to Black
	addi $t0,$t0,4			#Next pixel
	subi $t1,$t1,1			#pixels need to be initialize = pixels need to be initialize - 1
	bnez $t1,clearloop		#if (pixels need to be initialize != 0) goto clearloop
	jr $ra				#back to mStrategyn

	
#This function is used to draw both the up row and the bottom row-------------------------------
row:								
	lw $t1, contourcolor		#Load contourcolor
	lh $t2, side			#Load the side of the bitmap
drawrow:					#Row loop					
	sw $t1,($t0)			#Draw contour
	addi $t0,$t0,4			#Next pixel
	subi $t2,$t2,1			#pixels need to be draw = pixels need to be draw - 1
	bnez $t2, drawrow		#if (pixels need to be draw != 0) goto drawrow
	jr $ra				#back to mStrategyn
	
	
#Draw the columns--------------------------------------------------------------------------------	
columns:						
	lw $t0, startaddress		#Load the start address
	addi $t0,$t0,128		#Go to the next line: add 0x80
	lh $t2, side			#Load the side of the bitmap
	subi $t2,$t2,2			#Substract the up and down row pixel where x=0 and x=31
drawcolumns:				#Columns Loop
	sw $t1,($t0)			#Draw contourcolor (the countourcolor is still in $t1 after row function was called)
	addi $t0,$t0,124		#Add 31 pixels to the end of the row
	sw $t1,($t0)			#Load contourcolor (draw the column at the end of rows)
	addi $t0,$t0,4			#Next pixel (from the last pixel to the first pixel in next line)
	subi $t2,$t2,1			#pixels need to be draw = pixels need to be draw - 1
	bnez $t2, drawcolumns		#if (pixels need to be draw != 0) goto drawcolumns
	jr $ra				#back to mStrategyn
	
	
#Draw the snake-----------------------------------------------------------------------------------
snake:					
	la $s0,snakeaddress		#Load the address of "snakeaddress"
	lw $s3,bodylength		#Load bodylength
	lw $t0,snakecolor		#Load snakecolor
	lw $t1,($s0)			#Load the address of snake's head which was saved in "snakeaddress" to $t1
drawsnake:				#Snake loop
	sw $t0,($t1)			#Store snake color
	sw $t1,($s0)			#Store next bodypart address to "snakeaddress" array
	subi $t1,$t1,4			#Move $t1 to the next bodypart address (the next bodypart is on the left of the head)
	addi $s0,$s0,4			#Move to the next element of "snakeaddress" array
	subi $s3,$s3,1			#bodylength = bodylength - 1
	bnez $s3, drawsnake		#if (bodylength != 0) goto drawsnake
	jr $ra				#back to mStrategyn
	
	
#Draw the food with  a pseudorandom address----------------------------------------------------------
food:						
	li $a1,0x3c0			#$a1= 32 pixel * (32 - 2)pixel ; food will be randomly gernerate within 0x10008080 ~ (0x10008F80 - 4)
	li $v0,42			#Generate a random number to $a0 ( from 0 to value($a1) )
	syscall		
	sll $a0, $a0, 2			#Multiply $a0 with 4 to generate the address (4 bytes for each pixel)
	add $a0, $a0,0x10008000		#Put the random number on the bitmap address
	lw $t0, ($a0)			#And then check if the new address is already colored
	bnez $t0,food			#Food loop
	lw $t1,foodcolor		#Load foodcolor
	sw $t1,($a0)			#Finally draw the food
foodXY:  #Calculate food's coordinate
	subi $t0,$a0,0x10008000		#food.X = ( foodaddress - 0x10008000 ) % 0x80 /4
	div $t0,$t0,0x80
	mfhi $t0			
	mflo $s7			#food.Y = ( foodaddress - 0x10008000) / 0x80	
	div $s4,$t0,4			#$s4 = food.X
	jr $ra
########################################################################################################	
#"judge" use function
########################################################################################################
#----------------------------------------------------------------------------------------------
check_final:			
	lw	$t0,($s1)		#Load color occupied by the next head address in ($s1)
	lw $t1,contourcolor		#Load contourcolor to $t1
	lw $t2,snakecolor		#Load snakecolor to $t2
	beq $t0,$t1, final		#If I hit the countour, I lose
	beq $t0,$t2,final		#If I bit myself, I lose
	jr $ra				#back to common

#snake growup -----------------------------------------------------------------------------------	
growup:		
	
	lw $s3,bodylength		
	addi $s3,$s3,1			#Increase bodylength
	sw $s3, bodylength		#Store new bodylength
	lw $t0,snakecolor
	sw $t0,($s1)			#color new head
	lw $s2,($s0)			#Load old head address
	sw $s1,($s0)			#Store new head address to "snakeaddress"	
	li $s6,1			
	sh $s6,growupflag	
#print message-----------------------------------------
	li $v0,4
	la $a0, outstring
	syscall
	li $v0, 1
	lw $a0, bodylength
	syscall
	li $v0,4
	la $a0, newline
	syscall
#--------------------------------------------------------
	jal food				#in initialization use function
		
	j	snake_move
	
	
snakebody_shift:
	subi $s3,$s3,1			#bodylength = bodylength - 1
	addi $s0,$s0,4			#Move to the next element of "snakeaddress" array
	lw $t0,($s0)			#Load old bodypart address in "snakeaddress" array to $t0
	sw $s2,($s0)			#Store next new bodypart to "snakeaddress" array
	move $s2,$t0			#Store old bodypart from $s2 to $t0 for being new next bodypart
	bnez $s3,snakebody_shift			#if (bodylength != 0) goto shift	
	lw $t1,($s0)			#Load the tStrategyl from the end of "snakeaddress" array to $t1
	lh $t2,black			#Erase tStrategyl
	sw $t2,($t1)			
	jr $ra				#back to growup or common	
	
	
############################################################################################################


############################################################################################################
