#	MIPS SNAKE game
#	Bitmap display options:
#	Unit Width in Pixels:	16
#	Unit Height in Pixels:	 16
#	Display Width in Pixels: 512
#	Display Height in Pixels: 512
#	Base address for display: $gp
#---------------------------------------------------------------------------
#	$s0	snakeaddr address
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
	outstring: .asciiz "Snake l= "
	newline: .asciiz "\n"
	side: .half 32			#Give a length 32 of a bitmap
	black: .half 0x0
	growupflag: .half 1
	vertical:	.half 30
	horizontal:	.half 30
	contourcolor: .word 0xabcdef
	foodcolor: .word 0xCC0000
	snakecolor: .word 0xAAAAAAAA
	bodylength: .half 3
	startaddress: .word 0x10008000
	snakeaddr: .word 0x0	 	#snakeaddress array
	
	

	

###########################################################
.text
init:			
	li $t0, 0x1000819c		#Assign snake address to $t0
	sw $t0, snakeaddr		#$t0 = "snakeaddress"
	jal clear			
	lw $t0, startaddress		#$t0 = start address
	jal row				
	jal columns			
	jal row				
	jal snake			
	jal food			
	lw $t9,snakecolor
	li $t7,0			#檢查會不會自殘
	li $t6,15		#檢查會不會進入無限迴圈
	li $k0,0			#用於確認前半圈是否有吃到食物
	li $k1,0			#用於確認後半圈是否有吃到食物
################################################
FindheadXY:			#找蛇頭
	la $s0,snakeaddr
	lw $s1,($s0)
	subi $t0,$s1,0x10008000
	div $t0,$t0,0x80
	mfhi $t0
	mflo $s5                       #headY
	div $s4,$t0,4		 #headX
###############################################Security Mode 
SecurityS:
	li $a0,10			#調節速度; Increase to decrease difficulty
	li $v0,32			#Sleep for 16 ms
	syscall
	
	li $t0,1			
	beq $s5,$t0,Lp1		#若現在蛇在最上層則進入Security mode
	j Lp1		
	j upS
DirectionS:
#================================================================
Lp1:					#(貼著上邊界走)
	bne $a2,$0,Lp2		#若上邊界還沒走完，$a2==0
	slti $t0,$s4,30			#X軸最遠只能到30，判斷是否已走到底
	beq $t0,$0,Lp2		#若已走到底，則跳到loop2
	bne $k0,$0,LL1		#k0==1代表現在是可以吃食物的狀態，$k0==0則代表剛剛已吃到食物，
					#並讓蛇回到邊框，不讓蛇吃食物了	
addi $s4,$s4,1
	j rightS

LL1:	
	beq $s4,$s6,Lp2		#若現在蛇的位置和食物x軸相同，goto loop2(往下走)
	addi $s4,$s4,1			
	j rightS
#===============================================================
Lp2:					#(貼著右邊界走or為了找食物而往下走)
	li $a2,1			#代表上邊界走完，$a2==1
	bne $a3,$0,Lp3		#若右邊界還沒走完，$a3==0
	slti $t0,$s5,30			#y軸最遠只能到30，判斷是否已走到底
	beq $t0,$0,Lp3		#若已走到底，則跳到loop3
	bne $k0,$0,LL2			#k0==1代表現在是可以吃食物的狀態，$k0==0則代表剛剛已吃到食物
	addi $s5,$s5,1
	j underS
LL2:
	beq $s5,$s7,skr1		#若現在蛇的位置和食物y軸相同，則跳至skr1代表已經吃到食物
	addi $s5,$s5,1			#若沒有則繼續往下走
	j underS			
skr1:
	li $a2,0			#設$a2==0是因為剛剛因為$k0==1時，在LL1判斷hx==foodx時就先跳到loop2(向下走)，
					#而不是因為$s4已經走到30才往下走，所以要設$a2==0讓蛇把x軸走到30
	li $k0,0			#因已經吃到食物，設$k0==0
	j Lp1
#================================================================
Lp3:					#(貼著下邊界走)
	li $a3,1			#代表右邊界走完，$a3==1
	slti $t0,$s4,2			#判斷hx是否以走到下邊界的最左邊(x==1)
	bne $t0,$zero,Lp4		#若有則跳至loop4
	bne $k1,$0,LL3			#k1==1代表現在是可以吃食物的狀態，$k1==0則代表剛剛已吃到食物
	subi $s4,$s4,1
	j leftS
LL3:
	beq $s4,$s6,Lp4		#若現在蛇的位置和食物x軸相同，則跳至loop4(往上走)
	subi $s4,$s4,1
	j leftS
#==============================================================
Lp4:					#(貼著左邊界走)
	slti $t0,$s5,2			#判斷hy是否以走到左邊界的最上面(y==1)
	bne $t0,$0,check		#若有則跳至check做重設的動作
	bne $k1,$0,LL4			#k1==1代表現在是可以吃食物的狀態，$k1==0則代表剛剛已吃到食物
	subi $s5,$s5,1
	j upS
	
LL4:
	beq $s5,$s7,skr2		#若現在蛇的位置(hy)和食物(foody)相同，則跳至skr2代表已經吃到食物
	subi $s5,$s5,1
	j upS
skr2:	
	li $k1,0			#因已經吃到食物，設$k1==0則代表剛剛已吃到食物
	j Lp3				#在此要讓蛇把x軸走完(走到1)
#============================================================
check:					#(繞完一圈旗標重設)
	li $a2,0			#用於確認上邊界是否走完
	li $a3,0			#用於確認右邊界是否走完
	li $k0,1			#用於確認前半圈是否有吃到食物
	li $k1,1			#用於確認後半圈是否有吃到食物
	j DirectionS
	
#################################################################	
gettailXY:				#get tailXY
	la $s0,snakeaddr		
	lw $s3,bodylength		
	j SecurityS		#goto StrategS
	addi $s3,$s3,-1	
	sll $s3,$s3,2			#在地址中操作尾坐標：（bodylength-1）* 4 + first snakeaddr
	add $t2,$s0,$s3			
	lw $t1,($t2)			#get tail address
	subi $t0,$t1,0x10008000		#操作尾坐標
	div $t0,$t0,0x80
	mfhi $t0
	mflo $t9                         #get tailY
	div $t8,$t0,4			 #get tailX
	#==========================================
	la $s0,snakeaddr		#得到這次頭部坐標（總是會改變的）
	lw $s1,($s0)
	subi $t0,$s1,0x10008000		
	div $t0,$t0,0x80
	mfhi $t0
	mflo $t5                         #get headY
	div $t4,$t0,4			 #get headX

	#================security2====================
	
	slt $t0,$s4,$s6          		 
	bne $t0,$zero,foodXrightofsnakeX    	# hx > foodX
	beq $s4,$s6,foodXrightofsnakeX
	j foodXleftofsnakeX			#hx < foodx


foodXleftofsnakeX:
	slt $t0,$s5,$s7				
	bne $t0,$zero,headYsmallfoodY		#if hy<foody goto headYsmallfoodY(food is under than snake)
	beq $s5,$s7,foodlloop			#if hy==foody goto foodlloop (because food left of snake)
	j headYbigerfoodY			#if hy>foody goto headYbigerfoodY(food is up than snake)
foodlloop:
	beq $s4,$s6,strateEND			#if already eat food goto strateEND(reset head coordinate)
	addi $s4,$s4,-1				#goto left
	j leftS

foodXrightofsnakeX:	
	slt $t0,$s5,$s7        
	bne $t0,$zero,headYsmallfoodY    #if hy<foody goto headYsmallfoodY(food is under than snake)
	beq $s5,$s7,foodrloop		#if hy==foody goto foodlloop (because food right of snake)
	j headYbigerfoodY		#if hy>foody goto headYbigerfoodY(food is up than snake)
foodrloop:	
	beq $s4,$s6,strateEND		#if already eat food goto strateEND(reset head coordinate)
	addi $s4,$s4,1			#goto right
	j rightS
headYsmallfoodY:
	addi $s5,$s5,1			#goto under(because food is under than snake)
	j underS
	
headYbigerfoodY:
	addi $s5,$s5,-1			#goto up(because food is up than snake)
	j upS

strateEND:
	j FindheadXY			#reset head coordinate
	
#################################################################	
#execution(NEW snake head address)
underS:						
	la $s0, snakeaddr		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address to $s2
	addi $s1,$s2,128		#New head address to $s1
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,Ls1			# 若下一步會吃到自己，轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls1			# 若下一步會出界，轉向
	beq $t7,$0,judgeS		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若又則t7會為1	
	li $t7,0			#重設旗標t7
	addi $s5,$s5,1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
	j judgeS			#goto judge	
Ls1:
	addi $t6,$t6,-1
	beq $t6,$0,finalS
	bne $t7,$0,rightS
	li $t7,1
	addi $s5,$s5,-1
	j rightS
rightS:
	la $s0, snakeaddr		#加載舊頭的地址 to $s0
	lw $s2,($s0)			#加載舊頭的地址
	addi $s1,$s2,4			#New head address
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,Ls2			# 若下一步會吃到自己，轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls2			# 若下一步會出界，轉向
	beq $t7,$0,judgeS		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若又則t7會為1
	li $t7,0			#重設旗標t7
	addi $s4,$s4,1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
	j judgeS			#goto judge
Ls2:
	addi $t6,$t6,-1
	beq $t6,$0,finalS
	bne $t7,$0,upS
	li $t7,1
	addi $s4,$s4,-1
	j upS
leftS:
	la $s0, snakeaddr		#Load the head address to $s0
	lw $s2,($s0)			#加載舊頭的地址
	subi $s1,$s2,4			#新頭的地址
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,Ls3			# 若下一步會吃到自己，轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls3			# 若下一步會出界，轉向
	beq $t7,$0,judgeS		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若又則t7會為1
	li $t7,0			#重設旗標t7
	addi $s4,$s4,-1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入了死亡迴圈
	j judgeS			#goto judge
Ls3:
	addi $t6,$t6,-1
	beq $t6,$0,finalS
	bne $t7,$0,underS
	li $t7,1
	addi $s4,$s4,1
	j underS
upS:
	la $s0, snakeaddr		#加載舊頭的地址 to $s0
	lw $s2,($s0)			#加載舊頭的地址
	subi $s1,$s2,128		#新頭的地址
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,Ls4			# 若下一步會吃到自己，轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls4			# 若下一步會出界，轉向
	beq $t7,$0,judgeS		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若又則t7會為1
	li $t7,0			#重設旗標t7
	addi $s5,$s5,-1			#多走了這步，所以要將插值調整回去，讓hx,hy,foodx,foody的距離之差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
	j judgeS			#goto judge
	
Ls4:
	addi $t6,$t6,-1
	beq $t6,$0,finalS
	bne $t7,$0,leftS
	li $t7,1
	addi $s5,$s5,1
	j leftS
  ##################################################################
judgeS:
	
	jal  final_check
	lw	$t0,($s1)		#加載（$ s1）中下一個頭地址佔用的color
	lw $t3,foodcolor		#Load foodcolor to $t3
	beq $t0,$t3,gupS		#If I eat the food, I grow up
snake_moveS:	
	lw $t0,snakecolor		#Load snakecolour to $t0
	lw $s3,bodylength		#Load bodylength to $s3
	sw $t0,($s1)			#Colour new head
	sw $s1,($s0)			#Store new head address to "snakeaddr"
	jal snakebody_shiftS
	j  SecurityS



finalS:
	li $s3,3			#Reset bodylength to 3
	sh $s3,bodylength	
	li $a0,500			#Sleep for 0.5s when dead
	li $v0,10
	syscall
	j init			#Return to the game

#常駐功能#########################################################
#Clear the screen (initialize the bitmap)--------------
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

	
#This function is used to draw both the up row and the bottom row-------------
row:								
	lw $t1, contourcolor		#Load contourcolor
	lh $t2, side			#Load the side of the bitmap
drawrow:					#Row loop					
	sw $t1,($t0)			#Draw contour
	addi $t0,$t0,4			#Next pixel
	subi $t2,$t2,1			#pixels need to be draw = pixels need to be draw - 1
	bnez $t2, drawrow		#if (pixels need to be draw != 0) goto drawrow
	jr $ra				#back to mStrategyn
	
	
#Draw the columns-------------------
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
	
	
#Draw the snake-----------------
snake:					
	la $s0,snakeaddr		#Load the address of "snakeaddress"
	lw $s3,bodylength		#Load bodylength
	lw $t0,snakecolor		#Load snakecolor
	lw $t1,($s0)			#Load the address of snake's head which was saved in "snakeaddr" to $t1
drawsnake:				#Snake loop
	sw $t0,($t1)			#Store snake color
	sw $t1,($s0)			#Store next bodypart address to "snakeaddr" array
	subi $t1,$t1,4			#Move $t1 to the next bodypart address (the next bodypart is on the left of the head)
	addi $s0,$s0,4			#Move to the next element of "snakeaddress" array
	subi $s3,$s3,1			#bodylength = bodylength - 1
	bnez $s3, drawsnake		#if (bodylength != 0) goto drawsnake
	jr $ra				#back to mStrategyn
	
	
#Draw the food with  a pseudorandom address----------------
food:						
	li $a1,0x3c0			#$a1= 32 pixel * (32 - 2)pixel ; food will be randomly gernerate within 0x10008080 ~ (0x10008F80 - 4)
	li $v0,42			#Generate a random number to $a0 ( from 0 to value($a1) )
	syscall		
	sll $a0, $a0, 2			#Multiply $a0 with 4 to generate the address (4 bytes for each pixel)
	add $a0, $a0,0x10008000		#Put the random number on the bitmap address
	lw $t0, ($a0)			#And then check if the new address is already colored
	bnez $t0,food
	lw $t1,foodcolor		#Load foodcolor
	sw $t1,($a0)			#Finally draw the food
foodXY:  #Calculate food's coordinate
	subi $t0,$a0,0x10008000	#food.X = ( foodaddress - 0x10008000 ) % 0x80 /4
	div $t0,$t0,0x80
	mfhi $t0			
	mflo $s7			#food.Y = ( foodaddress - 0x10008000) / 0x80	
	div $s6,$t0,4			#$s6 = food.X
	jr $ra
#####################################################
#"judge"用的功能
#####################################################
#-------------------------------------
final_check:			
	lw	$t0,($s1)		#Load color occupied by the next head address in ($s1)
	lw $t1,contourcolor		#Load contourcolor to $t1
	lw $t2,snakecolor		#Load snakecolor to $t2
	beq $t0,$t1, finalS	#If I hit the countour, I lose
	beq $t0,$t2,finalS		#If I bit myself, I lose
	jr $ra				#back to common

#snake growup ----------------------	
gupS:	
	lw $s3,bodylength		
	addi $s3,$s3,1			#Increase bodylength
	sw $s3, bodylength		#Store new bodylength
	lw $t0,snakecolor
	sw $t0,($s1)			#color new head
	lw $s2,($s0)			#Load old head address
	sw $s1,($s0)			#Store new head address to "snakeaddress"	
#print message--------------------
	li $v0,4
	la $a0, outstring
	syscall
	li $v0, 1
	lw $a0, bodylength
	syscall
	li $v0,4
	la $a0, newline
	syscall
#-----------------------------
	jal food						
	j	snake_moveS
	
	
snakebody_shiftS:
	subi $s3,$s3,1			#bodylength = bodylength - 1
	addi $s0,$s0,4			#Move to the next element of "snakeaddress" array
	lw $t0,($s0)			#Load old bodypart address in "snakeaddress" array to $t0
	sw $s2,($s0)			#Store next new bodypart to "snakeaddress" array
	move $s2,$t0			#Store old bodypart from $s2 to $t0 for being new next bodypart
	bnez $s3,snakebody_shiftS			#if (bodylength != 0) goto shift	
	lw $t1,($s0)			#Load the tStrategyl from the end of "snakeaddress" array to $t1
	lh $t2,black			#Erase tStrategyl
	sw $t2,($t1)			
	jr $ra				#back to growup or common	

	
#########################################################################


