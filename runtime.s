#Thanks to https://github.com/datkin/compilers/blob/master/runtime.s
# for providing clean mips assembly code, because
# modern cross-compiled code won't work on spim
# (and qemu was not behaving well)

#### MAIN ####
.data
.align 2
Runtconsts:
.space 2048
Runtempty: .word 0

.text
main:
li $a0,0
la $a1,Runtconsts
li $a2,1
Lrunt20:
sw $a2,($a1)
sb $a0,4($a1)
addiu $a1,$a1,8
addiu $a0,$a0,1
slt $a3,$a0,256
bnez $a3,Lrunt20
li $a0,0
j _tigermain_0


#### INIT ARRAY ####
.text
_allocArray:
sll $a0,$a0,2
li $v0,9
syscall             #alloc size*4 bytes
move $a2,$v0        #store pointer in a2
b Lrunt2
Lrunt1:             # Initialize next element 
sw $a1,($a2)        #store init in array element
sub $a0,$a0,4       #remove from count
add $a2,$a2,4       #move pointer
Lrunt2:
bgtz $a0, Lrunt1    #if initialization didnt end go to Lrunt1
j $ra


#### ALLOC RECORD ####
.text
_allocRecord:
li $v0,9
syscall
move $a2,$v0
b Lrunt4
Lrunt3:
sw $0,($a2)
sub $a0,$a0,4
add $a2,$a2,4
Lrunt4:
bgtz $a0, Lrunt3
j $ra


#### CHECK INDEX ####
.data
.align 2
CIstr: .asciiz "negative index not allowed\n"

.text
_checkIndex:
bltz $a1,CIbad #only checks for < 0
j $ra
CIbad:
li $v0,4      #printstr
la $a0,CIstr
syscall
li $v0,10     #exit
li $a0,1
syscall


#### CHECK NIL ####
.data
.align 2
CNstr: .asciiz "cant access nil record\n"

.text
_checkNil:
beqz $a0,CNbad
j $ra
CNbad:
li $v0,4      #printstr
la $a0,CIstr
syscall
li $v0,10     #exit
li $a0,1
syscall


#### PRINT ####
.text
_print:
lw $a1,0($a0)
add $a0,$a0,4
add $a2,$a0,$a1
lb $a3,($a2)
sb $0,($a2)
li $v0,4
syscall
sb $a3,($a2)
j $ra


#### STRINGEQUAL ####
.text
_stringCompare:
beq $a0,$a1,Lrunt10
lw  $a2,($a0)
lw  $a3,($a1)
addiu $a0,$a0,4
addiu $a1,$a1,4
beq $a2,$a3,Lrunt11
Lrunt13:
sub  $v0,$t0,$t1
j $ra
Lrunt12:
lbu  $t0,($a0)
lbu  $t1,($a1)
bne  $t0,$t1,Lrunt13
addiu $a0,$a0,1
addiu $a1,$a1,1
addiu $a2,$a2,-1
Lrunt11:
bgez $a2,Lrunt12
Lrunt10:
li $v0,0
j $ra

#### FLUSH? ####


#### ORD ####
.text
_ord:
lw $a1,($a0)
li $v0,-1
beqz $a1,Lrunt5
lbu $v0,4($a0)
Lrunt5:
j $ra


#### CHR ####
# Todo: check for out of bounds
.text
_chr:
sll  $a0,$a0,3
la   $v0,Runtconsts($a0)
j $ra


#### SIZE ####
_size:
lw $v0,($a0)
j $ra


#### SUBSTRING ####
.data
Lrunt40:  .asciiz "substring out of bounds\n"
.align 2
Lruntempty:
.word 0
.ascii ""

.text
_substring:
lw $t1,($a0)
bltz $a1,Lrunt41
add $t2,$a1,$a2
sgt $t3,$t2,$t1
bnez $t3,Lrunt41
add $t1,$a0,$a1
addi $t1, 4
bne $a2,1,Lrunt42
lbu $a0,($t1)
b _chr
Lrunt42:
bnez $a2,Lrunt43
la  $v0,Lruntempty
j $ra
Lrunt43:
addi $a0,$a2,4 # add space for the length field
li   $v0,9
syscall
move $t2,$v0
sw $a2,($t2) # store the length in the length field
addiu $t2,4
Lrunt44:
lbu  $t3,($t1)
sb   $t3,($t2)
addiu $t1,1
addiu $t2,1
addiu $a2,-1
bgtz $a2,Lrunt44
j $ra
Lrunt41:
li   $v0,4
la   $a0,Lrunt40
syscall
li   $v0,10
syscall


#### CONCAT ####
_concat:
lw $t0,($a0) # length str1
lw $t1,($a1) # length str2
beqz $t0,Lrunt50
beqz $t1,Lrunt51
addiu  $t2,$a0,4 # point to str1
addiu  $t3,$a1,4 # point to str2
add  $t4,$t0,$t1 # store the new length
addiu $a0,$t4,4 # new length + 4 = size to allocate
li $v0,9
syscall
addiu $t5,$v0,4 # point to the first char in newStr
sw $t4,($v0) # store the size
Lrunt52:
lbu $a0,($t2)
sb  $a0,($t5)
addiu $t2,1
addiu $t5,1
addiu $t0,-1
bgtz $t0,Lrunt52
Lrunt53:
lbu $a0,($t3)
sb  $a0,($t5)
addiu $t3,1
addiu $t5,1
addiu $t1,-1
bgtz $t1,Lrunt53
j $ra
Lrunt50:
move $v0,$a1
j $ra
Lrunt51:
move $v0,$a0
j $ra


#### NOT ####
_not:
seq $v0,$a0,0
j $ra


#### GETCHAR ####
.data
getchbuf: .space 200
getchptr: .word getchbuf

.text
_getchar:
lw  $a0,getchptr # get a pointer to the current location in the buffer
lbu $v0,($a0) # load the byte at the current location in the buffer
add $a0,$a0,1 # advance the ptr 1 byte
bnez $v0,Lrunt6 # if the byte just read is null...
li $v0,8
la $a0,getchbuf
li $a1,200
syscall
la $a0,getchbuf
lbu $v0,($a0)
add $a0,$a0,1
bnez $v0,Lrunt6
li $v0,-1
j $ra
Lrunt6:
sw $a0,getchptr
sll  $v0,$v0,3 # Multiply by 8 b/c string pointers are 8
la   $v0,Runtconsts($v0)
j $ra


#### EXIT ####
exit:
li $v0, 10
syscall


#### TIGERMAIN #####
