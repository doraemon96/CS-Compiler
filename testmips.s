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
syscall
move $a2,$v0
b Lrunt2
Lrunt1:
sw $a1,($a2)
sub $a0,$a0,4
add $a2,$a2,4
Lrunt2:
bgtz $a0, Lrunt1
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
bltz $a1,CIbad
j $ra
CIbad:
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
li  $v0,0
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
li $v0,1
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
#### SUBSTRING ####
#### CONCAT ####
#### NOT ####


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



#### TIGERMAIN #####
