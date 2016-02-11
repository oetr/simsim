;;; Constants
.set RAMEND,0x045f
.set SPH,0x3e
.set SPL,0x3d

.org 0x0000
__vectors:
	rjmp main
	
.section .text
.global main
main:
	ldi r16,hi8(RAMEND)
	out SPH,r16
	ldi r16,lo8(RAMEND)
	out SPL,r16
	nop

testSetConstant:		; Assign register to a constant
	ldi r16,0x00
	ldi r16,0x01
	ldi r16,0x02
	ldi r16,0x03
	ldi r16,0x04
	ldi r16,0x05
	ldi r16,0x06
	ldi r16,0x07
	ldi r16,0x08
	ldi r16,0x09
	ldi r16,0x0a
	ldi r16,0x0b
	ldi r16,0x0c
	ldi r16,0x0d
	ldi r16,0x0e
	ldi r16,0x0f

testMOV:			; Moving data from registers to each other
	mov r0,r16

testJUMPS:
	rjmp main

.end
