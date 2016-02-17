#include <data.S>
	
;;; Constants
.set RAMEND,0x045f
.set SPH,0x3e
.set SPL,0x3d
.set zl,30
.set zh,31

.org 0x00100
__vectors:
	rjmp main
	
.section .text
.global main
main:
	ldi r16,hi8(RAMEND)
	out SPH,r16
	ldi r16,lo8(RAMEND)
	out SPL,r16
init:	
	nop

startTestLDI:		; Assign register to a constant
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
	ldi r16,0xf0
	ldi r16,0xf1
	ldi r16,0xf2
	ldi r16,0xf3
	ldi r16,0xf4
	ldi r16,0xf5
	ldi r16,0xf6
	ldi r16,0xf7
	ldi r16,0xf8
	ldi r16,0xf9
	ldi r16,0xfa
	ldi r16,0xfb
	ldi r16,0xfc
	ldi r16,0xfd
	ldi r16,0xfe
	ldi r16,0xff
	ldi r17,0xc0
	ldi r17,0xc1
	ldi r17,0xc2
	ldi r17,0xc3
	ldi r17,0xc4
	ldi r17,0xc5
	ldi r17,0xc6
	ldi r17,0xc7
	ldi r17,0xc8
	ldi r17,0xc9
	ldi r17,0xca
	ldi r17,0xcb
	ldi r17,0xcc
	ldi r17,0xcd
	ldi r17,0xce
	ldi r17,0xcf
	ldi r18,0xff
	ldi r19,0xff
	ldi r20,0xff
	ldi r21,0xff
	ldi r22,0xff
	ldi r23,0xff
	ldi r24,0xff
	ldi r25,0xff
	ldi r26,0xff
	ldi r27,0xff
	ldi r28,0xff
	ldi r29,0xff
	ldi r30,0xff
	ldi r31,0xff
stopTestLDI:
	nop

startTestLPM:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM
	inc zl
	jmp 1b

testMOV:			; Moving data from registers to each other
	mov r0,r16

testJUMPS:
	rjmp main

.end
