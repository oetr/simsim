#include <data.S>
	
;;; Constants
.set RAMEND,0x045f
.set SPH,0x3e
.set SPL,0x3d
.set xl,26
.set xh,27
.set yl,28
.set yh,29
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

startTestLPMR0:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM
	adiw r30,1
	jmp 1b

startTestLPMR1:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R1,z
	adiw r30,1
	jmp 1b

startTestLPMR2:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R2,z
	adiw r30,1
	jmp 1b

startTestLPMR3:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R3,z
	adiw r30,1
	jmp 1b

startTestLPMR4:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R4,z
	adiw r30,1
	jmp 1b

startTestLPMR5:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R5,z
	adiw r30,1
	jmp 1b

startTestLPMR6:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R6,z
	adiw r30,1
	jmp 1b

startTestLPMR7:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R7,z
	adiw r30,1
	jmp 1b

startTestLPMR8:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R8,z
	adiw r30,1
	jmp 1b

startTestLPMR9:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R9,z
	adiw r30,1
	jmp 1b

startTestLPMR10:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R10,z
	adiw r30,1
	jmp 1b

startTestLPMR11:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R11,z
	adiw r30,1
	jmp 1b

startTestLPMR12:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R12,z
	adiw r30,1
	jmp 1b

startTestLPMR13:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R13,z
	adiw r30,1
	jmp 1b

startTestLPMR14:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R14,z
	adiw r30,1
	jmp 1b

startTestLPMR15:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R15,z
	adiw r30,1
	jmp 1b


startTestLPMR16:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM r16,z
	adiw r30,1
	jmp 1b

startTestLPMR17:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R17,z
	adiw r30,1
	jmp 1b

startTestLPMR18:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R18,z
	adiw r30,1
	jmp 1b

startTestLPMR19:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R19,z
	adiw r30,1
	jmp 1b

startTestLPMR20:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM r20,z
	adiw r30,1
	jmp 1b

startTestLPMR21:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R21,z
	adiw r30,1
	jmp 1b

startTestLPMR22:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R22,z
	adiw r30,1
	jmp 1b

startTestLPMR23:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R23,z
	adiw r30,1
	jmp 1b

startTestLPMR24:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R24,z
	adiw r30,1
	jmp 1b

startTestLPMR25:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R25,z
	adiw r30,1
	jmp 1b

startTestLPMR26:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R26,z
	adiw r30,1
	jmp 1b

startTestLPMR27:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R27,z
	adiw r30,1
	jmp 1b

startTestLPMR28:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM r28,z
	adiw r30,1
	jmp 1b

startTestLPMR29:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R29,z
	adiw r30,1
	jmp 1b

startTestLPMZplusR0:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R0,z+
	jmp 1b

startTestLPMZplusR1:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R1,z+
	jmp 1b

startTestLPMZplusR2:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R2,z+
	jmp 1b

startTestLPMZplusR3:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R3,z+
	jmp 1b

startTestLPMZplusR4:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R4,z+
	jmp 1b

startTestLPMZplusR5:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R5,z+
	jmp 1b

startTestLPMZplusR6:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R6,z+
	jmp 1b

startTestLPMZplusR7:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R7,z+
	jmp 1b

startTestLPMZplusR8:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R8,z+
	jmp 1b

startTestLPMZplusR9:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R9,z+
	jmp 1b

startTestLPMZplusR10:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R10,z+
	jmp 1b

startTestLPMZplusR11:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R11,z+
	jmp 1b

startTestLPMZplusR12:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R12,z+
	jmp 1b

startTestLPMZplusR13:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R13,z+
	jmp 1b

startTestLPMZplusR14:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R14,z+
	jmp 1b

startTestLPMZplusR15:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R15,z+
	jmp 1b


startTestLPMZplusR16:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM r16,z+
	jmp 1b

startTestLPMZplusR17:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R17,z+
	jmp 1b

startTestLPMZplusR18:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R18,z+
	jmp 1b

startTestLPMZplusR19:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R19,z+
	jmp 1b

startTestLPMZplusR20:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM r20,z+
	jmp 1b

startTestLPMZplusR21:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R21,z+
	jmp 1b

startTestLPMZplusR22:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R22,z+
	jmp 1b

startTestLPMZplusR23:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R23,z+
	jmp 1b

startTestLPMZplusR24:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R24,z+
	jmp 1b

startTestLPMZplusR25:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R25,z+
	jmp 1b

startTestLPMZplusR26:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R26,z+
	jmp 1b

startTestLPMZplusR27:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R27,z+
	jmp 1b

startTestLPMZplusR28:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM r28,z+
	jmp 1b

startTestLPMZplusR29:
	ldi zh,hi8(sbox)
	ldi zl,0x00
1:	
	LPM R29,z+
	jmp 1b

startTestSTXRr:
	ldi zh,hi8(sbox)
	ldi zl,0x00
	ldi xh,0x00
	ldi xl,0x60
1:	
	lpm r5,z+
	ST X,r5
	adiw xl,1
	jmp 1b
	
	
testMOV0:
	mov r0,r16
	mov r1,r17
	mov r2,r18
	mov r3,r19
	mov r4,r20
	mov r5,r21
	mov r6,r22
	mov r7,r23
	mov r8,r24
	mov r9,r25
	mov r10,r26
	mov r11,r27
	mov r12,r28
	mov r13,r29
	mov r14,r30
	mov r15,r31

testMOV1:
	mov r1,r16
	mov r2,r17
	mov r3,r18
	mov r4,r19
	mov r5,r20
	mov r6,r21
	mov r7,r22
	mov r8,r23
	mov r9,r24
	mov r10,r25
	mov r11,r26
	mov r12,r27
	mov r13,r28
	mov r14,r29
	mov r15,r30
	mov r0,r31

test32BitInstructions:
	cpse r30,r31
	call testJUMPS
	ldi r31,0x00

testJUMPS:
	rjmp main

testMULS:
	muls r16,r17

.end
