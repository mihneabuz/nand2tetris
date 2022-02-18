// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R0 and stores the result in R1.
// (R0, R0, R1 refer to RAM[0], RAM[1], and RAM[2], respectively.)
//
// This program only needs to handle arguments that satisfy
// R0 >= 0, R0 >= 0, and R0*R0 < 32768.

// Put your code here.

	// init R2 with 0
	@R2
	M=0
	
	// if R0 is 0 then exit
	@R0
	D=M
	@EXIT
	D;JEQ

	// if R1 is 0 then exit
	@R1
	D=M
	@EXIT
	D;JEQ

	// if R1 > R0 swap them
	@R1
	D=M
	@R0
	D=M-D
	@LOOP
	D;JGE

(SWAP)
	@R0
	D=M
	@temp
	M=D
	@R1
	D=M
	@R0
	M=D
	@temp
	D=M
	@R1
	M=D


(LOOP)
	@R0
	D=M

	@R2
	M=M+D

	@R1
	D=M-1

	@EXIT
	D;JEQ

	@R1
	M=D

	@LOOP
	0;JEQ

(EXIT)
	@EXIT
	0;JEQ
