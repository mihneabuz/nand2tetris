// pushconstant0
@0
D=A
@SP
M=M+1
A=M-1
M=D

// poplocal0
@LCL
D=M
@0
D=D+A
@SP
M=M-1
A=M+1
M=D
A=A-1
D=M
A=A+1
A=M
M=D

// labelLOOP_START
(LOOP_START)

// pushargument0
@ARG
D=M
@0
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D

// pushlocal0
@LCL
D=M
@0
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D

// add
@SP
AM=M-1
D=M
A=A-1
M=M+D

// poplocal0
@LCL
D=M
@0
D=D+A
@SP
M=M-1
A=M+1
M=D
A=A-1
D=M
A=A+1
A=M
M=D

// pushargument0
@ARG
D=M
@0
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D

// pushconstant1
@1
D=A
@SP
M=M+1
A=M-1
M=D

// sub
@SP
AM=M-1
D=M
A=A-1
M=M-D

// popargument0
@ARG
D=M
@0
D=D+A
@SP
M=M-1
A=M+1
M=D
A=A-1
D=M
A=A+1
A=M
M=D

// pushargument0
@ARG
D=M
@0
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D

// if-gotoLOOP_START
@SP
AM=M-1
D=M
@LOOP_START
D; JNE

// pushlocal0
@LCL
D=M
@0
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D
