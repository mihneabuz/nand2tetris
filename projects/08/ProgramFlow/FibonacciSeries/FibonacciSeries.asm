// pushargument1
@ARG
D=M
@1
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D

// poppointer1
@3
D=A
@1
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

// pushconstant0
@0
D=A
@SP
M=M+1
A=M-1
M=D

// popthat0
@THAT
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

// pushconstant1
@1
D=A
@SP
M=M+1
A=M-1
M=D

// popthat1
@THAT
D=M
@1
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

// pushconstant2
@2
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

// labelMAIN_LOOP_START
(MAIN_LOOP_START)

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

// if-gotoCOMPUTE_ELEMENT
@SP
AM=M-1
D=M
@COMPUTE_ELEMENT
D; JNE

// gotoEND_PROGRAM
@END_PROGRAM
0; JEQ

// labelCOMPUTE_ELEMENT
(COMPUTE_ELEMENT)

// pushthat0
@THAT
D=M
@0
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D

// pushthat1
@THAT
D=M
@1
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

// popthat2
@THAT
D=M
@2
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

// pushpointer1
@3
D=A
@1
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

// add
@SP
AM=M-1
D=M
A=A-1
M=M+D

// poppointer1
@3
D=A
@1
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

// gotoMAIN_LOOP_START
@MAIN_LOOP_START
0; JEQ

// labelEND_PROGRAM
(END_PROGRAM)
