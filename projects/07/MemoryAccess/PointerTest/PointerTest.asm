// pushconstant3030
@3030
D=A
@SP
M=M+1
A=M-1
M=D

// poppointer0
@3
D=A
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

// pushconstant3040
@3040
D=A
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

// pushconstant32
@32
D=A
@SP
M=M+1
A=M-1
M=D

// popthis2
@THIS
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

// pushconstant46
@46
D=A
@SP
M=M+1
A=M-1
M=D

// popthat6
@THAT
D=M
@6
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

// pushpointer0
@3
D=A
@0
A=D+A
D=M
@SP
M=M+1
A=M-1
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

// add
@SP
AM=M-1
D=M
A=A-1
M=M+D

// pushthis2
@THIS
D=M
@2
A=D+A
D=M
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

// pushthat6
@THAT
D=M
@6
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
