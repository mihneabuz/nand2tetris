// pushconstant111
@111
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant333
@333
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant888
@888
D=A
@SP
M=M+1
A=M-1
M=D

// popstatic8
@StaticTest.8
D=A
@SP
M=M-1
A=M+1
M=D
A=A-1
D=M
A=A+1
A=M
M=D

// popstatic3
@StaticTest.3
D=A
@SP
M=M-1
A=M+1
M=D
A=A-1
D=M
A=A+1
A=M
M=D

// popstatic1
@StaticTest.1
D=A
@SP
M=M-1
A=M+1
M=D
A=A-1
D=M
A=A+1
A=M
M=D

// pushstatic3
@StaticTest.3
D=M
@SP
M=M+1
A=M-1
M=D

// pushstatic1
@StaticTest.1
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

// pushstatic8
@StaticTest.8
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
