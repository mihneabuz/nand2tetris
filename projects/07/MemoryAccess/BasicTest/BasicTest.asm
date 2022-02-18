// pushconstant10
@10
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

// pushconstant21
@21
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant22
@22
D=A
@SP
M=M+1
A=M-1
M=D

// popargument2
@ARG
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

// popargument1
@ARG
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

// pushconstant36
@36
D=A
@SP
M=M+1
A=M-1
M=D

// popthis6
@THIS
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

// pushconstant42
@42
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant45
@45
D=A
@SP
M=M+1
A=M-1
M=D

// popthat5
@THAT
D=M
@5
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

// pushconstant510
@510
D=A
@SP
M=M+1
A=M-1
M=D

// poptemp6
@5
D=A
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

// pushthat5
@THAT
D=M
@5
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

// sub
@SP
AM=M-1
D=M
A=A-1
M=M-D

// pushthis6
@THIS
D=M
@6
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D

// pushthis6
@THIS
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

// sub
@SP
AM=M-1
D=M
A=A-1
M=M-D

// pushtemp6
@5
D=A
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
