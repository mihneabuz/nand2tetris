// initialize
@256
D=A
@SP
M=D

// call Sys.init 
@RETURN_LABEL_1
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
@SP
D=M
@LCL
M=D
@5
D=D-A
@0
D=D-A
@ARG
M=D
@Sys.init
0; JEQ
(RETURN_LABEL_1)

// function Sys.init 
(Sys.init)
@0
D=A
@SP
AM=M+D

// pushconstant4000
@4000
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

// pushconstant5000
@5000
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

// call Sys.main 
@RETURN_LABEL_2
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
@SP
D=M
@LCL
M=D
@5
D=D-A
@0
D=D-A
@ARG
M=D
@Sys.main
0; JEQ
(RETURN_LABEL_2)

// poptemp1
@5
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

// labelLOOP
(LOOP)

// gotoLOOP
@LOOP
0; JEQ

// function Sys.main 
(Sys.main)
@5
D=A
@SP
AM=M+D
A=A-1
M=0
A=A-1
M=0
A=A-1
M=0
A=A-1
M=0
A=A-1
M=0

// pushconstant4001
@4001
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

// pushconstant5001
@5001
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

// pushconstant200
@200
D=A
@SP
M=M+1
A=M-1
M=D

// poplocal1
@LCL
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

// pushconstant40
@40
D=A
@SP
M=M+1
A=M-1
M=D

// poplocal2
@LCL
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

// pushconstant6
@6
D=A
@SP
M=M+1
A=M-1
M=D

// poplocal3
@LCL
D=M
@3
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

// pushconstant123
@123
D=A
@SP
M=M+1
A=M-1
M=D

// call Sys.add12 
@RETURN_LABEL_3
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
@SP
D=M
@LCL
M=D
@5
D=D-A
@1
D=D-A
@ARG
M=D
@Sys.add12
0; JEQ
(RETURN_LABEL_3)

// poptemp0
@5
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

// pushlocal1
@LCL
D=M
@1
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D

// pushlocal2
@LCL
D=M
@2
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D

// pushlocal3
@LCL
D=M
@3
A=D+A
D=M
@SP
M=M+1
A=M-1
M=D

// pushlocal4
@LCL
D=M
@4
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

// add
@SP
AM=M-1
D=M
A=A-1
M=M+D

// add
@SP
AM=M-1
D=M
A=A-1
M=M+D

// add
@SP
AM=M-1
D=M
A=A-1
M=M+D

// return
@LCL
D=M
@R13
M=D
@5
A=D-A
D=M
@R14
M=D
@SP
AM=M-1
D=M
@ARG
A=M
M=D
@ARG
D=M
@SP
M=D+1
@R13
AM=M-1
D=M
@THAT
M=D
@R13
AM=M-1
D=M
@THIS
M=D
@R13
AM=M-1
D=M
@ARG
M=D
@R13
AM=M-1
D=M
@LCL
M=D
@R14
A=M
0; JEQ

// function Sys.add12 
(Sys.add12)
@0
D=A
@SP
AM=M+D

// pushconstant4002
@4002
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

// pushconstant5002
@5002
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

// pushconstant12
@12
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

// return
@LCL
D=M
@R13
M=D
@5
A=D-A
D=M
@R14
M=D
@SP
AM=M-1
D=M
@ARG
A=M
M=D
@ARG
D=M
@SP
M=D+1
@R13
AM=M-1
D=M
@THAT
M=D
@R13
AM=M-1
D=M
@THIS
M=D
@R13
AM=M-1
D=M
@ARG
M=D
@R13
AM=M-1
D=M
@LCL
M=D
@R14
A=M
0; JEQ
