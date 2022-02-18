// pushconstant17
@17
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant17
@17
D=A
@SP
M=M+1
A=M-1
M=D

// eq
@SP
AM=M-1
D=M
A=A-1
D=M-D
@_TRUE_LABEL_EQ_1
D; JEQ
(_FALSE_LABEL_EQ_1)
@SP
A=M-1
M=0
@_END_LABEL_EQ_1
0; JEQ
(_TRUE_LABEL_EQ_1)
@SP
A=M-1
M=-1
(_END_LABEL_EQ_1)

// pushconstant17
@17
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant16
@16
D=A
@SP
M=M+1
A=M-1
M=D

// eq
@SP
AM=M-1
D=M
A=A-1
D=M-D
@_TRUE_LABEL_EQ_2
D; JEQ
(_FALSE_LABEL_EQ_2)
@SP
A=M-1
M=0
@_END_LABEL_EQ_2
0; JEQ
(_TRUE_LABEL_EQ_2)
@SP
A=M-1
M=-1
(_END_LABEL_EQ_2)

// pushconstant16
@16
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant17
@17
D=A
@SP
M=M+1
A=M-1
M=D

// eq
@SP
AM=M-1
D=M
A=A-1
D=M-D
@_TRUE_LABEL_EQ_3
D; JEQ
(_FALSE_LABEL_EQ_3)
@SP
A=M-1
M=0
@_END_LABEL_EQ_3
0; JEQ
(_TRUE_LABEL_EQ_3)
@SP
A=M-1
M=-1
(_END_LABEL_EQ_3)

// pushconstant892
@892
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant891
@891
D=A
@SP
M=M+1
A=M-1
M=D

// lt
@SP
AM=M-1
D=M
A=A-1
D=M-D
@_TRUE_LABEL_LT_4
D; JLT
(_FALSE_LABEL_LT_4)
@SP
A=M-1
M=0
@_END_LABEL_LT_4
0; JEQ
(_TRUE_LABEL_LT_4)
@SP
A=M-1
M=-1
(_END_LABEL_LT_4)

// pushconstant891
@891
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant892
@892
D=A
@SP
M=M+1
A=M-1
M=D

// lt
@SP
AM=M-1
D=M
A=A-1
D=M-D
@_TRUE_LABEL_LT_5
D; JLT
(_FALSE_LABEL_LT_5)
@SP
A=M-1
M=0
@_END_LABEL_LT_5
0; JEQ
(_TRUE_LABEL_LT_5)
@SP
A=M-1
M=-1
(_END_LABEL_LT_5)

// pushconstant891
@891
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant891
@891
D=A
@SP
M=M+1
A=M-1
M=D

// lt
@SP
AM=M-1
D=M
A=A-1
D=M-D
@_TRUE_LABEL_LT_6
D; JLT
(_FALSE_LABEL_LT_6)
@SP
A=M-1
M=0
@_END_LABEL_LT_6
0; JEQ
(_TRUE_LABEL_LT_6)
@SP
A=M-1
M=-1
(_END_LABEL_LT_6)

// pushconstant32767
@32767
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant32766
@32766
D=A
@SP
M=M+1
A=M-1
M=D

// gt
@SP
AM=M-1
D=M
A=A-1
D=M-D
@_TRUE_LABEL_GT_7
D; JGT
(_FALSE_LABEL_GT_7)
@SP
A=M-1
M=0
@_END_LABEL_GT_7
0; JEQ
(_TRUE_LABEL_GT_7)
@SP
A=M-1
M=-1
(_END_LABEL_GT_7)

// pushconstant32766
@32766
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant32767
@32767
D=A
@SP
M=M+1
A=M-1
M=D

// gt
@SP
AM=M-1
D=M
A=A-1
D=M-D
@_TRUE_LABEL_GT_8
D; JGT
(_FALSE_LABEL_GT_8)
@SP
A=M-1
M=0
@_END_LABEL_GT_8
0; JEQ
(_TRUE_LABEL_GT_8)
@SP
A=M-1
M=-1
(_END_LABEL_GT_8)

// pushconstant32766
@32766
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant32766
@32766
D=A
@SP
M=M+1
A=M-1
M=D

// gt
@SP
AM=M-1
D=M
A=A-1
D=M-D
@_TRUE_LABEL_GT_9
D; JGT
(_FALSE_LABEL_GT_9)
@SP
A=M-1
M=0
@_END_LABEL_GT_9
0; JEQ
(_TRUE_LABEL_GT_9)
@SP
A=M-1
M=-1
(_END_LABEL_GT_9)

// pushconstant57
@57
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant31
@31
D=A
@SP
M=M+1
A=M-1
M=D

// pushconstant53
@53
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

// pushconstant112
@112
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

// neg
@SP
A=M-1
M=-M

// and
@SP
AM=M-1
D=M
A=A-1
M=M&D

// pushconstant82
@82
D=A
@SP
M=M+1
A=M-1
M=D

// or
@SP
AM=M-1
D=M
A=A-1
M=M|D

// not
@SP
A=M-1
M=!M
