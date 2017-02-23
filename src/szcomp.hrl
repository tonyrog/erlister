-ifndef(__SZCOMP_HRL__).
-define(__SZCOMP_HRL__, true).

-define(OPCODE1(X), ((X) band 0x7f)).
-define(OPCODE2(A,B), (16#80 bor (((B) band 15) bsl 3) bor ((A) band 7))).

%% op3 : first part of OPCODE2 also an OPCODE1
-define(ZBRAN_H,     1).  %% either 4 or 8 bits offset
-define(PUSH_H,      2).  %% const: ( -- x ) either 4 or 8 bits
-define(DUP,         2).  %% ( a -- a a )
-define(ROT,         3).  %% ( x1 x2 x3 -- x2 x3 x1 )
-define(OVER,        4).  %% over: ( x1 x2 -- x1 x2 x1 )
-define(DROP,        5).  %% drop: ( x1 -- )
-define(SWAP,        6).  %% ( a b -- b a )
-define(SUB,         7).   %% ( x1 x2 -- (x1-x2) )

%% op4 : second part of OPCODE2 also an OPCODE1
-define(ADD,         8).   %% +  ( x1 x2 -- (x1+x2) )
-define(MUL,         9).   %% *: ( x1 x2 -- (x1*x2) )
-define(EQ,          10).  %% =: ( a b -- ( a==b) )
-define(AND,         11).  %% and: ( a b -- (a&b) )
-define(OR,          12).  %% or: ( a b -- (a|b) )
-define(ZEQ,         13).  %% 0=:  ( a -- (a==0) )
-define(ZLT,         14).  %% 0<:  ( a -- (a<0) )
-define(NOT,         15).  %% not: ( a -- not a)

%% op7 : reset of opcodes 
-define(DIV,         16).  %% ( a b -- (a/b) )
-define(MOD,         17).  %% ( a b -- (a%b) )
-define(XOR,         18).  %% ( a b -- (a^b) )
-define(NEG,         19).  %% ( a -- (-a) )
-define(INV,         20).  %% ( a -- (~a) )
-define(BSL,         21).  %% <<: ( a n -- (a << n) )
-define(BSR,         22).  %% >>: ( a n -- (a >> n) )
-define(ASR,         23).  %% >>a: ( a n -- (a >> n) )
-define(INC,         24).  %% 1+ : ( a -- (a+1) )
-define(DEC,         25).  %% 1- : ( a -- (a-1) )
-define(ABS,         26).  %% ( a -- |a| )
-define(MIN,         27).  %% ( a b -- min(a,b) )
-define(MAX,         28).  %% ( a b -- max(a,b) )
-define(ULT,         29).  %% ( a b -- (a < b) )
-define(LT,          30).  %% ( a b -- (a < b) )
-define(STORE,       31).  %% ( a i -- )
-define(FETCH,       32).  %% ( i -- a )
-define(NOP,         33).  %% nop: ( -- )
-define(LTE,         34).  %% '-' '--' '0<'
-define(ULTE,        35).  %% '-' '--' '0<'
-define(RET,         36).  %% ( -- ) R: ( addr -- )
-define(PUSH_W,      37).  %% ( -- w )
-define(PUSH_L,      38).  %% ( -- l )
-define(BRAN_B,      39).  %% ( -- )
-define(BRAN_W,      40).  %% ( -- )
-define(ZBRAN_W,     41).  %% ( cond -- )
-define(IBRAN_B,     42).  %% ( i -- )
-define(IBRAN_W,     43).  %% ( i -- )
-define(CALL_B,      44).  %% ( -- ) R: ( -- addr )
-define(CALL_W,      45).  %% ( -- ) R: ( -- addr )
-define(SYS_B,       46).  %% sys ( x1 .. xn -- xi .. xj )

-define(EXIT,        47).  %% ( -- )  - fixme

%% when PUSH_H/ZBRAN_H is a op7 then it's the byte versions
-define(PUSH_B,        ?PUSH_H).
-define(ZBRAN_B,       ?ZBRAN_H).

%% Failure codes
-define(FAIL_STACK_OVERFLOW,    -1).
-define(FAIL_STACK_UNDERFLOW,   -2).
-define(FAIL_RSTACK_OVERFLOW,   -3).
-define(FAIL_RSTACK_UNDERFLOW,  -4).
-define(FAIL_DIV_ZERO,          -5).
-define(FAIL_TIMER_OVERFLOW,    -6).
-define(FAIL_MEMORY_OVERFLOW,   -7).

%% SYSTEM CALLS
-define(SYS_PARAM_FETCH,   1).  %% ( i s -- n )
-define(SYS_PARAM_STORE,   2).  %% ( v i s -- )
-define(SYS_TIMER_INIT,    3).  %% ( i --  )
-define(SYS_TIMER_START,   4).  %% ( i time --  )
-define(SYS_TIMER_STOP,    5).  %% ( i --  )
-define(SYS_TIMER_TIMEOUT, 6).  %% ( i -- f )
-define(SYS_TIMER_RUNNING, 7).  %% ( i -- f )
-define(SYS_INPUT_FETCH,   8).  %% ( i k -- n )
-define(SYS_SELECT,        9).  %% ( tmask imask -- )
-define(SYS_EMIT,         10).  %% ( c -- )
-define(SYS_KEY,          11).  %% (   -- c )

%% INPUT kind (k)
-define(INPUT_BOOLEAN, 0).
-define(INPUT_ANALOG,  1).
-define(INPUT_ENCODER, 2).

-endif.
