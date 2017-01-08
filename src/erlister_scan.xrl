%%
%% ERLISTER scanner
%%

Definitions.

B	= [0-1]
D	= [0-9]
O	= [0-7]
L	= [a-zA-Z_]
H	= [a-fA-F0-9]
WS      = [\000-\s]

Rules.

machine		: {token,{machine,TokenLine}}.
in		: {token,{in,TokenLine}}.
def		: {token,{def,TokenLine}}.
out		: {token,{out,TokenLine}}.
clocks		: {token,{clocks,TokenLine}}.
states		: {token,{states,TokenLine}}.
trans		: {token,{trans,TokenLine}}.
start		: {token,{start,TokenLine}}.
timeout		: {token,{timeout,TokenLine}}.
submachines	: {token,{submachines,TokenLine}}.
submachine	: {token,{submachine,TokenLine}}.

\&\&		: {token,{'&&',TokenLine}}.
\|\|		: {token,{'||',TokenLine}}.
\!		: {token,{'!',TokenLine}}.
->		: {token,{'->',TokenLine}}.
<->		: {token,{'<->',TokenLine}}.
ALL		: {token,{'ALL',TokenLine}}.
SOME		: {token,{'SOME',TokenLine}}.
{D}+\.{D}*      : {token,{flonum,TokenLine,TokenChars}}.
1		: {token,{'1',TokenLine}}.
0		: {token,{'0',TokenLine}}.
0[xX]{H}+	: {token,{hexnum,TokenLine,TokenChars}}.
0[b]{B}+        : {token,{binnum,TokenLine,TokenChars}}.
0{O}+	        : {token,{octnum,TokenLine,TokenChars}}.
{D}+		: {token,{decnum,TokenLine,TokenChars}}.
=		: {token,{'=',TokenLine}}.
\(		: {token,{'(',TokenLine}}.
\)		: {token,{')',TokenLine}}.
\[		: {token,{'[',TokenLine}}.
\]		: {token,{']',TokenLine}}.
;		: {token,{';',TokenLine}}.
:		: {token,{':',TokenLine}}.
,		: {token,{',',TokenLine}}.
\.		: {token,{'.',TokenLine}}.
-		: {token,{'-',TokenLine}}.
//.*\n          : skip_token.
{L}({L}|{D})*	: {token,{identifier,TokenLine,TokenChars}}.

%%\+		: {token,{'+',TokenLine}}.
%%\*		: {token,{'*',TokenLine}}.
%%/		: {token,{'/',TokenLine}}.
%%\%		: {token,{'%',TokenLine}}.
%%<		: {token,{'<',TokenLine}}.
%%>		: {token,{'>',TokenLine}}.
%%<=		: {token,{'<=',TokenLine}}.
%%>=		: {token,{'>=',TokenLine}}.
%%==		: {token,{'==',TokenLine}}.
%%!=		: {token,{'!=',TokenLine}}.

{WS}+		: skip_token .

Erlang code.
