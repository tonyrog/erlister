%% -*- erlang -*-
%% ERLISTER parser 
%%
Nonterminals
        definitions
        machine_defs
        machine_def
	atomic_machine_def
	composed_machine_def
	submachine_defs
	submachine_def
	in_def in_list
	def_def def_list
	out_def out_list
        clocks_def clocks_list clock
        misc_def misc_defs
        submachines_def
	states_def
	trans_def trans_list trans_list_item
        trans_def_list trans_def_item
        start_timer start_timer_list
        formula
        identifier_list
        argument_list
        argument
        number
        type
        comp arith
	.

Terminals
	'&&' '||' '!' '->' '<->' 'ALL' 'SOME'
	'=' '(' ')' '[' ']' ';' ':' '.' ',' '-'
        '<=' '>=' '<' '>' '==' '!='
        '+' '*' '/' '%'
	'machine' 'in' 'def' 'out' 'clocks' 'states' 'trans' 'start' 'timeout'
	'submachines' 'submachine' identifier
        flonum hexnum octnum binnum decnum
        true false
        boolean unsigned8 unsigned16 unsigned32
        integer8 integer16 integer32
	.

Left 200  '->'.
Left 250  '<->'.
Left 300  '||'.
Left 400  '&&'.
Left 500  '<' '>' '==' '!=' '<=' '>='.
Left 600  '+' '-'.
Left 700  '*' '/' '%'.
Unary 900 '!' 'ALL' 'SOME'.

Rootsymbol definitions.

definitions -> machine_defs : '$1'.

machine_defs -> machine_def : ['$1'].
machine_defs -> machine_defs machine_def : '$1'++['$2'].

machine_def -> atomic_machine_def : '$1'.
machine_def -> composed_machine_def : '$1'.

atomic_machine_def ->
    'machine' identifier ';'
	misc_defs 
	states_def
	trans_def :
	{machine,line('$1'),'$2','$4','$5','$6'}.

composed_machine_def ->
    'machine' identifier ';'
	submachines_def
	misc_defs
	submachine_defs : 
	{machine,line('$1'),'$2',['$4'|'$5'],'$6'}.

submachine_defs -> submachine_def : ['$1'].
submachine_defs -> submachine_defs submachine_def : '$1'++['$2'].

submachine_def ->
    'submachine' identifier ';'
	misc_defs
	states_def 
	trans_def :
	{submachine,line('$1'),'$2','$4','$5','$6'}.

misc_defs -> misc_def : '$1'.
misc_defs -> misc_defs misc_def : '$1'++'$2'.

misc_def -> in_def  : ['$1'].
misc_def -> def_def : ['$1'].
misc_def -> out_def : ['$1'].
misc_def -> clocks_def : ['$1'].

in_def -> type 'in' in_list ';' : {in,'$1','$3'}.

in_list -> identifier : ['$1'].
in_list -> identifier '=' formula : [{'=',line('$2'),'$1','$3'}].
in_list -> in_list ',' identifier   : '$1' ++ ['$3'].
in_list -> in_list ',' identifier '=' formula : 
	       '$1' ++ [{'=',line('$4'),'$3','$5'}].

def_def -> type 'def' def_list ';' : {def,'$1','$3'}.

def_list -> identifier '=' formula : [{'=',line('$2'),'$1','$3'}].
def_list -> def_list ',' identifier '=' formula : 
		'$1' ++ [{'=',line('$4'),'$3','$5'}].

out_def -> type 'out' out_list ';' : {out,'$1','$3'}.

out_list -> identifier '=' formula : [{'=',line('$2'),'$1','$3'}].
out_list -> out_list ',' identifier '=' formula :
		'$1' ++ [{'=',line('$2'),'$3','$5'}].

clocks_def -> 'clocks' clocks_list ';' : {clocks,'$2'}.

clocks_list -> clock : ['$1'].
clocks_list -> clocks_list ',' clock : '$1'++['$3'].

clock ->
    identifier '=' identifier '[' number '-' number ',' number ']' number :
	{'clock',line('$2'), '$1', '$3', {'$5','$7'}, '$9', '$11'}.

submachines_def -> 'submachines' identifier_list ';' : {submachines,'$2'}.

states_def -> '$empty' : {states,[]}.
states_def -> 'states' identifier_list ';' : {states,'$2'}.

trans_def -> '$empty' : {'trans',[]}.
trans_def -> 'trans' trans_list : {trans,'$2'}.

trans_list -> trans_list_item : ['$1'].
trans_list -> trans_list trans_list_item : '$1'++['$2'].

trans_list_item -> identifier ':' trans_def_list ';' : {'$1','$3'}.

trans_def_list -> trans_def_item  : ['$1'].
trans_def_list -> trans_def_list ',' trans_def_item  : '$1'++['$3'].

trans_def_item -> identifier formula :
		      {'$1','$2',{start,[]}}.
trans_def_item -> identifier formula start_timer_list :
		      {'$1','$2',{start,'$3'}}.

start_timer_list -> start_timer    : ['$1'].
start_timer_list -> start_timer_list start_timer : '$1'++['$2'].

start_timer -> 'start' '(' identifier ')' : '$3'.

identifier_list -> identifier : ['$1'].
identifier_list -> identifier_list ',' identifier : '$1'++['$3'].

argument_list -> argument : ['$1'].
argument_list -> argument_list ',' argument : '$1'++['$3'].

argument -> number : '$1'.
argument -> identifier : '$1'.

type -> boolean    : boolean.
type -> unsigned8  : unsigned8.
type -> unsigned16 : unsigned16.
type -> unsigned32 : unsigned32.
type -> integer8   : integer8.
type -> integer16  : integer16.
type -> integer32  : integer32.
type -> '$empty'   : boolean.

formula -> true  : '$1'.
formula -> false : '$1'.
formula -> arith : '$1'.
formula -> identifier '(' argument_list ')' :{pred,line('$1'),'$1','$3'}.
formula -> comp : '$1'.
formula -> '!' formula : {'!',line('$1'),'$2'}.
formula -> '(' formula ')' : '$2'.
formula -> 'timeout' '(' identifier ')' : {timeout,line('$1'),'$3'}.
formula -> formula '&&' formula : {'&&',line('$2'),'$1','$3'}.
formula -> formula '||' formula : {'||',line('$2'),'$1','$3'}.
formula -> formula '->' formula : {'->',line('$2'),'$1','$3'}.
formula -> formula '<->' formula : {'<->',line('$2'),'$1','$3'}.
formula -> 'ALL' identifier formula : {'ALL',line('$1'),'$2','$3'}.
formula -> 'SOME' identifier formula : {'SOME',line('$1'),'$2','$3'}.

comp -> arith '>'  arith : {'>',line('$1'),'$1','$3'}.
comp -> arith '>=' arith : {'>=',line('$1'),'$1','$3'}.
comp -> arith '<'  arith : {'<',line('$1'),'$1','$3'}.
comp -> arith '<=' arith : {'<=',line('$1'),'$1','$3'}.
comp -> arith '==' arith : {'==',line('$1'),'$1','$3'}.
comp -> arith '!=' arith : {'!=',line('$1'),'$1','$3'}.

arith -> number : '$1'.
arith -> identifier : '$1'.
arith -> identifier '.' identifier : {field,line('$2'),'$1','$3'}.
arith -> '-' argument :
	     case '$2' of
		 {decnum,Ln,Ds} -> {decnum,Ln,[$-|Ds]};
		 {hexnum,Ln,Ds} -> {hexnum,Ln,[$-|Ds]};
		 {octnum,Ln,Ds} -> {octnum,Ln,[$-|Ds]};
		 {binnum,Ln,Ds} -> {binnum,Ln,[$-|Ds]};
		 {flonum,Ln,Ds} -> {flonum,Ln,[$-|Ds]};
		 _ -> {'-',line('$1'),'$2'}
	     end.
arith -> arith '+' arith : {'+',line('$1'),'$1','$3'}.
arith -> arith '-' arith : {'-',line('$1'),'$1','$3'}.
arith -> arith '*' arith : {'*',line('$1'),'$1','$3'}.
arith -> arith '/' arith : {'/',line('$1'),'$1','$3'}.
arith -> arith '%' arith : {'%',line('$1'),'$1','$3'}.

number -> flonum : '$1'.
number -> hexnum : '$1'.
number -> octnum : '$1'.
number -> binnum : '$1'.
number -> decnum : '$1'.

Erlang code.

line([H|_]) -> line(H);
line({_,Line}) -> Line;
line({_,Line,_}) -> Line;
line({_,Line,_,_}) -> Line.
