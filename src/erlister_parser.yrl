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
        param_def param_list param_spec
	def_def def_list
	out_def out_list
        clocks_def clocks_list clock
        misc_def misc_defs
        submachines_def
	states_def
	trans_def trans_list trans_list_item
        trans_def_list trans_def_item
        start_timer start_timer_list
        action action_decls action_decl action_stmts action_stmt
        formula
        identifier_list
        expr expr_list
        number
        type opt_type
        comp arith
	.

Terminals
	'&&' '||' '!' '->' '<->' 'ALL' 'SOME'
        '&' '^' '|' '~' '<<' '>>'
	'=' '(' ')' '[' ']' '{' '}' ';' ':' '.' ',' '-' '?'
        '<=' '>=' '<' '>' '==' '!='
        '+' '*' '/' '%'
	'machine' 'in' 'param' 'def' 'out' 'clocks' 'states' 'trans' 'start' 
        'timeout' 'submachines' 'submachine' identifier
        flonum hexnum octnum binnum decnum
        true false
        boolean unsigned8 unsigned16 unsigned32
        integer8 integer16 integer32
	.

Left 200  '->'.
Left 250  '<->'.
Left 260  '?' ':'.
Left 300  '||'.
Left 400  '&&'.
Left 410  '|'.
Left 420  '^'.
Left 430  '&'.
Left 440  '==' '!='.
Left 450  '<' '>'  '<=' '>='.
Left 500  '<<' '>>'.
Left 600  '+' '-'.
Left 700  '*' '/' '%'.
Unary 900 '!' '~' 'ALL' 'SOME'.

Rootsymbol definitions.

definitions -> machine_defs : '$1'.

machine_defs -> machine_def : ['$1'].
machine_defs -> machine_def machine_defs  : ['$1'|'$2'].

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
submachine_defs -> submachine_def submachine_defs : ['$1'|'$2'].

submachine_def ->
    'submachine' identifier ';'
	misc_defs
	states_def 
	trans_def :
	{submachine,line('$1'),'$2','$4','$5','$6'}.

misc_defs -> '$empty' : [].
misc_defs -> misc_def misc_defs  : '$1'++'$2'.

misc_def -> in_def    : ['$1'].
misc_def -> param_def : ['$1'].
misc_def -> def_def   : ['$1'].
misc_def -> out_def   : ['$1'].
misc_def -> clocks_def : ['$1'].

param_def -> opt_type 'param' param_list ';' : {param,'$1','$3'}.

param_list -> param_spec : ['$1'].
param_list -> param_list ',' param_spec : '$1' ++ ['$3'].

param_spec -> identifier : {'$1',default}.
param_spec -> identifier '=' number : {'$1','$3'}.

in_def -> opt_type 'in' in_list ';' : {in,'$1','$3'}.

in_list -> identifier : ['$1'].
in_list -> identifier '=' formula : [{'=',line('$2'),'$1','$3'}].
in_list -> in_list ',' identifier   : '$1' ++ ['$3'].
in_list -> in_list ',' identifier '=' formula : 
	       '$1' ++ [{'=',line('$4'),'$3','$5'}].

def_def -> opt_type 'def' def_list ';' : {def,'$1','$3'}.

def_list -> identifier '=' formula : [{'=',line('$2'),'$1','$3'}].
def_list -> def_list ',' identifier '=' formula : 
		'$1' ++ [{'=',line('$4'),'$3','$5'}].

out_def -> opt_type 'out' out_list ';' : {out,'$1','$3'}.

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

trans_def_item -> identifier formula action :
		      {'$1','$2',[],'$3'}.
trans_def_item -> identifier formula start_timer_list action :
		      {'$1','$2','$3','$4'}.

start_timer_list -> start_timer    : ['$1'].
start_timer_list -> start_timer_list start_timer : '$1'++['$2'].

start_timer -> 'start' '(' identifier ')' : '$3'.

action -> '$empty' : [].
action -> '{' action_decls action_stmts '}' : '$2'++'$3'.

action_decls -> '$empty' : [].
action_decls -> action_decl ';' action_decls : ['$1'|'$3'].

action_decl -> type identifier : 
		   {decl,line('$2'),'$1','$2',undefined}.
action_decl -> type identifier '=' formula :
		   {decl,line('$2'),'$1','$2','$4'}.

action_stmts -> '$empty' : [].
action_stmts -> action_stmt ';' action_stmts : ['$1'|'$3'].

action_stmt -> identifier '=' formula :
		   {store,line('$1'),'$1','$3'}.
action_stmt -> identifier '(' expr_list ')' : 
		   {call,line('$1'),'$1','$3'}.

identifier_list -> identifier : ['$1'].
identifier_list -> identifier ',' identifier_list : ['$1'|'$3'].

expr_list -> '$empty' : [].
expr_list -> expr : ['$1'].
expr_list -> expr ',' expr_list : ['$1'|'$3'].

expr -> arith : '$1'.
expr -> comp  : '$1'.

opt_type -> type : '$1'.
opt_type -> '$empty' : boolean.

type -> boolean    : boolean.
type -> unsigned8  : unsigned8.
type -> unsigned16 : unsigned16.
type -> unsigned32 : unsigned32.
type -> integer8   : integer8.
type -> integer16  : integer16.
type -> integer32  : integer32.

formula -> true  : '$1'.
formula -> false : '$1'.
formula -> arith : '$1'.
formula -> identifier '(' expr_list ')' :{pred,line('$1'),'$1','$3'}.
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
arith -> '-' arith :
	     case '$2' of
		 {decnum,Ln,Ds} -> {decnum,Ln,[$-|Ds]};
		 {hexnum,Ln,Ds} -> {hexnum,Ln,[$-|Ds]};
		 {octnum,Ln,Ds} -> {octnum,Ln,[$-|Ds]};
		 {binnum,Ln,Ds} -> {binnum,Ln,[$-|Ds]};
		 {flonum,Ln,Ds} -> {flonum,Ln,[$-|Ds]};
		 _ -> {'-',line('$1'),'$2'}
	     end.
arith -> '~' arith : {'-',line('$1'),'$2'}.
arith -> arith '+' arith : {'+',line('$1'),'$1','$3'}.
arith -> arith '-' arith : {'-',line('$1'),'$1','$3'}.
arith -> arith '*' arith : {'*',line('$1'),'$1','$3'}.
arith -> arith '/' arith : {'/',line('$1'),'$1','$3'}.
arith -> arith '%' arith : {'%',line('$1'),'$1','$3'}.
arith -> arith '&' arith : {'&',line('$1'),'$1','$3'}.
arith -> arith '|' arith : {'|',line('$1'),'$1','$3'}.
arith -> arith '^' arith : {'^',line('$1'),'$1','$3'}.
arith -> arith '<<' arith : {'<<',line('$1'),'$1','$3'}.
arith -> arith '>>' arith : {'>>',line('$1'),'$1','$3'}.
arith -> formula '?' formula ':' formula : {'?',line('$1'),'$1','$3','$5'}.

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
