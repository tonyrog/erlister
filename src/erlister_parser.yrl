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
        pred_formula
        sat_formula
        identifier_list
        number
	.

Terminals
	'&&' '||' '!' '->' '<->' 'ALL' 'SOME' '1' '0'
	'=' '(' ')' '[' ']' ';' ':' '.' ',' '-'
	'machine' 'in' 'def' 'out' 'clocks' 'states' 'trans' 'start' 'timeout'
	'submachines' 'submachine' identifier
        flonum hexnum octnum binnum decnum
	.

%% Right 100 '='.
Left 200  '->'.
Left 300  '<->'.
Left 400  '||'.
Left 500  '&&'.
Unary 600 '!' 'ALL' 'SOME'.

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

misc_defs -> '$empty' : [].
misc_defs -> misc_def : '$1'.
misc_defs -> misc_defs misc_def : '$1'++'$2'.

misc_def -> in_def  : ['$1'].
misc_def -> def_def : ['$1'].
misc_def -> out_def : ['$1'].
misc_def -> clocks_def : ['$1'].

in_def -> 'in' in_list ';' : {in,'$2'}.

in_list -> identifier : ['$1'].
in_list -> identifier '=' pred_formula : [{'=',line('$2'),'$1','$3'}].
in_list -> in_list ',' identifier   : '$1' ++ ['$3'].
in_list -> in_list ',' identifier '=' pred_formula : 
	       '$1' ++ [{'=',line('$4'),'$3','$5'}].

def_def -> 'def' def_list ';' : {def,'$2'}.

def_list -> identifier '=' sat_formula : [{'=',line('$2'),'$1','$3'}].
def_list -> def_list ',' identifier '=' sat_formula : 
		'$1' ++ [{'=',line('$4'),'$3','$5'}].

out_def -> 'out' out_list ';' : {out,'$2'}.

out_list -> identifier '=' sat_formula : [{'=',line('$2'),'$1','$3'}].
out_list -> out_list ',' identifier '=' sat_formula :
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

trans_def_item -> identifier sat_formula start_timer_list :
		      {'$1','$2',{start,'$3'}}.

start_timer -> 'start' '(' identifier ')' : '$3'.

start_timer_list -> '$empty' : [].
start_timer_list -> start_timer    : ['$1'].
start_timer_list -> start_timer_list start_timer : '$1'++['$2'].

identifier_list -> identifier : ['$1'].
identifier_list -> identifier_list ',' identifier : '$1'++['$3'].
     
number -> flonum : '$1'.
number -> hexnum : '$1'.
number -> octnum : '$1'.
number -> binnum : '$1'.
number -> decnum : '$1'.
number -> '0'    : {decnum,line('$1'),"0"}.
number -> '1'    : {decnum,line('$1'),"1"}.
     
sat_formula -> '0' : '$1'.
sat_formula -> '1' : '$1'.
sat_formula -> identifier : '$1'.
sat_formula -> identifier '.' identifier : {field,line('$2'),'$1','$3'}.
sat_formula -> '!' sat_formula : {'!',line('$1'),'$2'}.
sat_formula -> '(' sat_formula ')' : '$2'.
sat_formula -> 'timeout' '(' identifier ')' : {timeout,line('$1'),'$3'}.
sat_formula -> sat_formula '&&' sat_formula : {'&&',line('$2'),'$1','$3'}.
sat_formula -> sat_formula '||' sat_formula : {'||',line('$2'),'$1','$3'}.
sat_formula -> sat_formula '->' sat_formula : {'->',line('$2'),'$1','$3'}.
sat_formula -> sat_formula '<->' sat_formula : {'<->',line('$2'),'$1','$3'}.

pred_formula -> '0' : '$1'.
pred_formula -> '1' : '$1'.
pred_formula -> identifier : '$1'.
pred_formula -> identifier '(' identifier_list ')' :{pred,line('$1'),'$1','$3'}.
pred_formula -> identifier '.' identifier : {field,line('$2'),'$1','$3'}.
pred_formula -> '!' pred_formula : {'!',line('$1'),'$2'}.
pred_formula -> '(' pred_formula ')' : '$2'.
pred_formula -> pred_formula '&&' pred_formula : {'&&',line('$2'),'$1','$3'}.
pred_formula -> pred_formula '||' pred_formula : {'||',line('$2'),'$1','$3'}.
pred_formula -> pred_formula '->' pred_formula : {'->',line('$2'),'$1','$3'}.
pred_formula -> pred_formula '<->' pred_formula : {'<->',line('$2'),'$1','$3'}.
pred_formula -> 'ALL' identifier pred_formula : {'ALL',line('$1'),'$2','$3'}.
pred_formula -> 'SOME' identifier pred_formula : {'SOME',line('$1'),'$2','$3'}.

Erlang code.

line([H|_]) -> line(H);
line({_,Line}) -> Line;
line({_,Line,_}) -> Line;
line({_,Line,_,_}) -> Line.
