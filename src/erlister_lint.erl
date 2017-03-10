%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Erlister linter
%%% @end
%%% Created : 18 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister_lint).

-export([machine/1]).
-export([format_error/1]).

-include("../include/erlister.hrl").

machine(M) ->
    %% FIXME: we need to specify parameter definitions better,
    %% this is a test
    co_objdef:start_link(),
    co_objdef:load(pdb),
    machine_(M).

machine_([{machine,Ln,{identifier,_Ln1,ID},[{submachines,Ms}|Misc],Machines}]) ->
    Sym = maps:new(),
    Es0 = [],
    {IN1,PAR1,DEF1,OUT1,CLOCK1,Sym1,Es1} = lint_misc_0(Misc,[],[],[],[],[],Sym,Es0),
    {SUBMACHINES,Es2} = lint_submachines(Ms,[],Es1),
    {MACHINES0,Es3} = lint_submachine_list_0(Machines,[],SUBMACHINES,Es2),
    %% remove clocks?
    Sym2 = export_submachines(MACHINES0,Sym1),
    {MACHINES1,Es4} = lint_submachine_list(MACHINES0,[],Sym2,SUBMACHINES,Es3),

    {IN2,Es5} = lint_in(IN1,[],Sym2,ID,[],Es4),
    {PAR2,Es51} = lint_param(PAR1,[],Sym2,Es5),
    {DEF2,Es6} = lint_def(DEF1,[],Sym2,Es51),
    {CLOCK2,Es7} = lint_clock(CLOCK1,[],Sym2,Es6),
    %% Fixme: remove global in variables?
    {OUT2,Es8} = lint_out(OUT1,[],Sym2,ID,[],Es7),
    {#machine{line=Ln,name=ID,in=IN2,param=PAR2,def=DEF2,out=OUT2,clocks=CLOCK2,
	      submachines=SUBMACHINES,machines=MACHINES1},Es8};
machine_([{machine,Ln,{identifier,_Ln1,ID},Misc,{states,States},{trans,Trans}}]) ->
    Sym = maps:new(),
    {STATES1,Sym1,Es1} = lint_states(States,[],Sym,[]),
    {IN1,PAR1,DEF1,OUT1,CLOCK1,Sym2,Es2} = lint_misc_0(Misc,[],[],[],[],[],Sym1,Es1),
    {IN2,Es3} = lint_in(IN1,[],Sym2,ID,[],Es2),
    {PAR2,Es31} = lint_param(PAR1,[],Sym2,Es3),
    {DEF2,Es4} = lint_def(DEF1,[],Sym2,Es31),
    {CLOCK2,Es5} = lint_clock(CLOCK1,[],Sym2,Es4),
    {OUT2,Es6} = lint_out(OUT1,[],Sym2,ID,[],Es5),
    %% FIXME: check that all state has rules
    {TRANS1,Es7} = lint_trans(Trans,[],Sym2,Es6),
    {#machine{line=Ln,name=ID,in=IN2,param=PAR2,def=DEF2,out=OUT2,clocks=CLOCK2,
	      states=STATES1,trans=TRANS1},Es7}.

%% First scan of submachines, check unique submachine name,
%% check uniqueness for in,def,out,state.
lint_submachine_list_0([{submachine,Ln,{identifier,_Ln,ID},Misc,
		       {states,States},{trans,Trans}}|SubList],
		     Acc,SUBMACHINES,Es) ->
    Es1 = case lists:member(ID, SUBMACHINES) of
	      false ->
		  E = {Ln,?MODULE,["submachine ", ID, " not declared"]},
		  [E|Es];
	       true ->
		   Es
	  end,
    Sym = maps:new(),
    {STATES1,Sym1,Es2} = lint_states(States,[],Sym,Es1),
    {IN1,PAR1,DEF1,OUT1,CLOCKS1,Sym2,Es3} = lint_misc_0(Misc,[],[],[],[],[],Sym1,Es2),
    M = {submachine0,Ln,ID,IN1,PAR1,DEF1,OUT1,CLOCKS1,STATES1,Trans,Sym2},
    lint_submachine_list_0(SubList,[M|Acc],SUBMACHINES,Es3);
lint_submachine_list_0([],Acc,_SUBMACHINES,Es) ->
    {Acc,Es}.

%% For each submachine export,
%% output and state as ID.outi and ID.state1

export_submachines([{submachine0,_Ln,ID,_IN,_PAR,_DEF,OUT,_CLOCKS,
		     STATES,_Trans,_Sym}|Ms],Sym) ->
    Sym1 = lists:foldl(
	     fun(V=#var{id=Out},Si) -> 
		     maps:put([ID,".",Out],V,Si)
	     end, Sym, OUT),
    Sym2 = lists:foldl(
	     fun(S=#state{id=State},Si) ->
		     maps:put([ID,".",State],S,Si)
	     end, Sym1, STATES),
    export_submachines(Ms, Sym2);
export_submachines([],Sym) ->
    Sym.

lint_submachine_list([{submachine0,Ln,ID,IN1,PAR1,DEF1,OUT1,CLOCK1,
		       STATE,Trans,Sym0}|Ms],Acc,Sym,SUBMACHINES,Es) ->
    %% merge "global" symbols with locals, should remove self! maybe ok?
    Sym1 = maps:merge(Sym0,Sym),
    {IN2,Es1} = lint_in(IN1,[],Sym1,ID,SUBMACHINES,Es),
    {PAR2,Es11} = lint_param(PAR1,[],Sym1,Es1),
    {DEF2,Es2} = lint_def(DEF1,[],Sym1,Es11),
    {CLOCK2,Es3} = lint_clock(CLOCK1,[],Sym1,Es2),
    {OUT2,Es4} = lint_out(OUT1,[],Sym1,ID,SUBMACHINES,Es3),
    %% FIXME: check that all state has rules
    {TRANS1,Es5} = lint_trans(Trans,[],Sym1,Es4),
    M = #machine{line=Ln,name=ID,in=IN2,param=PAR2,def=DEF2,out=OUT2,
		 clocks=CLOCK2,
		 states=STATE,trans=TRANS1},
    lint_submachine_list(Ms,[M|Acc],Sym,SUBMACHINES,Es5);
lint_submachine_list([],Acc,_Sym,_SUBMACHINES,Es) ->
    {Acc,Es}.

lint_submachines([{identifier,Ln,ID}|Xs],SUBMACHINES,Es) ->
    case lists:member(ID, SUBMACHINES) of
	true ->
	    E = {Ln,?MODULE,["machine ", ID, " is already defined"]},
	    lint_submachines(Xs,SUBMACHINES,[E|Es]);
	false ->
	    lint_submachines(Xs, [ID|SUBMACHINES], Es)
    end;
lint_submachines([],SUBMACHINES,Es) ->
    {lists:reverse(SUBMACHINES),Es}.


lint_states([{identifier,Ln,ID}|Xs],Acc,Sym,Es) ->
    S = #state { id=ID, line=Ln },
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_states(Xs,[S|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,S,Sym),
	    lint_states(Xs,[S|Acc],Sym1,Es)
    end;
lint_states([],Acc,Sym,Es) ->
    {lists:reverse(Acc),Sym,Es}.

%% lint_misc0, collect declaration and check uniqness
lint_misc_0([{in,Type,Xs}|Ms],IN,PAR,DEF,OUT,CLOCK,Sym,Es) ->
    lint_misc_0(Ms,IN++[{Type,X}||X<-Xs],PAR,DEF,OUT,CLOCK,Sym,Es);
lint_misc_0([{param,Type,Xs}|Ms],IN,PAR,DEF,OUT,CLOCK,Sym,Es) ->
    lint_misc_0(Ms,IN,PAR++[{Type,X}||X<-Xs],DEF,OUT,CLOCK,Sym,Es);
lint_misc_0([{def,Type,Xs}|Ms],IN,PAR,DEF,OUT,CLOCK,Sym,Es) ->
    lint_misc_0(Ms,IN,PAR,DEF++[{Type,X}||X<-Xs],OUT,CLOCK,Sym,Es);
lint_misc_0([{out,Type,Xs}|Ms],IN,PAR,DEF,OUT,CLOCK,Sym,Es) ->
    lint_misc_0(Ms,IN,PAR,DEF,OUT++[{Type,X}||X<-Xs],CLOCK,Sym,Es);
lint_misc_0([{clocks,Xs}|Ms],IN,PAR,DEF,OUT,CLOCK,Sym,Es) ->
    lint_misc_0(Ms,IN,PAR,DEF,OUT,CLOCK++Xs,Sym,Es);
lint_misc_0([],IN,PAR,DEF,OUT,CLOCK,Sym,Es) ->
    {IN1,Sym1,Es1} = lint_in_0(IN,[],Sym,Es),
    {PAR1,Sym11,Es11} = lint_param_0(PAR,[],Sym1,Es1),
    {DEF1,Sym2,Es2} = lint_def_0(DEF,[],Sym11,Es11),
    {CLOCK1,Sym3,Es3} = lint_clock_0(CLOCK,[],Sym2,Es2),
    {OUT1,Sym4,Es4} = lint_out_0(OUT,[],Sym3,Es3),
    {IN1,PAR1,DEF1,OUT1,CLOCK1,Sym4,Es4}.

lint_in_0([{Type,{identifier,Ln,ID}}|Xs],Acc,Sym,Es) ->
    V = #var{id=ID,line=Ln,type=Type,class=in},
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_in_0(Xs,[V|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,V,Sym),
	    lint_in_0(Xs,[V|Acc],Sym1,Es)
    end;
lint_in_0([{Type,{'=',_Ln,{identifier,Ln,ID},Expr}}|Xs],Acc,Sym,Es) ->
    V = #var{id=ID,line=Ln,type=Type,class=in,expr=Expr},
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_in_0(Xs,[V|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,V,Sym),
	    lint_in_0(Xs,[V|Acc],Sym1,Es)
    end;
lint_in_0([],Acc,Sym,Es) ->
    {Acc,Sym,Es}.


lint_param_0([{Type,{{identifier,Ln,ID},Num}}|Xs],Acc,Sym,Es) ->
    V = #var{id=ID,line=Ln,type=Type,class=param,expr=Num},
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_param_0(Xs,[V|Acc],Sym,[E|Es]);
	error when Num =:= default ->
	    try co_objdef:lookup_object({index,list_to_atom(ID)}) of
		{Index,_,_} ->
		    V1 = V#var { expr=Index },
		    Sym1 = maps:put(ID,V1,Sym),
		    lint_param_0(Xs,[V1|Acc],Sym1,Es)
	    catch
		exit:_ ->
		    E = {Ln,?MODULE,
			 ["index for parameter ", ID, " not defined "]},
		    Sym1 = maps:put(ID,V,Sym),
		    lint_param_0(Xs,[V|Acc],Sym1,[E|Es])
	    end;
	error ->
	    Index = get_number(Num),
	    V1 = V#var { expr=Index },
	    Sym1 = maps:put(ID,V1,Sym),
	    lint_param_0(Xs,[V1|Acc],Sym1,Es)
    end;
lint_param_0([],Acc,Sym,Es) ->
    {Acc,Sym,Es}.

%% a list of {ID,Ln,Sat} where Sat is parse tree

lint_def_0([{Type,{'=',_Ln,{identifier,Ln,ID},Expr}}|Xs],Acc,Sym,Es) ->
    V = #var{id=ID,line=Ln,type=Type,class=def,expr=Expr},
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_def_0(Xs,[V|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,V,Sym),
	    lint_def_0(Xs,[V|Acc],Sym1,Es)
    end;
lint_def_0([],Acc,Sym,Es) ->
    {Acc,Sym,Es}.

lint_clock_0([{clock,Ln,{identifier,_Ln,ID},{identifier,_Ln,NAME},
	       {Low,High},Step,Default}|Xs],Acc,Sym,Es) ->
    L = get_number(Low),
    H = get_number(High),
    S = get_number(Step),
    D = get_number(Default),
    Clock = #clock{id=ID,name=NAME,line=Ln,range={L,H,S},default=D},
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_clock_0(Xs,[Clock|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,Clock,Sym),
    	    lint_clock_0(Xs,[Clock|Acc],Sym1,Es)
    end;
lint_clock_0([],Acc,Sym,Es) ->
    {Acc,Sym,Es}.

lint_out_0([{Type,{'=',_Ln,{identifier,Ln,ID},Expr}}|Xs],Acc,Sym,Es) ->
    V = #var{id=ID,line=Ln,type=Type,class=out,expr=Expr},
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_out_0(Xs,[V|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,V,Sym),
	    lint_out_0(Xs,[V|Acc],Sym1,Es)
    end;
lint_out_0([],Acc,Sym,Es) ->
    {Acc,Sym,Es}.

%% Check input
lint_in([V=#var{expr=undefined}|Xs],Acc,Sym,Mid,SUBMACHINES,Es0) ->
    lint_in(Xs,[V|Acc],Sym,Mid,SUBMACHINES,Es0);
lint_in([V=#var{expr=Expr}|Xs],Acc,Sym,Mid,SUBMACHINES,Es0) ->
    {Expr1,Es1} = 
	expr(Expr,in,V#var.type,
	     fun Lookup(I={identifier,_Ln,_ID},_Vs,Es) ->
		     %% global in variable
		     lint_in_variable(I,Sym,Es);
		 Lookup(I={field,_Ln,_,_},_Vs,Es)->
		     %% submachine out or state
		     lint_out_or_state_variable(I,Sym,Mid,SUBMACHINES,Es);
		 Lookup({pred,_Ln,{identifier,_,P},Args},Vs,Es) ->
		     {As,Es1} = pred_args(Args,[],Lookup,Vs,Es),
		     {{pred,P,As},Es1};
		 Lookup({timeout,Ln1,{identifier,_Ln,TID}},_Vs,Es) ->
		     E = {Ln1,?MODULE,["timeout(",TID,")"
				       " can not be used in this section"]},
		     {{timeout,TID},[E|Es]}
	     end,[],Es0),
    lint_in(Xs,[V#var{expr=Expr1}|Acc],Sym,Mid,SUBMACHINES,Es1);
lint_in([],Acc,_Sym,_Mid,_SUBMACHINES,Es) ->
    {Acc,Es}.

%% Check param
lint_param([V=#var{id=_ID,line=_Ln,expr=_Expr}|Xs],Acc,Sym,Es) ->
    lint_param(Xs,[V|Acc],Sym,Es);
lint_param([],Acc,_Sym,Es) ->
    {Acc,Es}.

%% Lint param
lint_def([V=#var{expr=Expr}|Xs],Acc,Sym,Es) ->
    {Expr1,Es1} = lint_dsat(Expr,V#var.type,Sym,Es),
    lint_def(Xs,[V#var{expr=Expr1}|Acc],Sym,Es1);
lint_def([],Acc,_Sym,Es) ->
    {Acc,Es}.

lint_clock([Clock=#clock{}|Xs],Acc,Sym,Es) ->
    lint_clock(Xs,[Clock|Acc],Sym,Es);
lint_clock([],Acc,_Sym,Es) ->
    {Acc,Es}.

%% Lint  output SAT formual
%% For submachines out variables are
%%   local IN, local DEF, local STATE
%% For machine variables are
%%   global IN, global DEF, STATE and submachine OUT, submachine STATE
%%
lint_out([V=#var{expr=Expr}|Xs],Acc,Sym,Mid,SUBMACHINES,Es0) ->
    {Expr1,Es1} =
	expr(Expr,out,V#var.type,
	     fun(I={identifier,_Ln,_ID},_Vs,Es) ->
		     %% global IN, DEF or STATE
		     lint_in_def_state_variable(I,Sym,Es);
		(I={field,_Ln,_,_},_Vs,Es) ->
		     %% sibling STATE,OUT?
		     lint_out_or_state_variable(I,Sym,Mid,SUBMACHINES,Es);
		({pred,Ln,{identifier,_,P},Args},_Vs,Es) ->
		     E = {Ln,?MODULE,["predicate ",P,
				      " is not allowed in this section"]},
		     {{pred,P,Args},[E|Es]};
		({timeout,_Ln,{identifier,Ln,ID}},_Vs,Es) ->
		     lint_timeout_(ID,Ln,Sym,Es)
	     end, [], Es0),
    lint_out(Xs,[V#var{expr=Expr1}|Acc],Sym,Mid,SUBMACHINES,Es1);
lint_out([],Acc,_Sym,_Mid,_SUBMACHINES,Es) ->
    {Acc,Es}.

lint_dsat(Form,Type,Sym,Es0) ->
    expr(Form,def,Type,
	 fun(_I={identifier,Ln,ID},_Vs,Es) ->
		 lint_in_def_variable_(ID,Ln,Sym,Es);
	    (I={field,_Ln,_,_},_Vs,Es) ->
		 lint_in_variable(I,Sym,Es);
	    ({pred,Ln,{identifier,_,P},Args},_Vs,Es) ->
		 E = {Ln,?MODULE,["predicate ",P,
				  " is not allowed in this section"]},
		 {{pred,P,Args},[E|Es]};
	    ({timeout,Ln,{identifier,_Ln,ID}},_Vs,Es) ->
		 E = {Ln,?MODULE,["timeout(",ID,")"
				  " can not be used in def section"]},
		 {{timeout,ID},[E|Es]}
	 end,[],Es0).

lint_trans([{{identifier,Ln,ID},TR}|Xs],Acc,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,#state{}} ->
	    {TR1,Es1} = lint_trans_list(TR,[],Sym,Es),
	    lint_trans(Xs,[{ID,Ln,TR1}|Acc],Sym,Es1);
	_ ->
	    E = {Ln,?MODULE,["state ", ID, " not declared"]},
	    {TR1,Es1} = lint_trans_list(TR,[],Sym,[E|Es]),
	    lint_trans(Xs,[{ID,Ln,TR1}|Acc],Sym,Es1)
    end;
lint_trans([],Acc,_Sym,Es) ->
    {Acc,Es}.

lint_trans_list([{{identifier,Ln,ID},Sat,Start,Action}|TR],Acc,Sym,Es) ->
    Es1 = case maps:find(ID,Sym) of
	      {ok,#state{}} -> Es;
	      {ok,_} ->
		  [{Ln,?MODULE,[ID, " is not a state"]}|Es];
	      error -> 
		  [{Ln,?MODULE,["state ", ID, " not declared"]}|Es]
	  end,
    {Sat1,Es2} = lint_tsat(Sat,Sym,Es1),
    {START1,Es3} = lint_start(Start,[],Sym,Es2),
    {ACTION1,Es4} = lint_action(Action,[],#{},Es3),
    lint_trans_list(TR,[{ID,Ln,Sat1,START1,ACTION1}|Acc],Sym,Es4);
lint_trans_list([],Acc,_Sym,Es) ->
    {Acc,Es}.

lint_start([{identifier,Ln,ID}|START],Acc,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,#clock{}} ->
	    lint_start(START,[{ID,Ln}|Acc],Sym,Es);
	_ ->
	    E = {Ln,?MODULE,["start ", ID, " is not a clock"]},
	    lint_start(START,[{ID,Ln}|Acc],Sym,[E|Es])
    end;
lint_start([],Acc,_Sym,Es) ->
    {Acc,Es}.

lint_action([{decl,Ln,Type,{identifier,_Ln,Var},Init}|As],Acc,Sym,Es) ->
    {Init1, Es1} = lint_action_expr(Init,Type,Sym,Es),
    V = #var{id=Var,line=Ln,type=Type,class=var,expr=Init1},
    case maps:find(Var,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[Var, " is already declared"]},
	    lint_action(As,[{decl,V}|Acc],Sym,[E|Es1]);
	error ->
	    Sym1 = maps:put(Var,V,Sym),
	    lint_action(As,[{decl,V}|Acc],Sym1,Es1)
    end;
lint_action([{store,Ln,{identifier,_Ln,Var},Expr}|As],Acc,Sym,Es) ->
    case maps:find(Var,Sym) of
	error ->
	    V = #var{id=Var,line=Ln,type=integer32},
	    {Expr1, Es1} = lint_action_expr(Expr,integer32,Sym,Es),
	    E = {Ln,?MODULE,[Var, " is not declared"]},
	    lint_action(As,[{store,V,Expr1}|Acc],Sym,[E|Es1]);
	{ok,V} ->
	    {Expr1, Es1} = lint_action_expr(Expr,V#var.type,Sym,Es),
	    lint_action(As,[{store,V,Expr1}|Acc],Sym,Es1)
    end;
lint_action([{call,Ln,{identifier,_Ln,Func},Args}|As],Acc,Sym,Es0) ->
    {Call,Es1} = lint_call(Func,Ln,Args,Sym,Es0),
    lint_action(As,[Call|Acc],Sym,Es1);
lint_action([],Acc,_Sym,Es) -> 
    {lists:reverse(Acc),Es}.

lint_call(Func, Ln, Args, Sym, Es0) ->
    case find_signature(Func) of
	false ->
	    E = {Ln,?MODULE,[Func," is not a defined function"]},
	    {{call,Func,Args},[E|Es0]};
	{_Ret,F,ATypes} ->
	    case length(ATypes) =:= length(Args) of
		true ->
		    {Args1,Es1} = lint_action_expr_list(Args,ATypes,Sym,Es0),
		    {{call,F,Args1},Es1};
		false ->
		    E = {Ln,?MODULE,["wrong number of arguments to ", Func]},
		    {Args1,Es1} = lint_action_expr_list(Args,ATypes,Sym,Es0),
		    {{call,F,Args1},[E|Es1]}
	    end
    end.

%%
lint_action_expr_list([Expr|Exprs],[Type|Types],Sym,Es0) ->
    {E1,Es1} = lint_action_expr(Expr,Type,Sym,Es0),
    {Exprs1,Es2} = lint_action_expr_list(Exprs,Types,Sym,Es1),
    {[E1|Exprs1],Es2};
lint_action_expr_list([Expr|Exprs],[],Sym,Es0) -> %% error case
    {E1,Es1} = lint_action_expr(Expr,integer32,Sym,Es0),
    {Exprs1,Es2} = lint_action_expr_list(Exprs,[],Sym,Es1),
    {[E1|Exprs1],Es2};
lint_action_expr_list([],_Types,_Sym,Es) ->
    {[],Es}.

lint_action_expr(Expr,Type,Sym,Es0) ->
    expr(Expr, action, Type,
	 fun(I={identifier,_Ln,_ID},_Vs,Es) ->
		 lint_local_variable(I,Sym,Es);
	    ({field,Ln,{identifier,_,OBJ},{identifier,_,ID}},_Vs,Es)->
		 E = {Ln,?MODULE,["variable ",OBJ,".",ID,
				  " can not be used in trans section"]},
		 {{in,[OBJ,".",ID]},[E|Es]};
	    ({pred,Ln,{identifier,_,P},Args},_Vs,Es) ->
		 %% pretend that pred are call for now
		 lint_call(P,Ln,Args,Sym,Es);
	    ({timeout,Ln1,{identifier,_Ln,TID}},_Vs,Es) ->
		 E = {Ln1,?MODULE,["timeout(",TID,")"
				   " can not be used in this section"]},
		 {{timeout,TID},[E|Es]}
	 end,[],Es0).


find_signature("param_fetch") -> {integer32,'param@',[unsigend16,unsigned8]};
find_signature("param_store") -> {void,'param!',[unsigend16,unsigned8,integer32]};
find_signature("uart_send")   -> {void,uart_send,[integer8]};
find_signature("uart_recv")   -> {integer8,uart_recv,[]};
find_signature("uart_avail")  -> {boolean,uart_avail,[]};
find_signature("timer_now")   -> {unsigned32,timer_now,[]};
find_signature("gpio_input")  -> {void,gpio_input,[integer32]};
find_signature("gpio_output") -> {void,gpio_output,[integer32]};
find_signature("gpio_set")    -> {void,gpio_set,[integer32]};
find_signature("gpio_clr")    -> {void,gpio_clr,[integer32]};
find_signature("gpio_get")    -> {integer32,gpio_get,[]};
find_signature("analog_send") -> {void,analog_send,[integer32,unsigned16]};
find_signature("analog_recv") -> {integer32,analog_recv,[unsigned16]};
find_signature("can_send")    -> {void,can_send,[unsigned16,unsigned8,integer32]};
find_signature(_) -> false.

%% 
%% Lint transitions formulas
%%  allowed variables are 'in' parameters AND 'def' defined values
%%  also timeout expressions are allowed.

%% Fixme: global in should not be accessible directly!!!

lint_tsat(Form,Sym,Es0) ->
    expr(Form,trans,boolean,
	 fun({identifier,Ln,ID},_Vs,Es) ->
		 lint_in_def_variable_(ID,Ln,Sym,Es);
	    ({timeout,_Ln,{identifier,Ln,ID}},_Vs,Es) ->
		 lint_timeout_(ID,Ln,Sym,Es);
	    ({pred,Ln,{identifier,_,P},Args},_Vs,Es) ->
		 E = {Ln,?MODULE,["predicate ",P,
				  " is not allowed in this section"]},
		 {{pred,P,Args},[E|Es]};
	    ({field,Ln,{identifier,_Ln1,OBJ},{identifier,_Ln2,ID}},_Vs,Es) ->
		 E = {Ln,?MODULE,["variable ",OBJ,".",ID,
				  " can not be used in trans section"]},
		 {{in,[OBJ,".",ID]},[E|Es]}
	 end,[],Es0).

lint_local_variable({identifier,Ln,ID},Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,#var{class=var,type=Type}} ->
	    {{var,ID,Type},Es};
	error ->
	    E = {Ln,?MODULE,["variable ", ID, " not declared"]},
	    {{var,ID,boolean},[E|Es]};
	{ok,#var{type=Type}} ->
	    E = {Ln,?MODULE,["variable ", ID, " must be a local variable"]},
	    {{var,ID,Type},[E|Es]}
    end.


lint_in_variable({identifier,Ln,ID},Sym,Es) ->
    lint_in_variable_(ID,Ln,Sym,Es);
lint_in_variable({field,Ln,{identifier,_,OBJ},{identifier,_,ID}},Sym,Es) ->
    lint_in_variable_([OBJ,".",ID],Ln,Sym,Es).

lint_in_variable_(ID,Ln,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,#var{class=in,type=Type}} ->
	    {{in,ID,Type},Es};
	_ ->
	    E = {Ln,?MODULE,["variable ", ID, " must be an in parameter"]},
	    {{in,ID,boolean},[E|Es]}
    end.


lint_out_or_state_variable({identifier,Ln,ID},Sym,Mid,SUBMACHINES,Es) ->
    lint_out_or_state_variable_(ID,Ln,Sym,Mid,SUBMACHINES,Es);
lint_out_or_state_variable({field,Ln,{identifier,_,OBJ},{identifier,_,ID}},
			   Sym,Mid,SUBMACHINES,Es) ->
    lint_out_or_state_variable_([OBJ,".",ID],Ln,Sym,Mid,SUBMACHINES,Es).

lint_out_or_state_variable_(ID,Ln,Sym,Mid,SUBMACHINES,Es) ->
    case maps:find(ID,Sym) of
	{ok,#var{class=out,type=Type}} ->
	    case ID of
		[M,".",_FLD] when SUBMACHINES =/= [] ->
		    case index(M, SUBMACHINES) < index(Mid,SUBMACHINES) of
			true ->
			    %% out1 refere to the current instance of the output
			    {{out1,ID,Type},Es};
			false ->
			    {{out,ID,Type},Es}
		    end;
		[_M,".",_FLD] ->
		    {{out1,ID,Type},Es};
		_ ->
		    {{out,ID,Type},Es}
	    end;
	{ok,#state{}} ->
	    {{state,ID},Es};
	_ ->
	    E = {Ln,?MODULE,["variable ", ID, " must be a state or an out parameter"]},
	    {{out,ID,boolean},[E|Es]}
    end.

lint_in_def_state_variable({identifier,Ln,ID},Sym,Es) ->
    lint_in_def_state_variable_(ID,Ln,Sym,Es);
lint_in_def_state_variable({field,Ln,{identifier,_,OBJ},{identifier,_,ID}},
			   Sym,Es) ->
    lint_in_def_state_variable_([OBJ,".",ID],Ln,Sym,Es).

lint_in_def_state_variable_(ID,Ln,Sym,Es) ->
    case maps:find(ID, Sym) of
	{ok,#var{class=in,type=Type}} -> {{in,ID,Type},Es};
	{ok,#var{class=def,type=Type}} -> {{def,ID,Type},Es};
	{ok,#var{class=param,type=Type,expr=Index}} -> 
	    {{param,ID,Index,Type},Es};
	{ok,#state{}} -> {{state,ID},Es};
	_ ->
	    E = {Ln,?MODULE,["variable ", ID,
			     " must be a state,in or def parameter"]},
	    {{in,ID,boolean},[E|Es]}
    end.

lint_in_def_variable_(ID,Ln,Sym,Es) ->
    case maps:find(ID, Sym) of
	{ok,#var{class=in,type=Type}} -> {{in,ID,Type},Es};
	{ok,#var{class=def,type=Type}} -> {{def,ID,Type},Es};
	{ok,#var{class=param,expr=Index,type=Type}} -> 
	    {{param,ID,Index,Type},Es};
	_ ->
	    E = {Ln,?MODULE,["variable ", ID, 
			     " must be in, param or def parameter"]},
	    {{in,ID,boolean},[E|Es]}
    end.

lint_timeout_(ID,Ln,Sym,Es) ->
    case maps:find(ID, Sym) of
	{ok,#clock{}} -> {{timeout,ID},Es};
	_ ->
	    E = {Ln,?MODULE,["variable ", ID, " must be a clock"]},
	    {{timeout,ID},[E|Es]}
    end.

%%
%% expr(Expr,Class,Type,Lookup,Es) -> {Expr',Es'}
%% Class = in|out|def(|trans)
%% Type  = boolean|unsigned|integer
%%
expr(I={identifier,_Ln,_ID},_Class,_Type,Lookup,Vs,Es) ->
    Lookup(I,Vs,Es);
expr(I={field,_Ln,_OBJ,_ID},_Class,_Type,Lookup,Vs,Es) ->
    Lookup(I,Vs,Es);
expr(I={timeout,_Ln,{identifier,_Ln,_ID}},_Class,_Type,Lookup,Vs,Es) ->
    Lookup(I,Vs,Es);
expr(P={pred,_Ln,{identifier,_,_ID},_Args},_Class,_Type,Lookup,Vs,Es) ->
    Lookup(P,Vs,Es);
expr({true,Ln},_Class,Type,_Lookup,_Vs,Es) ->
    if Type =/= boolean ->
	    const(1,Ln,Type,Es);
       true ->
	    {{const,true},Es}
    end;
expr({false,Ln},_Class,Type,_Lookup,_Vs,Es) ->
    if Type =/= boolean ->
	    const(0,Ln,Type,Es);
       true ->
	    {{const,false},Es}
    end;
expr({decnum,Ln,Ds},_Class,Type,_Lookup,_Vs,Es) -> 
    const(list_to_integer(Ds,10),Ln,Type,Es);
expr({hexnum,Ln,Ds},_Class,Type,_Lookup,_Vs,Es) -> 
    const(list_to_integer(Ds,16),Ln,Type,Es);
expr({octnum,Ln,Ds},_Class,Type,_Lookup,_Vs,Es) -> 
    const(list_to_integer(Ds,8),Ln,Type,Es);
expr({binnum,Ln,Ds},_Class,Type,_Lookup,_Vs,Es) -> 
    const(list_to_integer(Ds,2),Ln,Type,Es);
expr({flonum,Ln,Ds},_Class,Type,_Lookup,_Vs,Es) -> 
    const(list_to_float(Ds),Ln,Type,Es);
expr({'ALL',Ln,{identifier,_,X},F},Class,Type,Lookup,Vs,Es) ->
    if Class =:= in ->
	    {F1,Es1} = expr(F,Class,Type,Lookup,[{var,X}|Vs],Es),    
	    {{'ALL',{var,X},F1},Es1};
       true ->
	    {{'ALL',{var,X},F},
	     [{Ln,?MODULE,["quantifier not allowed"]}|Es]}
    end;
expr({'SOME',Ln,{identifier,_,X},F},Class,Type,Lookup,Vs,Es) ->
    if Class =:= in ->
	    {F1,Es1} = expr(F,Class,Type,Lookup,[{var,X}|Vs],Es),
	    {{'SOME',{var,X},F1},Es1};
       true ->
	    {{'SOME',{var,X},F},
	     [{Ln,?MODULE,["quantifier not allowed"]}|Es]}
    end;
expr({Op,_Ln,L,R},Class,_Type,Lookup,Vs,Es) when 
      Op =:= '&&'; Op =:= '||'; Op =:= '->'; Op =:= '<->' ->
    %% assert Type == boolean
    {L1,Es1} = expr(L,Class,boolean,Lookup,Vs,Es),
    {R1,Es2} = expr(R,Class,boolean,Lookup,Vs,Es1),
    {{Op,L1,R1},Es2};
expr({Op,_Ln,L,R},Class,_Type,Lookup,Vs,Es) when 
      Op =:= '<'; Op =:= '<='; Op =:= '>'; Op =:= '>='; 
      Op =:= '=='; Op =:= '!=' ->
    %% assert Type == boolean
    {L1,Es1} = expr(L,Class,integer,Lookup,Vs,Es),
    {R1,Es2} = expr(R,Class,integer,Lookup,Vs,Es1),
    {{Op,L1,R1},Es2};
expr({Op,_Ln,M},Class,_Type,Lookup,Vs,Es) when 
      Op =:= '!' ->
    %% assert Type == boolean
    {M1,Es1} = expr(M,Class,boolean,Lookup,Vs,Es),
    {{Op,M1},Es1};
expr({'?',_Ln,C,T,E},Class,Type,Lookup,Vs,Es) ->
    {C1,Es1} = expr(C,Class,boolean,Lookup,Vs,Es),    
    {T1,Es2} = expr(T,Class,Type,Lookup,Vs,Es1),
    {E1,Es3} = expr(E,Class,Type,Lookup,Vs,Es2),
    {{'?',C1,T1,E1},Es3};
expr({Op,_Ln,L,R},Class,_Type,Lookup,Vs,Es) when 
      Op =:= '+'; Op =:= '-'; Op =:= '*'; Op =:= '/'; Op =:= '%' ->
    %% assert Type == unsigned|integer
    {L1,Es1} = expr(L,Class,integer,Lookup,Vs,Es),
    {R1,Es2} = expr(R,Class,integer,Lookup,Vs,Es1),
    {{Op,L1,R1},Es2};
expr({Op,_Ln,M},Class,_Type,Lookup,Vs,Es) when 
      Op =:= '-' ->
    %% assert Type == unsigned|integer
    {M1,Es1} = expr(M,Class,integer,Lookup,Vs,Es),
    {{Op,M1},Es1}.

%% constant check
const(Const,_Ln,float,Es) when is_float(Const) ->
    {{const,Const}, Es};
const(Const,_Ln,float,Es) when is_integer(Const) ->
    {{const,float(Const)}, Es};
const(Const,Ln,Type,Es) when is_float(Const) ->
    const(trunc(Const),Ln,Type,Es);  %% FIXME?? error? warning?
const(Const,Ln,boolean,Es) ->
    Bool = (Const =/= 0),
    {{const,Bool},
     [{Ln,?MODULE,["constant is not a boolean"]}|Es]};
const(Const,Ln,unsigned8,Es) when Const < 0; Const > 16#ff ->
    {{const,Const},
     [{Ln,?MODULE,["constant not in unsigned8 range"]}|Es]};
const(Const,Ln,unsigned16,Es) when Const < 0; Const > 16#ffff ->
    {{const,Const},
     [{Ln,?MODULE,["constant not in unsigned16 range"]}|Es]};
const(Const,Ln,unsigned32,Es) when Const < 0; Const > 16#ffffffff ->
    {{const,Const},
     [{Ln,?MODULE,["constant not in unsigned32 range"]}|Es]};
const(Const,Ln,integer8,Es) when Const < -16#80; Const > 16#7f ->
    {{const,Const},
     [{Ln,?MODULE,["constant not in integer8 range"]}|Es]};
const(Const,Ln,integer16,Es) when Const < -16#8000; Const > 16#7fff ->
    {{const,Const},
     [{Ln,?MODULE,["constant not in integer16 range"]}|Es]};
const(Const,Ln,integer32,Es) when Const < -16#80000000; Const > 16#7fffffff ->
    {{const,Const},
     [{Ln,?MODULE,["constant not in integer32 range"]}|Es]};
const(Const,_Ln,_Type,Es) ->
    {{const,Const}, Es}.
    
pred_args([F|Fs],Acc,Lookup,Vs,Es) ->
    {F1,Es1} = case F of
		   {identifier,_Ln,X} ->
		       case lists:member({var,X},Vs) of
			   true ->
			       {{var,X},Es};
			   false ->
			       Lookup(F,Vs,Es)
		       end;
		   {decnum,_Ln,Ds} -> {{const,list_to_integer(Ds,10)},Es};
		   {hexnum,_Ln,Ds} -> {{const,list_to_integer(Ds,16)},Es};
		   {octnum,_Ln,Ds} -> {{const,list_to_integer(Ds,8)},Es};
		   {binnum,_Ln,Ds} -> {{const,list_to_integer(Ds,2)},Es};
		   {flonum,_Ln,Ds} -> {{const,list_to_float(Ds)},Es}
	       end,
    pred_args(Fs,[F1|Acc],Lookup,Vs,Es1);
pred_args([],Acc,_Lookup,_Vs,Es) ->
    {lists:reverse(Acc),Es}.

get_number({decnum,_Ln,Ds}) -> list_to_integer(Ds,10);
get_number({hexnum,_Ln,[$0,$x|Ds]}) -> list_to_integer(Ds,16);
get_number({octnum,_Ln,[$0|Ds]}) -> list_to_integer(Ds,8);
get_number({binnum,_Ln,[$0,$b|Ds]}) -> list_to_integer(Ds,2);
get_number({flonum,_Ln,Ds}) -> list_to_float(Ds).

index(Key,Keys) ->
    index_(Key,Keys,1).

index_(Key,[Key|_],I) -> I;
index_(Key,[_|Keys],I) -> index_(Key,Keys,I+1).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.
