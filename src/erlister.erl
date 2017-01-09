%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Scan, Parse and translate state machines
%%% @end
%%% Created :  6 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister).

-compile(export_all).


start([File]) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    case erlister_scan:string(binary_to_list(Bin)) of
		{ok,Ts,_EndLine} ->
		    %% io:format("Tokens = ~p\n", [Ts]),
		    case erlister_parser:parse(Ts) of
			{error,{Ln,Mod,Message}} ->
			    io:format("~s:~w: ~s\n", 
				      [File,Ln,
				       apply(Mod,format_error,[Message])]),
			    halt(1);
			{ok,Machine} ->
			    io:format("machine ~p\n", [Machine]),
			    case lint(Machine) of
				{Machine1,[]} ->
				    io:format("machine' ~p\n", [Machine1]),
				    halt(0);
				{Machine1,ERR} ->
				    lists:foreach(
				      fun({Ln,Mod,Message}) ->
					      io:format("~s:~w: ~s\n", 
							[File,Ln,
							 apply(Mod,
							       format_error,
							       [Message])])
				      end, lists:keysort(1,ERR)),
				    io:format("machine' ~p\n", [Machine1]),
				    halt(1)
			    end
		    end;
		Error ->
		    io:format("error: ~p\n", [Error]),
		    halt(1)
	    end;
	Error ->
	    io:format("error: ~p\n", [Error]),
	    halt(1)
    end.

lint([{machine,Ln,{identifier,_Ln1,ID},[{submachines,Ms}|Misc],Machines}]) ->
    Sym = maps:new(),
    Es0 = [],
    {IN1,DEF1,OUT1,CLOCK1,Sym1,Es1} = lint_misc_0(Misc,[],[],[],[],Sym,Es0),
    {SUBMACHINES,Es2} = lint_submachines(Ms,[],Es1),
    {MACHINES0,Es3} = lint_submachine_list_0(Machines,[],SUBMACHINES,Es2),
    %% remove clocks?
    Sym2 = export_submachines(MACHINES0,Sym1),
    {MACHINES1,Es4} = lint_submachine_list(MACHINES0,[],Sym2,Es3),
    {{machine,Ln,ID,IN1,DEF1,OUT1,CLOCK1,MACHINES1},Es4};
lint([{machine,Ln,{identifier,_Ln1,ID},Misc,{states,States},{trans,Trans}}]) ->
    Sym = maps:new(),
    {STATES1,Sym1,Es1} = lint_states(States,[],Sym,[]),
    {IN1,DEF1,OUT1,CLOCK1,Sym2,Es2} = lint_misc_0(Misc,[],[],[],[],Sym1,Es1),

    {IN2,Es3} = lint_in(IN1,[],Sym2,Es2),
    {DEF2,Es4} = lint_def(DEF1,[],Sym2,Es3),
    {CLOCK2,Es5} = lint_clock(CLOCK1,[],Sym2,Es4),
    {OUT2,Es6} = lint_out(OUT1,[],Sym2,Es5),
    {TRANS1,Es7} = lint_trans(Trans,[],Sym2,Es6),
    {{machine,Ln,ID,IN2,DEF2,OUT2,CLOCK2,STATES1,TRANS1},Es7}.

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
    {IN1,DEF1,OUT1,CLOCKS1,Sym2,Es3} = lint_misc_0(Misc,[],[],[],[],Sym1,Es2),
    M = {submachine0,Ln,ID,IN1,DEF1,OUT1,CLOCKS1,STATES1,Trans,Sym2},
    lint_submachine_list_0(SubList,[M|Acc],SUBMACHINES,Es3);
lint_submachine_list_0([],Acc,_SUBMACHINES,Es) ->
    {Acc,Es}.

%% For each submachine export,
%% output and state as ID.outi and ID.state1

export_submachines([{submachine0,_Ln,ID,_IN,_DEF,OUT,_CLOCKS,
		     STATES,_Trans,_Sym}|Ms],Sym) ->
    Sym1 = lists:foldl(
	     fun({Out,Ln,_},Si) -> 
		     maps:put([ID,".",Out],{out,Ln},Si)
	     end, Sym, OUT),
    Sym2 = lists:foldl(
	     fun({State,Ln},Si) -> 
		     maps:put([ID,".",State],{state,Ln},Si)
	     end, Sym1, STATES),
    export_submachines(Ms, Sym2);
export_submachines([],Sym) ->
    Sym.

lint_submachine_list([{submachine0,Ln,ID,IN1,DEF1,OUT1,CLOCK1,
		       STATE,Trans,Sym0}|Ms],Acc,Sym,Es) ->
    %% merge "global" symbols with locals, should remove self! maybe ok?
    Sym1 = maps:merge(Sym0,Sym),
    {IN2,Es1} = lint_in(IN1,[],Sym1,Es),
    {DEF2,Es2} = lint_def(DEF1,[],Sym1,Es1),
    {CLOCK2,Es3} = lint_clock(CLOCK1,[],Sym1,Es2),
    {OUT2,Es4} = lint_out(OUT1,[],Sym1,Es3),
    {TRANS1,Es5} = lint_trans(Trans,[],Sym1,Es4),
    M = {submachine,Ln,ID,IN2,DEF2,OUT2,CLOCK2,STATE,TRANS1},
    lint_submachine_list(Ms,[M|Acc],Sym,Es5);
lint_submachine_list([],Acc,_Sym,Es) ->
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
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_states(Xs,[{ID,Ln}|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,{state,Ln},Sym),
	    lint_states(Xs,[{ID,Ln}|Acc],Sym1,Es)
    end;
lint_states([],Acc,Sym,Es) ->
    {lists:reverse(Acc),Sym,Es}.

%% lint_misc0, collect declaration and check uniqness
lint_misc_0([{in,Xs}|Ms],IN,DEF,OUT,CLOCK,Sym,Es) ->
    lint_misc_0(Ms,IN++Xs,DEF,OUT,CLOCK,Sym,Es);
lint_misc_0([{def,Xs}|Ms],IN,DEF,OUT,CLOCK,Sym,Es) ->
    lint_misc_0(Ms,IN,DEF++Xs,OUT,CLOCK,Sym,Es);
lint_misc_0([{out,Xs}|Ms],IN,DEF,OUT,CLOCK,Sym,Es) ->
    lint_misc_0(Ms,IN,DEF,OUT++Xs,CLOCK,Sym,Es);
lint_misc_0([{clocks,Xs}|Ms],IN,DEF,OUT,CLOCK,Sym,Es) ->
    lint_misc_0(Ms,IN,DEF,OUT,CLOCK++Xs,Sym,Es);
lint_misc_0([],IN,DEF,OUT,CLOCK,Sym,Es) ->
    {IN1,Sym1,Es1} = lint_in_0(IN,[],Sym,Es),
    {DEF1,Sym2,Es2} = lint_def_0(DEF,[],Sym1,Es1),
    {CLOCK1,Sym3,Es3} = lint_clock_0(CLOCK,[],Sym2,Es2),
    {OUT1,Sym4,Es4} = lint_out_0(OUT,[],Sym3,Es3),
    {IN1,DEF1,OUT1,CLOCK1,Sym4,Es4}.


lint_in_0([{identifier,Ln,ID}|Xs],Acc,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_in_0(Xs,[{ID,Ln,undefined}|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,{in,Ln},Sym),
	    lint_in_0(Xs,[{ID,Ln,undefined}|Acc],Sym1,Es)
    end;
lint_in_0([{'=',_Ln,{identifier,Ln,ID},Pred}|Xs],Acc,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_in_0(Xs,[{ID,Ln,Pred}|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,{in,Ln},Sym),
	    lint_in_0(Xs,[{ID,Ln,Pred}|Acc],Sym1,Es)
    end;
lint_in_0([],Acc,Sym,Es) ->
    {Acc,Sym,Es}.


%% a list of {ID,Ln,Sat} where Sat is parse tree

lint_def_0([{'=',_Ln,{identifier,Ln,ID},Sat}|Xs],Acc,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_def_0(Xs,[{ID,Ln,Sat}|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,{def,Ln},Sym),
	    lint_def_0(Xs,[{ID,Ln,Sat}|Acc],Sym1,Es)
    end;
lint_def_0([],Acc,Sym,Es) ->
    {Acc,Sym,Es}.

lint_clock_0([{clock,Ln,{identifier,_Ln,ID},{identifier,_Ln,NAME},
	       {Low,High},Step,Default}|Xs],Acc,Sym,Es) ->
    L = get_number(Low),
    H = get_number(High),
    S = get_number(Step),
    D = get_number(Default),
    Clock = {ID,Ln,{NAME,{L,H,S},D}},
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_clock_0(Xs,[Clock|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,{clock,Ln},Sym),
    	    lint_clock_0(Xs,[Clock|Acc],Sym1,Es)
    end;
lint_clock_0([],Acc,Sym,Es) ->
    {Acc,Sym,Es}.

lint_out_0([{'=',_Ln,{identifier,Ln,ID},Sat}|Xs],Acc,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,_} ->
	    E = {Ln,?MODULE,[ID, " is already defined"]},
	    lint_out_0(Xs,[{ID,Ln,Sat}|Acc],Sym,[E|Es]);
	error ->
	    Sym1 = maps:put(ID,{out,Ln},Sym),
	    lint_out_0(Xs,[{ID,Ln,Sat}|Acc],Sym1,Es)
    end;
lint_out_0([],Acc,Sym,Es) ->
    {Acc,Sym,Es}.

%% Check input
lint_in([{ID,Ln,undefined}|Xs],Acc,Sym,Es0) ->
    lint_in(Xs,[{ID,Ln,undefined}|Acc],Sym,Es0);
lint_in([{ID,Ln,Pred}|Xs],Acc,Sym,Es0) ->
    {Pred1,Es1} = 
	lint_pred(Pred,
		  fun(I={identifier,_Ln,_ID},Es) ->
			  %% global in variable
			  lint_in_variable(I,Sym,Es);
		     (I={field,_Ln,{identifier,_,_OBJ},{identifier,_,_ID}},Es)->
			  %% submachine out or state
			  lint_out_or_state_variable(I,Sym,Es);
		     ({timeout,Ln1,{identifier,_Ln,TID}},Es) ->
			  E = {Ln1,?MODULE,["timeout(",TID,")"
					    " can not be used in this section"]},
			  {{timeout,ID},[E|Es]}
		  end,Es0),
    lint_in(Xs,[{ID,Ln,Pred1}|Acc],Sym,Es1);
lint_in([],Acc,_Sym,Es) ->
    {Acc,Es}.

%% Check def
lint_def([{ID,Ln,Sat}|Xs],Acc,Sym,Es) ->
    {Sat1,Es1} = lint_dsat(Sat,Sym,Es),
    lint_def(Xs,[{ID,Ln,Sat1}|Acc],Sym,Es1);
lint_def([],Acc,_Sym,Es) ->
    {Acc,Es}.

lint_clock([Clock={_ID,_Ln,{_Name,{_L,_H,_S},_D}}|Xs],Acc,Sym,Es) ->
    lint_clock(Xs,[Clock|Acc],Sym,Es);
lint_clock([],Acc,_Sym,Es) ->
    {Acc,Es}.

lint_out([{ID,Ln,Sat}|Xs],Acc,Sym,Es) ->
    {Sat1,Es1} = lint_osat(Sat,Sym,Es),
    lint_out(Xs,[{ID,Ln,Sat1}|Acc],Sym,Es1);
lint_out([],Acc,_Sym,Es) ->
    {Acc,Es}.


lint_dsat(Form,Sym,Es0) ->
    lint_sat(Form,
	     fun(I={identifier,_Ln,_ID},Es) ->
		     lint_in_variable(I,Sym,Es);
		(I={field,_Ln,{identifier,_,_OBJ},{identifier,_,_ID}},Es) ->
		     lint_in_variable(I,Sym,Es);
		({timeout,Ln,{identifier,_Ln,ID}},Es) ->
		     E = {Ln,?MODULE,["timeout(",ID,")"
				      " can not be used in def section"]},
		     {{timeout,ID},[E|Es]}
	     end,Es0).

%% Lint  output SAT formual
%% allowed variables are IN,DEF and STATE
%%
lint_osat(Form,Sym,Es0) ->
    lint_sat(Form,
	     fun(I={identifier,_Ln,_ID},Es) ->
		     lint_in_def_state_variable(I,Sym,Es);
		(I={field,_Ln,{identifier,_,_OBJ},{identifier,_,_ID}},Es) ->
		     lint_in_def_state_variable(I,Sym,Es);
		({timeout,_Ln,{identifier,Ln,ID}},Es) ->
		     lint_timeout_(ID,Ln,Sym,Es)
	     end, Es0).

lint_trans([{{identifier,Ln,ID},TR}|Xs],Acc,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,{state,_Ln}} ->
	    {TR1,Es1} = lint_trans_list(TR,[],Sym,Es),
	    lint_trans(Xs,[{ID,Ln,TR1}|Acc],Sym,Es1);
	_ ->
	    E = {Ln,?MODULE,["state ", ID, " not declared"]},
	    {TR1,Es1} = lint_trans_list(TR,[],Sym,[E|Es]),
	    lint_trans(Xs,[{ID,Ln,TR1}|Acc],Sym,Es1)
    end;
lint_trans([],Acc,_Sym,Es) ->
    {Acc,Es}.

lint_trans_list([{{identifier,Ln,ID},Sat,{start,START}}|TR],Acc,Sym,Es) ->
    Es1 = case maps:find(ID,Sym) of
	      {ok,{state,_}} -> Es;
	      {ok,{_,_}} ->
		  [{Ln,?MODULE,[ID, " is not a state"]}|Es];
	      error -> 
		  [{Ln,?MODULE,["state ", ID, " not declared"]}|Es]
	  end,
    {Sat1,Es2} = lint_tsat(Sat,Sym,Es1),
    {START1,Es3} = lint_start(START,[],Sym,Es2),
    lint_trans_list(TR,[{ID,Ln,Sat1,START1}|Acc],Sym,Es3);
lint_trans_list([],Acc,_Sym,Es) ->
    {Acc,Es}.

lint_start([{identifier,Ln,ID}|START],Acc,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,{clock,_}} ->
	    lint_start(START,[{ID,Ln}|Acc],Sym,Es);
	_ ->
	    E = {Ln,?MODULE,["start ", ID, " is not a clock"]},
	    lint_start(START,[{ID,Ln}|Acc],Sym,[E|Es])
    end;
lint_start([],Acc,_Sym,Es) ->
    {Acc,Es}.

%% 
%% Lint transitions formulas
%%  allowed variables are 'in' parameters AND 'def' defined values
%%  also timeout expressions are allowed.

lint_tsat(Form,Sym,Es0) ->
    lint_sat(Form,
	     fun({identifier,Ln,ID},Es) ->
		     lint_in_def_variable_(ID,Ln,Sym,Es);
		({timeout,_Ln,{identifier,Ln,ID}},Es) ->
		     lint_timeout_(ID,Ln,Sym,Es);
		({field,Ln,{identifier,_Ln1,OBJ},{identifier,_Ln2,ID}},Es) ->
		     E = {Ln,?MODULE,["variable ",OBJ,".",ID,
				      " can not be used in trans section"]},
		     {{in,[OBJ,".",ID]},[E|Es]}
	     end,Es0).

lint_in_variable({identifier,Ln,ID},Sym,Es) ->
    lint_in_variable_(ID,Ln,Sym,Es);
lint_in_variable({field,Ln,{identifier,_,OBJ},{identifier,_,ID}},Sym,Es) ->
    lint_in_variable_([OBJ,".",ID],Ln,Sym,Es).

lint_in_variable_(ID,Ln,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,{in,_}} ->
	    {{in,ID},Es};
	_ ->
	    E = {Ln,?MODULE,["variable ", ID, " must be an in parameter"]},
	    {{in,ID},[E|Es]}
    end.


lint_out_or_state_variable({identifier,Ln,ID},Sym,Es) ->
    lint_out_or_state_variable_(ID,Ln,Sym,Es);
lint_out_or_state_variable({field,Ln,{identifier,_,OBJ},{identifier,_,ID}},
			   Sym,Es) ->
    lint_out_or_state_variable_([OBJ,".",ID],Ln,Sym,Es).

lint_out_or_state_variable_(ID,Ln,Sym,Es) ->
    case maps:find(ID,Sym) of
	{ok,{out,_}} ->
	    {{out,ID},Es};
	{ok,{state,_}} ->
	    {{state,ID},Es};
	_ ->
	    E = {Ln,?MODULE,["variable ", ID, " must be a state or an out parameter"]},
	    {{out,ID},[E|Es]}
    end.

lint_in_def_state_variable({identifier,Ln,ID},Sym,Es) ->
    lint_in_def_state_variable_(ID,Ln,Sym,Es);
lint_in_def_state_variable({field,Ln,{identifier,_,OBJ},{identifier,_,ID}},
			   Sym,Es) ->
    lint_in_def_state_variable_([OBJ,".",ID],Ln,Sym,Es).

lint_in_def_state_variable_(ID,Ln,Sym,Es) ->
    case maps:find(ID, Sym) of
	{ok,{in,_}} -> {{in,ID},Es};
	{ok,{def,_}} -> {{def,ID},Es};
	{ok,{state,_}} -> {{state,ID},Es};
	_ ->
	    E = {Ln,?MODULE,["variable ", ID,
			     " must be a state,in or def parameter"]},
	    {{in,ID},[E|Es]}
    end.

lint_in_def_variable_(ID,Ln,Sym,Es) ->
    case maps:find(ID, Sym) of
	{ok,{in,_}} -> {{in,ID},Es};
	{ok,{def,_}} -> {{def,ID},Es};
	_ ->
	    E = {Ln,?MODULE,["variable ", ID, 
			     " must be in or def parameter"]},
	    {{in,ID},[E|Es]}
    end.

lint_timeout_(ID,Ln,Sym,Es) ->
    case maps:find(ID, Sym) of
	{ok,{clock,_}} -> {{timeout,ID},Es};
	_ ->
	    E = {Ln,?MODULE,["variable ", ID, " must be a clock"]},
	    {{timeout,ID},[E|Es]}
    end.


%% lint SAT formula, for identifiers, timeout a callback is used
lint_sat(I={identifier,_Ln,_ID},Lookup,Es) ->
    Lookup(I,Es);
lint_sat(I={field,_Ln,_OBJ,_ID},Lookup,Es) ->
    Lookup(I,Es);
lint_sat(I={timeout,_Ln,{identifier,_Ln,_ID}},Lookup,Es) ->
    Lookup(I,Es);
lint_sat(F={'0',_Ln},_Lookup,Es) -> {F,Es};
lint_sat(F={'1',_Ln},_Lookup,Es) -> {F,Es};
lint_sat({Op,_Ln,R,L},Lookup,Es) when 
      Op =:= '&&'; Op =:= '||'; Op =:= '->'; Op =:= '<->' ->
    {L1,Es1} = lint_sat(L,Lookup,Es),
    {R1,Es2} = lint_sat(R,Lookup,Es1),
    {{Op,L1,R1},Es2};
lint_sat({Op,_Ln,M},Lookup,Es) when 
      Op =:= '!' ->
    {M1,Es1} = lint_sat(M,Lookup,Es),
    {{Op,M1},Es1}.

%% lint PRED formula, for identifiers, timeout a callback is used
lint_pred(I={identifier,_Ln,_ID},Lookup,Es) ->
    Lookup(I,Es);
lint_pred(I={field,_Ln,_OBJ,_ID},Lookup,Es) ->
    Lookup(I,Es);
lint_pred(I={timeout,_Ln,{identifier,_Ln,_ID}},Lookup,Es) ->
    Lookup(I,Es);
lint_pred(F={'0',_Ln},_Lookup,Es) -> {F,Es};
lint_pred(F={'1',_Ln},_Lookup,Es) -> {F,Es};
lint_pred({'ALL',_Ln,{identifier,_,X},F},Lookup,Es) ->
    {F,Es1} = lint_pred(F,Lookup,Es),
    {{'ALL',{var,X},F},Es1};
lint_pred({'SOME',_Ln,{identifier,_,X},F},Lookup,Es) ->
    {F,Es1} = lint_pred(F,Lookup,Es),
    {{'SOME',{var,X},F},Es1};
lint_pred({Op,_Ln,R,L},Lookup,Es) when 
      Op =:= '&&'; Op =:= '||'; Op =:= '->'; Op =:= '<->' ->
    {L1,Es1} = lint_pred(L,Lookup,Es),
    {R1,Es2} = lint_pred(R,Lookup,Es1),
    {{Op,L1,R1},Es2};
lint_pred({Op,_Ln,M},Lookup,Es) when 
      Op =:= '!' ->
    {M1,Es1} = lint_pred(M,Lookup,Es),
    {{Op,M1},Es1}.
    

get_number({decnum,_Ln,Ds}) -> list_to_integer(Ds,10);
get_number({hexnum,_Ln,[$0,$x|Ds]}) -> list_to_integer(Ds,16);
get_number({octnum,_Ln,[$0|Ds]}) -> list_to_integer(Ds,8);
get_number({binnum,_Ln,[$0,$b|Ds]}) -> list_to_integer(Ds,2);
get_number({flonum,_Ln,Ds}) -> list_to_float(Ds);
get_number({'1',_Ln}) -> 1;
get_number({'0',_Ln}) -> 0.

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.
