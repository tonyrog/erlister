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
			    Machine1 = lint(Machine),
			    io:format("machine' ~p\n", [Machine1]),
			    halt(0)
		    end;
		Error ->
		    io:format("error: ~p\n", [Error]),
		    halt(1)
	    end;
	Error ->
	    io:format("error: ~p\n", [Error]),
	    halt(1)
    end.

lint([{machine,_Ln,{identifier,_Ln1,ID},[{submachines,Ms}|Misc],Machines}]) ->
    {IN,DEF,OUT,CLOCKS,ERR1} = lint_misc(Misc,[],[],[],[],[],[]),
    {SUBMACHINES,ERR2} = lint_submachines(Ms,[],ERR1),
    {MACHINES,ERR3} = lint_submachine_list(Machines,[],SUBMACHINES,
					   IN,DEF,OUT,CLOCKS,ERR2),
    {{machine,ID,IN,DEF,OUT,CLOCKS,MACHINES},ERR3};
lint([{machine,_Ln,{identifier,_Ln1,ID},Misc,{states,States},{trans,Trans}}]) ->
    {STATES1,ERR1} = lint_states(States,[],[]),
    {IN,DEF,OUT,CLOCKS,ERR2} = lint_misc(Misc,[],[],[],[],STATES1,ERR1),
    {TRANS1,ERR3} = lint_trans(Trans,[],STATES1,IN,DEF,CLOCKS,ERR2),
    {{machine,ID,IN,DEF,OUT,CLOCKS,STATES1,TRANS1},ERR3}.


lint_submachine_list([{submachine,Ln,{identifier,_Ln,ID},Misc,
		       {states,States},{trans,Trans}}|SubList],
		     Acc,SUBMACHINES,IN,DEF,OUT,CLOCKS,ERR) ->
    ERR1 = case lists:member(ID, SUBMACHINES) of
	       false ->
		   E = {Ln,erlister,["submachine ", ID, " not declared"]},
		   [E|ERR];
	       true ->
		   ERR
	   end,
    {STATES1,ERR2} = lint_states(States,[],ERR1),
    {IN1,DEF1,OUT1,CLOCKS1,ERR3} = lint_misc(Misc,[],[],[],[],STATES1,ERR2),
    {TRANS1,ERR4} = lint_trans(Trans,[],STATES1,IN1,DEF1,CLOCKS1,ERR3),
    lint_submachine_list(SubList,[{submachine,ID,IN1,DEF1,OUT1,CLOCKS1,STATES1,TRANS1}|Acc],SUBMACHINES,IN,DEF,OUT,CLOCKS,ERR4);
lint_submachine_list([],Acc,_SUBMACHINES,_IN,_DEF,_OUT,_CLOCKS,ERR) ->
    {Acc,ERR}.

lint_submachines([{identifier,Ln,ID}|Xs],SUBMACHINES,ERR) ->
    case lists:member(ID, SUBMACHINES) of
	true ->
	    E = {Ln,erlister,["machine ", ID, " is already defined"]},
	    lint_submachines(Xs,SUBMACHINES,[E|ERR]);
	false ->
	    lint_submachines(Xs, [ID|SUBMACHINES], ERR)
    end;
lint_submachines([],SUBMACHINES,ERR) ->
    {lists:reverse(SUBMACHINES),ERR}.


lint_states([{identifier,Ln,ID}|Xs],STATES,ERR) ->
    case lists:member(ID, STATES) of
	true ->
	    E = {Ln,erlister,["state ", ID, " is already defined"]},
	    lint_states(Xs,STATES,[E|ERR]);
	false ->
	    lint_states(Xs, [ID|STATES], ERR)
    end;
lint_states([],STATES,ERR) ->
    {lists:reverse(STATES),ERR}.

%% collect declarations
lint_misc([{in,Xs}|Ms],IN,DEF,OUT,CLOCKS,STATES,ERR) ->
    lint_misc(Ms,IN++Xs,DEF,OUT,CLOCKS,STATES,ERR);
lint_misc([{def,Xs}|Ms],IN,DEF,OUT,CLOCKS,STATES,ERR) ->
    lint_misc(Ms,IN,DEF++Xs,OUT,CLOCKS,STATES,ERR);
lint_misc([{out,Xs}|Ms],IN,DEF,OUT,CLOCKS,STATES,ERR) ->
    lint_misc(Ms,IN,DEF,OUT++Xs,CLOCKS,STATES,ERR);
lint_misc([{clocks,Xs}|Ms],IN,DEF,OUT,CLOCKS,STATES,ERR) ->
    lint_misc(Ms,IN,DEF,OUT,CLOCKS++Xs,STATES,ERR);
lint_misc([],IN,DEF,OUT,CLOCKS,STATES,ERR) ->
    {IN1,ERR1} = lint_in(IN,[],ERR),
    {DEF1,ERR2} = lint_def(DEF,[],IN1,ERR1),
    {CLOCKS1,ERR3} = lint_clock(CLOCKS,[],ERR2),
    {OUT1,ERR4} = lint_out(OUT,[],IN1,DEF1,CLOCKS1,STATES,ERR3),
    {IN1,DEF1,OUT1,CLOCKS1,ERR4}.

%%
%% Check input identifiers
%%
lint_in([{identifier,Ln,ID}|Xs],IN,ERR) ->
    case lists:keymember(ID,1,IN) of
	true ->
	    E = {Ln,erlister,["input ", ID, " is already defined"]},
	    lint_in(Xs,IN,[E|ERR]);
	false ->
	    lint_in(Xs,[{ID,undefined}|IN],ERR)
    end;
lint_in([{'=',_Ln,{identifier,Ln,ID},Pred}|Xs],IN,ERR) ->
    case lists:keymember(ID,1,IN) of
	true ->
	    E = {Ln,erlister,["input ", ID, " is already defined"]},
	    lint_in(Xs,IN,[E|ERR]);
	false ->
	    {Pred1,ERR1} = lint_pred(Pred,ERR),
	    lint_in(Xs,[{ID,Pred1}|IN],ERR1)
    end;
lint_in([],IN,ERR) ->
    {IN,ERR}.

lint_pred(Pred,ERR) ->
    %% FIXME: check formula
    {Pred,ERR}.

%%
%% Check def identifiers
%%  
lint_def([{'=',_Ln,{identifier,Ln,ID},Sat}|Xs],DEF,IN,ERR) ->
    case lists:keymember(ID,1,DEF) of
	true ->
	    E = {Ln,erlister,["def ", ID, " is already defined"]},
	    lint_def(Xs,DEF,IN,[E|ERR]);
	false ->
	    case lists:keymember(ID,2,IN) of
		true ->
		    E = {Ln,erlister,["def ", ID, " is defined as in"]},
		    lint_def(Xs,DEF,IN,[E|ERR]);
		false ->
		    {Sat1,ERR1} = lint_dsat(Sat,IN,ERR),
		    lint_def(Xs,[{ID,Sat1}|DEF],IN,ERR1)
	    end
    end;
lint_def([],DEF,_IN,ERR) ->
    {DEF,ERR}.

%% lint formula where identifiers may be in parameters
lint_dsat({identifier,Ln,ID},IN,ERR) ->
    case lists:keymember(ID,1,IN) of
	false ->
	    E = {Ln,erlister,["variable ", ID, " must be in parameter"]},
	    {{in,ID},[E|ERR]};
	true ->
	    {{in,ID},ERR}
    end;
lint_dsat({field,Ln,{identifier,_,OBJ},{identifier,_,ID}},IN,ERR) ->
    case lists:keymember({OBJ,ID},1,IN) of
	false ->
	    E = {Ln,erlister,["variable ",OBJ,".",ID," must be in parameter"]},
	    {{in,{OBJ,ID}},[E|ERR]};
	true ->
	    {{in,{OBJ,ID}},ERR}
    end;
lint_dsat(F={'0',_Ln},_IN,ERR) -> {F,ERR};
lint_dsat(F={'1',_Ln},_IN,ERR) -> {F,ERR};
lint_dsat({Op,_Ln,R,L},IN,ERR) when 
      Op =:= '&&'; Op =:= '||'; Op =:= '->'; Op =:= '<->' ->
    {L1,ERR1} = lint_dsat(L,IN,ERR),
    {R1,ERR2} = lint_dsat(R,IN,ERR1),
    {{Op,L1,R1},ERR2};
lint_dsat({Op,_Ln,M},IN,ERR) when 
      Op =:= '!' ->
    {M1,ERR1} = lint_dsat(M,IN,ERR),
    {{Op,M1},ERR1}.
    

lint_clock([{clock,Ln,{identifier,_Ln,ID},{identifier,_Ln,NAME},
	      {Low,High},Step,Default}|Xs], CLOCKS, ERR) ->
    case lists:keymember(ID,1,CLOCKS) of
	true ->
	    E = {Ln,erlister,["clock ", ID, " is already defined"]},
	    lint_clock(Xs,CLOCKS,[E|ERR]);
	false ->
	    L = get_number(Low),
	    H = get_number(High),
	    S = get_number(Step),
	    D = get_number(Default),
	    Clock = {ID,NAME,{L,H},S,D},
	    lint_clock(Xs,[Clock|CLOCKS], ERR)
    end;
lint_clock([], CLOCKS, ERR) ->
    {CLOCKS, ERR}.


lint_out([{'=',Ln,{identifier,_Ln,ID},Sat}|Xs],OUT,IN,DEF,CLOCKS,STATES,ERR) ->
    case lists:keymember(ID,1,IN) orelse
	lists:keymember(ID,1,OUT) orelse
	lists:keymember(ID,1,DEF) orelse
	lists:keymember(ID,1,CLOCKS) orelse
	lists:member(ID,STATES) of
	true ->
	    E = {Ln,erlister,["out ", ID, " is already defined"]},
	    {Sat1,ERR1} = lint_osat(Sat,IN,DEF,STATES,CLOCKS,[E|ERR]),
	    lint_out(Xs,[{ID,Sat1}|OUT],IN,DEF,CLOCKS,STATES,ERR1);
	false ->
	    {Sat1,ERR1} = lint_osat(Sat,IN,DEF,STATES,CLOCKS,ERR),
	    lint_out(Xs,[{ID,Sat1}|DEF],IN,DEF,CLOCKS,STATES,ERR1)
    end;
lint_out([],OUT,_IN,_DEF,_CLOCKS,_STATES,ERR) ->
    {OUT,ERR}.


%% lint formula where identifiers may be in parameters
lint_osat({identifier,Ln,ID},IN,DEF,STATES,_CLOCKS,ERR) ->
    case lists:keymember(ID,1,IN) of
	true ->
	    {{in,ID},ERR};
	false ->
	    case lists:keymember(ID,1,DEF) of
		true ->
		    {{def,ID},ERR};
		false ->
		    case lists:member(ID,STATES) of
			true ->
			    {{state,ID},ERR};
			false ->
			    E = {Ln,erlister,["variable ", ID, 
					      " must be a state,in or def parameter"]},
			    {{in,ID},[E|ERR]}
		    end
	    end
    end;
lint_osat({field,Ln,{identifier,_,OBJ},{identifier,_,ID}},IN,
	 _DEF,_STATES,_CLOCKS,ERR) ->
    case lists:keymember({OBJ,ID},1,IN) of
	false ->
	    E = {Ln,erlister,["variable ",OBJ,".",ID," must be in parameter"]},
	    {{in,{OBJ,ID}},[E|ERR]};
	true ->
	    {{in,{OBJ,ID}},ERR}
    end;
lint_osat(F={'0',_Ln},_IN,_DEF,_STATES,_CLOCKS,ERR) -> {F,ERR};
lint_osat(F={'1',_Ln},_IN,_DEF,_STATES,_CLOCKS,ERR) -> {F,ERR};
lint_osat({timeout,Ln,{identifier,_Ln,ID}},_IN,_DEF,_STATES,CLOCKS,ERR) ->
    case lists:keymember(ID,1,CLOCKS) of
	false ->
	    E = {Ln,erlister,["variable ", ID, " must be a clock"]},
	    {{timeout,ID},[E|ERR]};
	true -> 
	    {{timeout,ID},ERR}
    end;
lint_osat({Op,_Ln,R,L},IN,DEF,STATES,CLOCKS,ERR) when 
      Op =:= '&&'; Op =:= '||'; Op =:= '->'; Op =:= '<->' ->
    {L1,ERR1} = lint_osat(L,IN,DEF,STATES,CLOCKS,ERR),
    {R1,ERR2} = lint_osat(R,IN,DEF,STATES,CLOCKS,ERR1),
    {{Op,L1,R1},ERR2};
lint_osat({Op,_Ln,M},IN,DEF,STATES,CLOCKS,ERR) when 
      Op =:= '!' ->
    {M1,ERR1} = lint_osat(M,IN,DEF,STATES,CLOCKS,ERR),
    {{Op,M1},ERR1}.

lint_trans([{{identifier,Ln,ID},TR}|Trans],Acc,STATES,IN,DEF,CLOCKS,ERR) ->
    case lists:member(ID,STATES) of
	false ->
	    E = {Ln,erlister,["state ", ID, " not declared"]},
	    {TR1,ERR1} = lint_trans_list(TR,[],STATES,IN,DEF,CLOCKS,[E|ERR]),
	    lint_trans(Trans,[{ID,TR1}|Acc],STATES,IN,DEF,CLOCKS,ERR1);
	true ->
	    {TR1,ERR1} = lint_trans_list(TR,[],STATES,IN,DEF,CLOCKS,ERR),
	    lint_trans(Trans,[{ID,TR1}|Acc],STATES,IN,DEF,CLOCKS,ERR1)
    end;
lint_trans([],Acc,_STATES,_IN,_DEF,_CLOCKS,ERR) ->
    {Acc,ERR}.

lint_trans_list([{{identifier,Ln,ID},Sat,{start,START}}|TR],Acc,
		STATES,IN,DEF,CLOCKS,ERR) ->
    ERR1 = case lists:member(ID,STATES) of
	       false -> [{Ln,erlister,["state ", ID, " not declared"]}|ERR];
	       true -> ERR
	   end,
    {Sat1,ERR2} = lint_tsat(Sat,IN,DEF,CLOCKS,ERR1),
    {START1,ERR3} = lint_start(START,[],CLOCKS,ERR2),
    lint_trans_list(TR,[{ID,Sat1,START1}|Acc],STATES,IN,DEF,CLOCKS,ERR3);
lint_trans_list([],Acc,_STATES,_IN,_DEF,_CLOCKS,ERR) ->
    {Acc,ERR}.

lint_start([{identifier,Ln,ID}|START],Acc,CLOCKS,ERR) ->
    case lists:keymember(ID,1,CLOCKS) of
	false ->
	    E = {Ln,erlister,["start ", ID, " is not a clock"]},
	    lint_start(START,[ID|Acc],CLOCKS,[E|ERR]);
	true ->
	    lint_start(START,[ID|Acc],CLOCKS,ERR)
    end;
lint_start([],Acc,_CLOCKS,ERR) ->
    {Acc,ERR}.


%% lint formula where identifiers may be in parameters
lint_tsat({identifier,Ln,ID},IN,DEF,_CLOCKS,ERR) ->
    case lists:keymember(ID,1,IN) of
	false ->
	    case lists:keymember(ID,1,DEF) of
		false ->
		    E = {Ln,erlister,["variable ", ID, " must be in or def parameter"]},
		    {{in,ID},[E|ERR]};
		true ->
		    {{def,ID},ERR}
	    end;
	true ->
	    {{in,ID},ERR}
    end;
lint_tsat({field,Ln,{identifier,_,OBJ},{identifier,_,ID}},IN,_DEF,_CLOCKS,ERR) ->
    case lists:keymember({OBJ,ID},1,IN) of
	false ->
	    E = {Ln,erlister,["variable ",OBJ,".",ID," must be in parameter"]},
	    {{in,{OBJ,ID}},[E|ERR]};
	true ->
	    {{in,{OBJ,ID}},ERR}
    end;
lint_tsat(F={'0',_Ln},_IN,_DEF,_CLOCKS,ERR) -> {F,ERR};
lint_tsat(F={'1',_Ln},_IN,_DEF,_CLOCKS,ERR) -> {F,ERR};
lint_tsat({timeout,Ln,{identifier,_Ln,ID}},_IN,_DEF,CLOCKS,ERR) ->
    case lists:keymember(ID,1,CLOCKS) of
	false ->
	    E = {Ln,erlister,["variable ", ID, " must be a clock"]},
	    {{timeout,ID},[E|ERR]};
	true -> 
	    {{timeout,ID},ERR}
    end;
lint_tsat({Op,_Ln,R,L},IN,DEF,CLOCKS,ERR) when 
      Op =:= '&&'; Op =:= '||'; Op =:= '->'; Op =:= '<->' ->
    {L1,ERR1} = lint_tsat(L,IN,DEF,CLOCKS,ERR),
    {R1,ERR2} = lint_tsat(R,IN,DEF,CLOCKS,ERR1),
    {{Op,L1,R1},ERR2};
lint_tsat({Op,_Ln,M},IN,DEF,CLOCKS,ERR) when 
      Op =:= '!' ->
    {M1,ERR1} = lint_tsat(M,IN,DEF,CLOCKS,ERR),
    {{Op,M1},ERR1}.


get_number({decnum,_Ln,Ds}) -> list_to_integer(Ds,10);
get_number({hexnum,_Ln,[$0,$x|Ds]}) -> list_to_integer(Ds,16);
get_number({octnum,_Ln,[$0|Ds]}) -> list_to_integer(Ds,8);
get_number({binnum,_Ln,[$0,$b|Ds]}) -> list_to_integer(Ds,2);
get_number({flonum,_Ln,Ds}) -> list_to_float(Ds);
get_number({'1',_Ln}) -> 1;
get_number({'0',_Ln}) -> 0.


    

    
