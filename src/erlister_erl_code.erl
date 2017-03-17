%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Generate erlister Erlang code
%%% @end
%%% Created :  9 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister_erl_code).

-export([code/1]).

-include("../include/erlister.hrl").

-define(T, "    ").
-define(N, "\n").
-define(E, ";\n").
-define(Q, "'").

code(#machine{name=ID,in=IN,param=PAR,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=undefined,
	      states=STATES,trans=TRANS}) ->
    [
     declare(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     init(ID,IN,PAR,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     final(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     idle(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     run(ID,IN,PAR,DEF,OUT,STATES,TRANS,CLOCKS,[])
    ];
code(#machine{name=ID,in=IN,param=PAR,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=SUBMACHINES,
	      machines=MACHINES,
	      states=undefined,trans=undefined}) ->
    Ms = sort_by_keys(MACHINES,#machine.name,SUBMACHINES),
    [
     declare(ID,IN,DEF,OUT,[],[],CLOCKS,Ms),
     init(ID,IN,PAR,DEF,OUT,[],[],CLOCKS,Ms),
     final(ID,IN,DEF,OUT,[],[],CLOCKS,Ms),
     idle(ID,IN,DEF,OUT,[],[],CLOCKS,Ms),
     run(ID,IN,PAR,DEF,OUT,[],[],CLOCKS,Ms)
    ].

declare(ID,_IN,_DEF,_OUT,_STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     "-module('",ID,"').",?N,
     "-export([enter/0,enter/1,enter/2,idle/6,run/5,final/0]).",?N,
     "-define(B2I(X), if (X) -> 1; true -> 0 end).",?N,
     "-define(I2B(X), ((X) =/= 0)).", ?N,
     [ ["-define(CLOCK_",ID,"_",C#clock.id,",",
	integer_to_list(trunc(1000*C#clock.default)),").",?N] || C <- CLOCKS ],

     [
      [ ["-define(CLOCK_",M#machine.name,"_",C#clock.id,",",
	 integer_to_list(trunc(1000*C#clock.default)),").",?N] || C <- M#machine.clocks ] || M <- MACHINES],
     ?N,
     "enter() -> enter(false).", ?N,
     "enter(DEBUG) -> enter(DEBUG,#{}).", ?N
    ].

final(ID,_IN,_DEF,_OUT,_STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     ?N,
     "final() ->",?N,
     [ [[?T,"erlister_rt:stop_timer(clk_",M#machine.name,"_",T,"),",?N] ||
	   #clock{id=T} <- M#machine.clocks] || M <- MACHINES ],
     [[?T,"erlister_rt:stop_timer(clk_",ID,"_",T,"),",?N] ||
	 #clock{id=T} <- CLOCKS],
     ?T,"ok.",?N,
     ?N
    ].

idle(_ID,_IN,_DEF,_OUT,_STATES,_TRANS,_CLOCKS,_MACHINES) ->
    [
     ?N,
     "idle(STATE,IN,PAR,OUT,DEBUG,TMO) ->",?N,
     ?T,"if DEBUG -> io:format(\"state=~w, in=~w, out=~w\\n\", [STATE,IN,OUT]); true -> ok end,",?N,
     ?T,"receive",?N,
     ?T,?T,"{timeout,_Ref,_} -> run(STATE,IN,PAR,OUT,DEBUG);",?N,
     ?T,?T,"{debug,D} when is_boolean(D) -> idle(STATE,IN,PAR,OUT,D,TMO);",?N,
     ?T,?T,"upgrade -> ?MODULE:idle(STATE,IN,PAR,OUT,DEBUG,TMO);", ?N,
     ?T,?T,"INPUT when is_map(INPUT) -> run(STATE,maps:merge(IN,INPUT),PAR,OUT,DEBUG)",?N,
     ?T,"after TMO -> run(STATE,IN,PAR,OUT,DEBUG)",?N
     ?T,"end.",?N
     ?N,
     "idle_(STATE,STATE,IN,PAR,OUT,OUT,DEBUG) ->",?N,
     ?T,"idle(STATE,IN,PAR,OUT,DEBUG,infinity);",?N,
     "idle_(_STATE0,STATE,IN,PAR,OUT0,OUT,DEBUG) ->",?N,
     ?T,"erlister_rt:output(OUT0,OUT),",?N,
     ?T,"idle(STATE,IN,PAR,OUT,DEBUG,0).",?N
    ].


init(ID,IN,PAR,_DEF,OUT,STATES,_TRANS,_CLOCKS,MACHINES) ->
    OUT_LIST = [[?Q,ID,"_",Name,?Q]||#var{id=Name,expr=Expr} <- OUT,
				     Expr =/= undefined] ++
	lists:append([ [[?Q,M#machine.name,"_",Name,?Q] ||
			   #var{id=Name,expr=Expr} <- M#machine.out,
			   Expr =/= undefined] ||
			 M <- MACHINES]),
    [
     "enter(DEBUG,INPUT) -> ",?N,
     %% PARAM
     ?T, "PAR = #{",?N,
     arglist([ [[?Q,ID,"_",Name,?Q]," => ",
		"erlister_rt:param_fetch(",integer_to_list(Index),",0)"] ||
		 #var{id=Name,expr=Index} <- PAR],
	     [?T,?T],",",?N),?N,
     ?T,"}",",",?N,
     %% INPUT
     ?T, "IN0 = #{",?N,
     arglist([ [[?Q,ID,"_",Name,?Q]," => 0"] ||
		 #var{id=Name} <- IN],
	     [?T,?T],",",?N),?N,
     ?T,"}",",",?N,
     ?T,"IN = maps:merge(IN0, INPUT),",?N,
     %% OUTPUT
     ?T, "OUT = #{",?N,
     arglist([[X," => 0"] || X <- OUT_LIST ],
	     [?T,?T],",",?N),
     ?N,?T,"},",?N,
     %% STATE
     ?T, "STATE = #{ ",?N,
     arglist([[?Q,M#machine.name,"_state",?Q, " => ",
	       [?Q,initial_state(M#machine.states)],?Q] || 
		 M <- MACHINES],
	     [?T,?T],",",?N),
     if STATES =:= [] ->
	     [];
	true ->
	     [?T,?T,?Q,ID,"_state",?Q," => ",
	      [?Q,hd(state_names(STATES))],?Q,?N]
     end,
     ?T, "},",?N,

     ?T,"run(STATE,IN,PAR,OUT,DEBUG).",
     ?N
     ].

initial_state([]) -> "undefined";
initial_state(STATES) -> hd(state_names(STATES)).
    

run(ID,IN,PAR,DEF,OUT,STATES,TRANS,CLOCKS,MACHINES) ->
    OUT_LIST = [{ID,Name} || #var{id=Name,expr=Expr} <- OUT,
			     Expr =/= undefined] ++
	lists:append([ [{M#machine.name,Name} ||
			   #var{id=Name,expr=Expr} <- M#machine.out,
			   Expr =/= undefined] ||
			 M <- MACHINES]),
    [
     ?N,
     "run(",?N,
      %% STATES
     ?T,"STATE = #{",?N,
     arglist([ [?T,?T,?Q,M#machine.name,"_state'"," := ",
		["ST_",M#machine.name]] ||
		 M <- MACHINES],
	     [?T,?T],",",?N),
     if STATES =:= [] ->
	     [?N];
	true ->
	     [?T,?T,?Q,ID,"_state'"," := ",
	      ["ST_",ID],?N]
     end,
     ?T, "}",",",?N,

     %% INPUT
     ?T,"IN = #{",?N,
     arglist([ [[?Q,ID,"_",Name,?Q]," := ",
		["I_",ID,"_",Name]] ||
		 #var{id=Name} <- IN],
	     [?T,?T],",",?N),?N,
     ?T,"}",",",?N,

     %% PARAM
     ?T,"PAR = #{",?N,
     arglist([ [[?Q,ID,"_",Name,?Q]," := ",
		["P_",ID,"_",Name]] ||
		 #var{id=Name} <- PAR],
	     [?T,?T],",",?N),?N,
     ?T,"}",",",?N,

     %% OUTPUT
     ?T,"OUT = #{",?N,
     arglist([ [[?Q,M,"_",Name,?Q]," := ",
		["OUT_",M,"_",Name]] ||
		 {M,Name} <- OUT_LIST],
	     [?T,?T],",",?N),?N,
     ?T,"},DEBUG) ->", ?N,
     %% load input from formula
     [ [?T,["IN_",ID,"_",Name]," = ",expr(ID,in,Type,Name,Expr),",",?N] || 
	 #var{id=Name,type=Type,expr=Expr} <- IN],

     [ [?T,["DEF_",ID,"_",Name]," = ",expr(ID,def,Type,Expr),",",?N] ||
	 #var{id=Name,type=Type,expr=Expr} <- DEF, Expr =/= undefined ],

     [ [?T,["CLK_",ID,"_",T]," = ",
	"erlister_rt:timer_read(", "clk_",ID,"_",T,"),",?N]
       || #clock{id=T} <- CLOCKS],

     [ begin
	   Mid = M#machine.name,
	   [ [[?T,["IN_",Mid,"_",Name]," = ",expr(ID,in,Type,Expr),",",?N] ||
		 #var{id=Name,type=Type,expr=Expr} <- 
		     M#machine.in,Expr =/= undefined],
	     [[?T,["DEF_",Mid,"_",Name]," = ",
	       expr(ID,def,Type,Expr),",",?N] ||
		 #var{id=Name,type=Type,expr=Expr} <-
		     M#machine.def,Expr =/= undefined],
	     [[?T,["CLK_",Mid,"_",T]," = ",
	       "erlister_rt:timer_read(", "clk_",Mid,"_",T,"),",?N] ||
		 #clock{id=T} <- M#machine.clocks],
	     code_trans(Mid, M#machine.trans, true),
	     [ [?T,"OUT1_",[Mid,"_",Name]," = ", 
		expr(Mid,out,Type,Expr),",",?N] || 
		 #var{id=Name,type=Type,expr=Expr} <- 
		     M#machine.out,Expr =/= undefined]
	   ]
       end || M <- MACHINES],
     
     code_trans(ID, TRANS, false),
     [ [?T,"OUT1_",[ID,"_",Name]," = ", expr(ID,out,Type,Expr),",",?N] || 
	 #var{id=Name,type=Type,expr=Expr} <- OUT,Expr =/= undefined],

     [?T,"OUT1 = OUT#{",?N,
      arglist([ [[?Q,M,"_",Name,?Q]," => ",
		 ["OUT1_",M,"_",Name]] ||
		  {M,Name} <- OUT_LIST],
	      [?T,?T],",",?N),?N,
      ?T,"},", ?N],

     [?T, "STATE1 = STATE#{",?N,
      arglist([ [?T,?T,?Q,M#machine.name,"_state",?Q," => ",
		 ["ST1_",M#machine.name]] ||
		  M <- MACHINES],
	      [?T,?T],",",?N),
      if STATES =:= [] ->
	      [?N];
	 true ->
	      [?T,?T,?Q,ID,"_state",?Q," => ",
	       ["ST1_",ID],?N]
      end,
      ?T, "},",?N],
     ?T, "idle_(STATE,STATE1,IN,PAR,OUT,OUT1,DEBUG)."
     ?N
    ].

code_trans(_ID,[],_IsSubMachine=false) ->
    [];
code_trans(ID,[],_IsSubMachine=true) ->
    [?T,"ST1_",ID," = ","ST_",ID,",",?N];
code_trans(ID,TRs,_IsSubMachine) ->
    [?T,"ST1_",ID," = ",?N,
     ?T,?T,"case (ST_",ID,") of",?N,
     [[ [?T,?T,[?Q,From,?Q]," ->",?N,
	 [?T,?T,?T,"if",?N],
	 [ begin
	       [?T,?T,?T,?T,expr_(ID,trans,boolean,Cond), " ->",?N,
		tlist(ID,Start,[?T,?T,?T,?T,?T],[",",?N]),
		action(ID,Action,[?T,?T,?T,?T,?T],[",",?N]),
		?T,?T,?T,?T,?T,[?Q,To,?Q],?E
	       ]
	   end || {Cond,To,Start,Action} <- FromGroup],
	 [?T,?T,?T,?T,"true -> ", ["ST_",ID],?N,
	  ?T,?T,?T,"end",?E]
	] || {From,FromGroup} <- group_trans(TRs)],
      [?T,?T,"_ -> ST_", ID],?N,
      [?T,?T,"end,",?N]]
    ].

%% sort items according to the order of Keys
sort_by_keys(Items,Pos,Keys) ->
    lists:sort(
      fun(A,B) ->
	      index(element(Pos,A), Keys) < index(element(Pos,B),Keys)
      end, Items).

index(Key,Keys) ->
    index_(Key,Keys,1).

index_(Key,[Key|_],I) -> I;
index_(Key,[_|Keys],I) -> index_(Key,Keys,I+1).

state_names(STATES) ->    
    [S || #state{id=S} <- STATES].
    
%% group transitions as [{From,[{Cond,To,Start}]}]
group_trans(TRs) ->
    Ls = [{From,_,_,_,_}|_] = lists:sort(expand_trans(TRs)),
    group_trans_(From, Ls, [], []).

group_trans_(From, [{From,Cond,To,Start,Action}|TRs], FromList, Acc) ->
    group_trans_(From, TRs, [{Cond,To,Start,Action}|FromList], Acc);
group_trans_(From, [{From1,Cond,To,Start,Action}|TRs], FromList, Acc) ->
    group_trans_(From1, TRs, [{Cond,To,Start,Action}], [{From,FromList}|Acc]);
group_trans_(From, [], FromList, Acc) ->
    lists:reverse([{From,FromList}|Acc]).

expand_trans([{From,_Ln0,FromList} | TRs]) ->
    [{From,Cond,To,Start,Action} || {To,_Ln1,Cond,Start,Action} <- FromList] ++
	expand_trans(TRs);
expand_trans([]) ->
    [].

tlist(_ID,[],_Pre,_Sep) -> [];
tlist(ID,[{T,_Ln}|Ts],Pre,Sep) ->
    Timeout = ["?CLOCK_",ID,"_",T], %% fixme read params
    [Pre,"erlister_rt:timer_start(clk_",ID,"_",T,",",Timeout,")",Sep | 
     tlist(ID,Ts,Pre,Sep)].

%% FIXME: implement
action(_SELF,[],_Pre,_Sep) -> [];
action(SELF,[A|As],Pre,Sep) ->
    [Pre,"%% ", action_(SELF,A), Sep |
     action(SELF,As,Pre,Sep)].

action_(_SELF,{decl,#var{id=_Var,type=_Type,expr=undefined}}) ->
    [];
action_(SELF,{decl,#var{id=Var,type=Type,expr=Expr}}) ->
    [["VAR_",Var]," = ",expr_(SELF,action,Type,Expr),","];
action_(SELF,{store,#var{id=Var,type=Type},Expr}) -> 
    %% fixme: update variable
    [["VAR_",Var]," = ",expr_(SELF,action,Type,Expr),","];
action_(SELF,{call,Func,Args}) ->
    {_RType,Name,ArgTypes} = erlister_lint:find_signature(Func),
    call(SELF,action,Name,ArgTypes,Args).

arglist([], _Pre, _Sep, _Del) -> [];
arglist([A], Pre, _Sep, _Del) -> [Pre,A];
arglist([A|As],Pre,Sep,Del) -> [Pre,A,Sep,Del | arglist(As,Pre,Sep,Del)].

expr(SELF,in,_Type,Name,undefined) ->
    ["I_",fid(SELF,Name)];
expr(SELF,Class,boolean,_Name,F) ->
    case F of
	{in,ID,boolean}   -> ["IN_",fid(SELF,ID)];
	{out1,ID,boolean} -> ["OUT1_",fid(SELF,ID)];
	{out,ID,boolean} -> ["OUT_",fid(SELF,ID)];
	{def,ID,boolean} -> ["DEF_",fid(SELF,ID)];
	{var,ID,boolean} -> ["VAR_",fid(SELF,ID)];
	_ -> ["?B2I(",expr_(SELF,Class,boolean,F),")"]
    end;
expr(SELF,Class,Type,_Name,F) -> 
    expr_(SELF,Class,Type,F).

expr(SELF,Class,boolean,F) ->
    case F of
	{in,ID,boolean}   -> ["IN_",fid(SELF,ID)];
	{out1,ID,boolean} -> ["OUT1_",fid(SELF,ID)];
	{out,ID,boolean} -> ["OUT_",fid(SELF,ID)];
	{def,ID,boolean} -> ["DEF_",fid(SELF,ID)];
	{var,ID,boolean} -> ["VAR_",fid(SELF,ID)];
	_ -> ["?B2I(",expr_(SELF,Class,boolean,F),")"]
    end;
expr(SELF,Class,Type,F) ->
    expr_(SELF,Class,Type,F).

expr_(_SELF,_Class,_Type,{const,true})  -> "true";
expr_(_SELF,_Class,_Type,{const,false}) -> "false";
expr_(_SELF,_Class,_Type,{const,C})    -> integer_to_list(C);

expr_(SELF,_Class,boolean,{in,ID,boolean}) ->  ["?I2B(IN_",fid(SELF,ID),")"];
expr_(SELF,_Class,boolean,{out1,ID,boolean}) ->["?I2B(OUT1_",fid(SELF,ID),")"];
expr_(SELF,_Class,boolean,{out,ID,boolean}) -> ["?I2B(OUT_",fid(SELF,ID),")"];
expr_(SELF,_Class,boolean,{def,ID,boolean}) -> ["?I2B(DEF_",fid(SELF,ID),")"];
expr_(_SELF,_Class,boolean,{var,ID,boolean}) -> ["?I2B(","VAR_",ID,")"];
expr_(SELF,in,_Type,{in,ID,_T}) ->  ["I_",fid(SELF,ID)];
expr_(SELF,_Class,_Type,{in,ID,_T}) -> ["IN_",fid(SELF,ID)];
expr_(SELF,_Class,_Type,{out1,ID,_T}) -> ["OUT1_",fid(SELF,ID)];
expr_(SELF,_Class,_Type,{out,ID,_T}) -> ["OUT_",fid(SELF,ID)];
expr_(SELF,_Class,_Type,{def,ID,_T}) -> ["DEF_",fid(SELF,ID)];
expr_(_SELF,_Class,_Type,{var,ID,_T}) -> ["VAR_",ID];
expr_(SELF,_Class,boolean,{param,ID,_Index,boolean}) ->
    ["?I2B(P_",fid(SELF,ID),")"];
expr_(SELF,_Class,_Type,{param,ID,_Index,__Type})  ->
    ["P_",fid(SELF,ID)];
expr_(SELF,out,_Type,{state,ID}) ->
    ["(ST1_",mid(SELF,ID)," =:= ", ?Q,fld(ID),?Q,")"];
expr_(SELF,_Class,_Type,{state,ID}) ->
    ["(ST_",mid(SELF,ID)," =:= ", ?Q,fld(ID),?Q,")"];
expr_(SELF,_Class,_Type,{timeout,ID}) ->
    ["(","CLK_",fid(SELF,ID)," =:= timeout)"];
expr_(SELF,Class,_Type,{call,Func,Args}) ->
    {_RType,Name,ArgTypes} = erlister_lint:find_signature(Func),
    call(SELF,Class,Name,ArgTypes,Args);
expr_(SELF,Class,Type,{'?',C,T,E}) ->
    [ "if ",expr_(SELF,Class,boolean,C), " -> ",
      expr_(SELF,Class,Type,T), "; ",
      "true -> ", expr_(SELF,Class,Type,E), " end"];
expr_(SELF,Class,Type,{'&&',L,R}) -> logic(SELF,Class,Type,"andalso",L,R);
expr_(SELF,Class,Type,{'||',L,R}) -> logic(SELF,Class,Type,"orelse",L,R);
expr_(SELF,Class,Type,{'->',L,R}) -> expr_(SELF,Class,Type,{'||',{'!',L},R});
expr_(SELF,Class,Type,{'!',F}) -> logic(SELF,Class,Type,"not",F);
expr_(SELF,Class,Type,{'<->',L,R}) -> logic(SELF,Class,Type,"=:=",L,R);
expr_(SELF,Class,Type,{'<',L,R}) ->  compare(SELF,Class,Type,"<",L,R);
expr_(SELF,Class,Type,{'<=',L,R}) -> compare(SELF,Class,Type,"=<",L,R);
expr_(SELF,Class,Type,{'>',L,R}) ->  compare(SELF,Class,Type,">",L,R);
expr_(SELF,Class,Type,{'>=',L,R}) -> compare(SELF,Class,Type,">=",L,R);
expr_(SELF,Class,Type,{'==',L,R}) -> compare(SELF,Class,Type,"=:=",L,R);
expr_(SELF,Class,Type,{'!=',L,R}) -> compare(SELF,Class,Type,"=/=",L,R);
expr_(SELF,Class,Type,{'+',L,R}) -> arith(SELF,Class,Type,"+",L,R);
expr_(SELF,Class,Type,{'-',L,R}) -> arith(SELF,Class,Type,"-",L,R);
expr_(SELF,Class,Type,{'*',L,R}) -> arith(SELF,Class,Type,"*",L,R);
expr_(SELF,Class,Type,{'/',L,R}) -> arith(SELF,Class,Type,"div",L,R);
expr_(SELF,Class,Type,{'%',L,R}) -> arith(SELF,Class,Type,"rem",L,R);
expr_(SELF,Class,Type,{'&',L,R}) -> arith(SELF,Class,Type,"band",L,R);
expr_(SELF,Class,Type,{'|',L,R}) -> arith(SELF,Class,Type,"bor",L,R);
expr_(SELF,Class,Type,{'^',L,R}) -> arith(SELF,Class,Type,"bxor",L,R);
expr_(SELF,Class,Type,{'<<',L,R}) -> arith(SELF,Class,Type,"bsl",L,R);
expr_(SELF,Class,Type,{'>>',L,R}) -> arith(SELF,Class,Type,"bsr",L,R);
expr_(SELF,Class,Type,{'~',F}) -> arith(SELF,Class,Type,"bnot",F);
expr_(SELF,Class,Type,{'-',F}) -> arith(SELF,Class,Type,"-",F).

expr_list(_SELF,_Class,[],[]) -> [];
expr_list(SELF,Class,[T],[E]) -> expr(SELF,Class,T,E);
expr_list(SELF,Class,[T|Ts],[E|Es]) ->
    [expr(SELF,Class,T,E),",",expr_list(SELF,Class,Ts,Es)].

%% 
call(SELF,Class,Name,Types,Args) ->
    case Name of
	param_fetch ->
	    ["erlister_rt:param_fetch","(",
	     expr_list(SELF,Class,Types,Args),")"];
	param_store ->
	    ["erlister_rt:param_store","(",
	     expr_list(SELF,Class,Types,Args),")"];
	uart_send ->
	    ["erlister_rt:uart_send","(",
	     expr_list(SELF,Class,Types,Args),")"];
	uart_recv ->
	    ["erlister_rt:uart_recv","(",
	     expr_list(SELF,Class,Types,Args),")"];
	uart_avail ->
	    ["erlister_rt:uart_avail", "(",
	     expr_list(SELF,Class,Types,Args),")"];
	timer_now ->
	    ["erlang:system_time(milli_seconds)"];
	gpio_input ->
	    ["erlister_rt:gpio_input","(",
	     expr_list(SELF,Class,Types,Args),")"];
	gpio_output ->
	    ["erlister_rt:gpio_output","(",
	     expr_list(SELF,Class,Types,Args),")"];
	gpio_set ->
	    ["erlister_rt:gpio_set","(",
	     expr_list(SELF,Class,Types,Args),")"];
	gpio_clr ->
	    ["erlister_rt:gpio_clr","(",
	     expr_list(SELF,Class,Types,Args),")"];
	gpio_get ->
	    ["erlister_rt:gpio_get","(",
	     expr_list(SELF,Class,Types,Args),")"];
	analog_send ->
	    ["erlister_rt:analog_send","(",
	     expr_list(SELF,Class,Types,Args),")"];
	analog_recv ->
	    ["erlister_rt:analog_recv","(",
	     expr_list(SELF,Class,Types,Args),")"];
	can_send ->
	    ["erlister_rt:can_send","(",
	     expr_list(SELF,Class,Types,Args),")"]
    end.

arith(SELF,Class,Type,OP,F) ->
    pform(SELF,Class,Type,OP,F).    

arith(SELF,Class,Type,OP,L,R) ->
    pform(SELF,Class,Type,OP,L,R).

compare(SELF,Class,boolean,OP,L,R) ->
    pform(SELF,Class,integer,OP,L,R);
compare(SELF,Class,_IntegerType,OP,L,R) ->
    ["?B2I(",pform(SELF,Class,integer,OP,L,R),")"].

logic(SELF,Class,boolean,OP,F) ->
    pform(SELF,Class,boolean,OP,F);
logic(SELF,Class,_Type,OP,F) ->
    ["?B2I(",pform(SELF,Class,boolean,OP,F),")"].

logic(SELF,Class,boolean,OP,L,R) ->
    pform(SELF,Class,boolean,OP,L,R);
logic(SELF,Class,_IntegerType,OP,L,R) ->
    ["?B2I(",pform(SELF,Class,boolean,OP,L,R),")"].

pform(SELF,Class,Type,OP,F) ->
    [OP," ",pf(SELF,Class,Type,{OP,1},F)].
    
pform(SELF,Class,Type,OP,L,R) ->
    [ pf(SELF,Class,Type,{OP,2},L),
      " ",OP," ",
      pf(SELF,Class,Type,{OP,2},R) ].

pf(SELF,Class,Type,OPP,F) ->
    PrioOp   = priority(OPP),
    PrioF = priority(operator(F)),
    case (PrioF>0) andalso (PrioOp > PrioF) of
	true ->
	    ["(",expr_(SELF,Class,Type,F),")"];
	false ->
	    expr_(SELF,Class,Type,F)
    end.

mid(_SELF,[ID,".",_FLD]) -> ID;
mid(SELF, _) -> SELF.

fld([_ID,".",FLD]) -> FLD;
fld(ID) -> ID.

fid(_SELF,[ID,".",FLD]) -> [ID,"_",FLD];
fid(SELF,ID) -> [SELF,"_",ID].

%% for a node return {operator,arity}
operator(T) when is_tuple(T) ->
    A = tuple_size(T)-1,
    case element(1,T) of
	'&&' -> {"andalso",A};
	'||' -> {"orelse",A};
	'!' -> {"not",A};
	'<' -> {"<",A};
	'<=' -> {"=<",A};
	'>' -> {">",A};
	'>=' -> {">=",A};
	'==' -> {"=:=",A};
	'!=' -> {"=/=",A};
	'+' -> {"+",A};
	'-' -> {"-",A};
	'*' -> {"*",A};
	'/' -> {"div",A};
	'%' -> {"rem",A};
	'&' -> {"band",A};
	'|' -> {"bor",A};
	'^' -> {"bor",A};
	'<<' -> {"bsl",A};
	'>>' -> {"bsr",A};
	'~' -> {"bnot",A};
	_ -> {undefined,0}
    end.

priority({"orelse",2}) -> 150;
priority({"andalso",2}) -> 160;
priority({"==",2}) -> 200;
priority({"/=",2}) -> 200;
priority({"=<",2}) -> 200;
priority({"<",2}) -> 200;
priority({">=",2}) -> 200;
priority({">",2}) -> 200;
priority({"=:=",2}) -> 200;
priority({"=/=",2}) -> 200;
priority({"+",2}) -> 400;
priority({"-",2}) -> 400;
priority({"bor",2}) -> 400;
priority({"bxor",2}) -> 400;
priority({"bsl",2}) -> 400;
priority({"bsr",2}) -> 400;
priority({"or",2}) -> 400;
priority({"xor",2}) -> 400;
priority({"/",2}) -> 500;
priority({"*",2}) -> 500;
priority({"div",2}) -> 500;
priority({"rem",2}) -> 500;
priority({"band",2}) -> 500;
priority({"and",2}) -> 500;
priority({"+",1}) -> 600;
priority({"-",1}) -> 600;
priority({"not",1}) -> 600;
priority({"bnot",1}) -> 600;
priority(_) -> 0.
