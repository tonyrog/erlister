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

code(#machine{name=ID,in=IN,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=undefined,
	      states=STATES,trans=TRANS}) ->
    [
     declare(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     init(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     final(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     wait(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     main(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[])
    ];
code(#machine{name=ID,in=IN,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=SUBMACHINES,
	      machines=MACHINES,
	      states=undefined,trans=undefined}) ->
    Ms = sort_by_keys(MACHINES,#machine.name,SUBMACHINES),
    [
     declare(ID,IN,DEF,OUT,[],[],CLOCKS,Ms),
     init(ID,IN,DEF,OUT,[],[],CLOCKS,Ms),
     final(ID,IN,DEF,OUT,[],[],CLOCKS,Ms),
     wait(ID,IN,DEF,OUT,[],[],CLOCKS,Ms),
     main(ID,IN,DEF,OUT,[],[],CLOCKS,Ms)
    ].

declare(ID,_IN,_DEF,_OUT,_STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     "-module('",ID,"').",?N,
     "-export([enter/0,enter/1,enter/2,wait/4,main/4,final/0]).",?N,
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

wait(_ID,_IN,_DEF,_OUT,_STATES,_TRANS,_CLOCKS,_MACHINES) ->
    [
     ?N,
     "wait(STATE,IN,OUT,DEBUG) ->",?N,
     ?T,"if DEBUG -> io:format(\"state=~w, in=~w, out=~w\\n\", [STATE,IN,OUT]); true -> ok end,",?N,
     ?T,"receive",?N,
     ?T,?T,"{timeout,_Ref,_} -> main(STATE,IN,OUT,DEBUG);",?N,
     ?T,?T,"debug   -> wait(STATE,IN,OUT,true);",?N,
     ?T,?T,"nodebug -> wait(STATE,IN,OUT,false);",?N,
     ?T,?T,"upgrade -> ?MODULE:wait(STATE,IN,OUT,DEBUG);", ?N,
     ?T,?T,"INPUT -> main(STATE,maps:merge(IN,INPUT),OUT,DEBUG)",?N,
     ?T,"end.",?N
     ?N
    ].


init(ID,IN,_DEF,OUT,STATES,_TRANS,_CLOCKS,MACHINES) ->
    OUT_LIST = [[?Q,ID,"_",Name,?Q]||#var{id=Name,expr=Expr} <- OUT,
				     Expr =/= undefined] ++
	lists:append([ [[?Q,M#machine.name,"_",Name,?Q] ||
			   #var{id=Name,expr=Expr} <- M#machine.out,
			   Expr =/= undefined] ||
			 M <- MACHINES]),
    [
     "enter(DEBUG,INPUT) -> ",?N,
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
	       [?Q,hd(state_names(M#machine.states))],?Q] || 
		 M <- MACHINES, M#machine.states =/= []],
	     [?T,?T],",",?N),
     if STATES =:= [] ->
	     [];
	true ->
	     [?T,?T,?Q,ID,"_state",?Q," => ",
	      [?Q,hd(state_names(STATES))],?Q,?N]
     end,
     ?T, "},",?N,

     ?T,"main(STATE,IN,OUT,DEBUG).",
     ?N
     ].


main(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,MACHINES) ->
    OUT_LIST = [{ID,Name} || #var{id=Name,expr=Expr} <- OUT,
			     Expr =/= undefined] ++
	lists:append([ [{M#machine.name,Name} ||
			   {Name,_Type,_,Expr} <- M#machine.out,
			   Expr =/= undefined] ||
			 M <- MACHINES]),
    [
     ?N,
     "main(",?N,
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
     %% OUTPUT
     ?T,"OUT = #{",?N,
     arglist([ [[?Q,M,"_",Name,?Q]," := ",
		["OUT_",M,"_",Name]] ||
		 {M,Name} <- OUT_LIST],
	     [?T,?T],",",?N),?N,
     ?T,"},DEBUG) ->", ?N,
     %% load input from formula
     [ [?T,["IN_",ID,"_",Name]," = ",formula(ID,in,Type,Name,Expr),",",?N] || 
	 #var{id=Name,type=Type,expr=Expr} <- IN],

     [ [?T,["DEF_",ID,"_",Name]," = ",formula(ID,def,Type,Expr),",",?N] ||
	 #var{id=Name,type=Type,expr=Expr} <- DEF, Expr =/= undefined ],

     [ [?T,["CLK_",ID,"_",T]," = ",
	"erlister_rt:timer_read(", "clk_",ID,"_",T,"),",?N]
       || #clock{id=T} <- CLOCKS],

     [ begin
	   Mid = M#machine.name,
	   [ [[?T,["IN_",Mid,"_",Name]," = ",formula(ID,in,Type,Expr),",",?N] ||
		 #var{id=Name,type=Type,expr=Expr} <- 
		     M#machine.in,Expr =/= undefined],
	     [[?T,["DEF_",Mid,"_",Name]," = ",
	       formula(ID,def,Type,Expr),",",?N] ||
		 #var{id=Name,type=Type,expr=Expr} <-
		     M#machine.def,Expr =/= undefined],
	     [[?T,["CLK_",Mid,"_",T]," = ",
	       "erlister_rt:timer_read(", "clk_",Mid,"_",T,"),",?N] ||
		 #clock{id=T} <- M#machine.clocks],
	     code_trans(Mid, M#machine.trans),
	     [ [?T,"OUT1_",[Mid,"_",Name]," = ", 
		formula(Mid,out,Type,Expr),",",?N] || 
		 #var{id=Name,type=Type,expr=Expr} <- 
		     M#machine.out,Expr =/= undefined]
	   ]
       end || M <- MACHINES],
     
     code_trans(ID, TRANS),
     [ [?T,"OUT1_",[ID,"_",Name]," = ", formula(ID,out,Type,Expr),",",?N] || 
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
     [?T, "if STATE1 =:= STATE -> wait(STATE1,IN,OUT1,DEBUG)",?E,
      ?T,?T, "true -> main(STATE1,IN,OUT1,DEBUG)",?N,
      ?T, "end."],
     ?N
    ].

code_trans(_ID,[]) ->
    [];
code_trans(ID,TRs) ->
    [?T,"ST1_",ID," = ",?N,
     ?T,?T,"case (ST_",ID,") of",?N,
     [[ [?T,?T,[?Q,From,?Q]," ->",?N,
	 [?T,?T,?T,"if",?N],
	 [ begin
	       [?T,?T,?T,?T,formula_(ID,trans,boolean,Cond), " ->",?N,
		tlist(ID,Start,[?T,?T,?T,?T,?T],[",",?N]),
		action(Action,[?T,?T,?T,?T,?T],[",",?N]),
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
action([],_Pre,_Sep) -> [];
action([A|As],Pre,Sep) ->
    [Pre,"%% ",lists:flatten(io_lib:format("~p",[A])),Sep |
     action(As,Pre,Sep)].

arglist([], _Pre, _Sep, _Del) -> [];
arglist([A], Pre, _Sep, _Del) -> [Pre,A];
arglist([A|As],Pre,Sep,Del) -> [Pre,A,Sep,Del | arglist(As,Pre,Sep,Del)].

formula(SELF,in,_Type,Name,undefined) ->
    ["I_",fid(SELF,Name)];
formula(SELF,Class,boolean,_Name,F) ->
    ["?B2I(",formula_(SELF,Class,boolean,F),")"];
formula(SELF,Class,Type,_Name,F) -> 
    formula_(SELF,Class,Type,F).

formula(SELF,Class,boolean,F) ->
    ["?B2I(",formula_(SELF,Class,boolean,F),")"];
formula(SELF,Class,Type,F) ->
    formula_(SELF,Class,Type,F).

formula_(_SELF,_Class,_Type,{const,true})  -> "true";
formula_(_SELF,_Class,_Type,{const,false}) -> "false";
formula_(_SELF,_Class,_Type,{const,C})    -> integer_to_list(C);

formula_(SELF,_Class,_Type,{in,ID,boolean}) ->  ["?I2B(IN_",fid(SELF,ID),")"];
formula_(SELF,_Class,_Type,{out1,ID,boolean}) ->["?I2B(OUT1_",fid(SELF,ID),")"];
formula_(SELF,_Class,_Type,{out,ID,boolean}) -> ["?I2B(OUT_",fid(SELF,ID),")"];
formula_(SELF,_Class,_Type,{def,ID,boolean}) -> ["?I2B(DEF_",fid(SELF,ID),")"];
formula_(SELF,in,_Type,{in,ID,_T}) ->  ["I_",fid(SELF,ID)];
formula_(SELF,_Class,_Type,{in,ID,_T}) -> ["IN_",fid(SELF,ID)];
formula_(SELF,_Class,_Type,{out1,ID,_T}) -> ["OUT1_",fid(SELF,ID)];
formula_(SELF,_Class,_Type,{out,ID,_T}) -> ["OUT_",fid(SELF,ID)];
formula_(SELF,_Class,_Type,{def,ID,_T}) -> ["DEF_",fid(SELF,ID)];
formula_(SELF,out,_Type,{state,ID}) ->
    ["(ST1_",mid(SELF,ID)," =:= ", ?Q,fld(ID),?Q,")"];
formula_(SELF,_Class,_Type,{state,ID}) ->
    ["(ST_",mid(SELF,ID)," =:= ", ?Q,fld(ID),?Q,")"];
formula_(SELF,_Class,_Type,{timeout,ID}) ->
    ["(","CLK_",fid(SELF,ID)," =:= timeout)"];
formula_(SELF,Class,Type,{'?',C,T,E}) ->
    [ "if (",formula(SELF,Class,boolean,C), ") -> ",
      formula(SELF,Class,Type,T), "; ",
      "true -> ", formula(SELF,Class,Type,E), "end"];
formula_(SELF,Class,Type,{'&&',L,R}) -> logic(SELF,Class,Type," andalso ",L,R);
formula_(SELF,Class,Type,{'||',L,R}) -> logic(SELF,Class,Type," orelse ",L,R);
formula_(SELF,Class,Type,{'->',L,R}) -> formula_(SELF,Class,Type,{'||',{'!',L},R});
formula_(SELF,Class,Type,{'!',F}) -> logic(SELF,Class,Type,"not",F);
formula_(SELF,Class,Type,{'<->',L,R}) -> logic(SELF,Class,Type," =:= ",L,R);
formula_(SELF,Class,Type,{'<',L,R}) ->  compare(SELF,Class,Type,"<",L,R);
formula_(SELF,Class,Type,{'<=',L,R}) -> compare(SELF,Class,Type,"=<",L,R);
formula_(SELF,Class,Type,{'>',L,R}) ->  compare(SELF,Class,Type,">",L,R);
formula_(SELF,Class,Type,{'>=',L,R}) -> compare(SELF,Class,Type,">=",L,R);
formula_(SELF,Class,Type,{'==',L,R}) -> compare(SELF,Class,Type,"=:=",L,R);
formula_(SELF,Class,Type,{'!=',L,R}) -> compare(SELF,Class,Type,"=/=",L,R);
formula_(SELF,Class,Type,{'+',L,R}) -> arith(SELF,Class,Type,"+",L,R);
formula_(SELF,Class,Type,{'-',L,R}) -> arith(SELF,Class,Type,"-",L,R);
formula_(SELF,Class,Type,{'*',L,R}) -> arith(SELF,Class,Type,"*",L,R);
formula_(SELF,Class,Type,{'/',L,R}) -> arith(SELF,Class,Type,"div",L,R);
formula_(SELF,Class,Type,{'%',L,R}) -> arith(SELF,Class,Type,"rem",L,R);
formula_(SELF,Class,Type,{'&',L,R}) -> arith(SELF,Class,Type,"band",L,R);
formula_(SELF,Class,Type,{'|',L,R}) -> arith(SELF,Class,Type,"bor",L,R);
formula_(SELF,Class,Type,{'^',L,R}) -> arith(SELF,Class,Type,"bxor",L,R);
formula_(SELF,Class,Type,{'<<',L,R}) -> arith(SELF,Class,Type,"bsl",L,R);
formula_(SELF,Class,Type,{'>>',L,R}) -> arith(SELF,Class,Type,"bsr",L,R);
formula_(SELF,Class,Type,{'~',F}) -> arith(SELF,Class,Type,"bnot",F);
formula_(SELF,Class,Type,{'-',F}) -> arith(SELF,Class,Type,"-",F).

arith(SELF,Class,Type,Op,F) ->
    [Op,"(",formula_(SELF,Class,Type,F),")"].

arith(SELF,Class,Type,Op,L,R) ->
    ["(",formula_(SELF,Class,Type,L),") ",Op," (",
     formula_(SELF,Class,Type,R),")"].


compare(SELF,Class,boolean,Op,L,R) ->
    ["(",formula_(SELF,Class,integer,L),") ",
     Op, 
     " (",formula_(SELF,Class,integer,R),")"];
compare(SELF,Class,_IntegerType,Op,L,R) ->
    ["?B2I((",formula_(SELF,Class,integer,L),") ",
     Op, 
     " (",formula_(SELF,Class,integer,R),"))"].

logic(SELF,Class,boolean,Op,F) ->
    [Op,"(",formula_(SELF,Class,boolean,F),")"];
logic(SELF,Class,_IntegerType,Op,F) ->
    ["?B2I(",Op,"(",formula_(SELF,Class,boolean,F),"))"].

logic(SELF,Class,boolean,Op,L,R) ->
    ["(",formula_(SELF,Class,boolean,L),") ",
     Op, 
     " (",formula_(SELF,Class,boolean,R),")"];
logic(SELF,Class,_IntegerType,Op,L,R) ->
    ["?B2I((",formula_(SELF,Class,boolean,L),") ",
     Op, 
     " (",formula_(SELF,Class,boolean,R),"))"].


mid(_SELF,[ID,".",_FLD]) -> ID;
mid(SELF, _) -> SELF.

fld([_ID,".",FLD]) -> FLD;
fld(ID) -> ID.

fid(_SELF,[ID,".",FLD]) -> [ID,"_",FLD];
fid(SELF,ID) -> [SELF,"_",ID].
