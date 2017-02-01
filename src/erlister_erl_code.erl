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
     loop(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[])
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
     loop(ID,IN,DEF,OUT,[],[],CLOCKS,Ms)
    ].

declare(ID,_IN,_DEF,_OUT,_STATES,_TRANS,_CLOCKS,_MACHINES) ->
    [
     "-module('",ID,"').",?N,
     "-export([main/0,wait/3,loop/3,final/0]).",?N,
     "-define(BOOL2INT(X), if (X) -> 1; true -> 0 end).",?N,
     ?N
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
     "wait(STATE,IN,OUT) ->",?N,
     ?T,"receive",?N,
     ?T,?T,"{timeout,_Ref,_} -> loop(STATE,IN,OUT)",?E,
     ?T,?T,"INPUT -> loop(STATE,maps:merge(IN,INPUT),OUT)",?N,
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
     ?N,
     "main() -> ",?N,
     [?T,"wait(",?N,
      %% STATES
      ?T,"#{",?N,
      arglist([[?Q,M#machine.name,"_state",?Q, " => ",
		[?Q,hd(state_names(M#machine.states))],?Q] || M <- MACHINES],
	      [?T,?T],",",?N),
      if STATES =:= [] ->
	      [];
	 true ->
	      [?T,?T,?Q,ID,"_state",?Q," => ",
	       [?Q,hd(state_names(STATES))],?Q,?N]
      end,
      ?T, "}",",",?N,
      %% INPUT
      ?T, "#{",?N,
      arglist([ [[?Q,ID,"_",Name,?Q]," => 0"] ||
		  #var{id=Name} <- IN],
	      [?T,?T],",",?N),?N,
      ?T,"}",",",?N,
      %% OUTPUT
      ?T,"#{",?N,
      arglist([[X," => 0"] || X <- OUT_LIST ],
	      [?T,?T],",",?N),
      ?N,
      ?T,"}).",?N
     ]
    ].


loop(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,MACHINES) ->
    OUT_LIST = [{ID,Name} || #var{id=Name,expr=Expr} <- OUT,
			     Expr =/= undefined] ++
	lists:append([ [{M#machine.name,Name} ||
			   {Name,_Type,_,Expr} <- M#machine.out,
			   Expr =/= undefined] ||
			 M <- MACHINES]),
    [
     ?N,
     "loop(",?N,
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
     ?T,"}) ->", ?N,
     %% load input from formula
     [ [?T,["IN_",ID,"_",Name]," = ",formula(ID,in,Name,Expr),",",?N] || 
	 #var{id=Name,expr=Expr} <- IN],

     [ [?T,["DEF_",ID,"_",Name]," = ",formula(ID,def,Expr),",",?N] ||
	 #var{id=Name,expr=Expr} <- DEF, Expr =/= undefined ],

     [ [?T,["CLK_",ID,"_",T]," = ",
	"erlister_rt:timer_read(", "clk_",ID,"_",T,"),",?N]
       || #clock{id=T} <- CLOCKS],

     [ begin
	   Mid = M#machine.name,
	   [ [[?T,["IN_",Mid,"_",Name]," = ",formula(ID,in,Expr),",",?N] ||
		 #var{id=Name,expr=Expr} <- M#machine.in,Expr =/= undefined],
	     [[?T,["DEF_",Mid,"_",Name]," = ",formula(ID,def,Expr),",",?N] ||
		 #var{id=Name,expr=Expr} <- M#machine.def,Expr =/= undefined],
	     [[?T,["CLK_",Mid,"_",T]," = ",
	       "erlister_rt:read_timer(", "clk_",ID,"_",T,"),",?N] ||
		 #clock{id=T} <- M#machine.clocks],
	     code_trans(Mid, M#machine.trans),
	     [ [?T,"OUT1_",[Mid,"_",Name]," = ", 
		formula(Mid,out,Expr),",",?N] || 
		 #var{id=Name,expr=Expr} <- M#machine.out,Expr =/= undefined]
	   ]
       end || M <- MACHINES],
     
     code_trans(ID, TRANS),
     [ [?T,"OUT1_",[ID,"_",Name]," = ", formula(ID,out,Expr),",",?N] || 
	 #var{id=Name,expr=Expr} <- OUT,Expr =/= undefined],

     ?T, "wait(STATE#{",?N,
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
     ?T, "}",",",?N,
     ?T,"IN,",?N,
     %% OUTPUT
     ?T,"OUT#{",?N,
     arglist([ [[?Q,M,"_",Name,?Q]," => ",
		["OUT1_",M,"_",Name]] ||
		 {M,Name} <- OUT_LIST],
	     [?T,?T],",",?N),?N,
     ?T,"}).", ?N
    ].

code_trans(_ID,[]) ->
    [];
code_trans(ID,TRs) ->
    [?T,"ST1_",ID," = ",?N,
     ?T,?T,"case (ST_",ID,") of",?N,
     [[ [?T,?T,[?Q,From,?Q]," ->",?N,
	 [?T,?T,?T,"if",?N],
	 [ begin
	       [?T,?T,?T,?T,formula_(ID,trans,Cond), " ->",?N,
		tlist(ID,Start,[?T,?T,?T,?T,?T],[",",?N]),
		?T,?T,?T,?T,?T,[?Q,To,?Q],?E
	       ]
	   end || {Cond,To,Start} <- FromGroup],
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
    Ls = [{From,_,_,_}|_] = lists:sort(expand_trans(TRs)),
    group_trans_(From, Ls, [], []).

group_trans_(From, [{From,Cond,To,Start}|TRs], FromList, Acc) ->
    group_trans_(From, TRs, [{Cond,To,Start}|FromList], Acc);
group_trans_(From, [{From1,Cond,To,Start}|TRs], FromList, Acc) ->
    group_trans_(From1, TRs, [{Cond,To,Start}], [{From,FromList}|Acc]);
group_trans_(From, [], FromList, Acc) ->
    lists:reverse([{From,FromList}|Acc]).

expand_trans([{From,_Ln0,FromList} | TRs]) ->
    [{From,Cond,To,Start} || {To,_Ln1,Cond,Start} <- FromList] ++
	expand_trans(TRs);
expand_trans([]) ->
    [].

tlist(_ID,[],_Pre,_Sep) -> [];
tlist(ID,[{T,_Ln}|Ts],Pre,Sep) ->
    Timeout = "2000", %% fixme
    [Pre,"erlister_rt:timer_start(clk_",ID,"_",T,",",Timeout,")",Sep | 
     tlist(ID,Ts,Pre,Sep)].

arglist([], _Pre, _Sep, _Del) -> [];
arglist([A], Pre, _Sep, _Del) -> [Pre,A];
arglist([A|As],Pre,Sep,Del) -> [Pre,A,Sep,Del | arglist(As,Pre,Sep,Del)].

formula(SELF,in,Name,undefined) -> ["I_",fid(SELF,Name)];
formula(SELF,Class,_Name,F) -> ["?BOOL2INT(",formula_(SELF,Class,F),")"].

formula(SELF,Class,F) ->
    ["?BOOL2INT(",formula_(SELF,Class,F),")"].

formula_(_SELF,_Class,{const,1})    -> "true";  %% fixme!
formula_(_SELF,_Class,{const,0})    -> "false";  %% fixme!
formula_(_SELF,_Class,{const,C})    -> integer_to_list(C);
formula_(SELF,in,{in,ID,boolean})    ->  ["(I_",fid(SELF,ID)," =/= 0)"];
formula_(SELF,in,{in,ID,_Type})       -> ["I_",fid(SELF,ID)];
formula_(SELF,_Class,{in,ID,boolean})  -> ["(IN_",fid(SELF,ID)," =/= 0)"];
formula_(SELF,_Class,{in,ID,_Type})    -> ["IN_",fid(SELF,ID)];
formula_(SELF,_Class,{out1,ID,boolean}) -> ["(OUT1_",fid(SELF,ID)," =/= 0)"];
formula_(SELF,_Class,{out1,ID,_Type}) -> ["OUT1_",fid(SELF,ID)];
formula_(SELF,_Class,{out,ID,boolean}) -> ["(OUT_",fid(SELF,ID)," =/= 0)"];
formula_(SELF,_Class,{out,ID,_Type}) -> ["OUT_",fid(SELF,ID)];
formula_(SELF,_Class,{def,ID,boolean}) -> ["(DEF_",fid(SELF,ID)," =/= 0)"];
formula_(SELF,_Class,{def,ID,_Type}) -> ["DEF_",fid(SELF,ID)];
formula_(SELF,out,{state,ID}) ->  ["(ST1_",mid(SELF,ID)," =:= ",
				  ?Q,fld(ID),?Q,")"];
formula_(SELF,_Class,{state,ID}) ->  ["(ST_",mid(SELF,ID)," =:= ",
				    ?Q,fld(ID),?Q,")"];
formula_(SELF,_Class,{timeout,ID}) ->
    ["(","CLK_",fid(SELF,ID)," =:= timeout)"];
formula_(SELF,Class,{'&&',L,R}) ->
    ["(",formula_(SELF,Class,L),") andalso (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'||',L,R}) ->
    ["(",formula_(SELF,Class,L),") orelse (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'->',L,R}) ->
    ["not (",formula_(SELF,Class,L),") orelse (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'<->',L,R}) ->
    ["(",formula_(SELF,Class,L),") =:= (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'<',L,R}) ->
    ["(",formula_(SELF,Class,L),") < (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'<=',L,R}) ->
    ["(",formula_(SELF,Class,L),") =< (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'>',L,R}) ->
    ["(",formula_(SELF,Class,L),") > (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'>=',L,R}) ->
    ["(",formula_(SELF,Class,L),") >= (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'==',L,R}) ->
    ["(",formula_(SELF,Class,L),") =:= (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'!=',L,R}) ->
    ["(",formula_(SELF,Class,L),") =/= (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'+',L,R}) ->
    ["(",formula_(SELF,Class,L),") + (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'-',L,R}) -> 
    ["(",formula_(SELF,Class,L),") - (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'*',L,R}) -> 
    ["(",formula_(SELF,Class,L),") * (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'/',L,R}) -> 
    ["(",formula_(SELF,Class,L),") div (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'%',L,R}) -> 
    ["(",formula_(SELF,Class,L),") rem (",formula_(SELF,Class,R),")"];
formula_(SELF,Class,{'-',F}) -> 
    ["-(",formula_(SELF,Class,F),")"];
formula_(SELF,Class,{'!',F}) -> 
    ["not (",formula_(SELF,Class,F),")"].

mid(_SELF,[ID,".",_FLD]) -> ID;
mid(SELF, _) -> SELF.

fld([_ID,".",FLD]) -> FLD;
fld(ID) -> ID.

fid(_SELF,[ID,".",FLD]) -> [ID,"_",FLD];
fid(SELF,ID) -> [SELF,"_",ID].
