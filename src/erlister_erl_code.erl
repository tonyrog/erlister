%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Generate erlister Erlang code
%%% @end
%%% Created :  9 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister_erl_code).

-export([code/1]).

-include("../include/erlister.hrl").

-define(T, "  ").
-define(N, "\n").
-define(E, ".\n").

code(#machine{name=ID,in=IN,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=undefined,
	      states=STATES,trans=TRANS}) ->
    [
     code_init(ID,ID,IN,OUT,STATES),
     code_update(ID, "main", IN, DEF, OUT, CLOCKS, TRANS)
    ];
code(#machine{name=ID,in=IN,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=SUBMACHINES,
	      machines=MACHINES,
	      states=undefined,trans=undefined}) ->
    [
     code_init(ID,ID,IN,OUT,[]),
     code_update(ID,hd(SUBMACHINES),IN,DEF,OUT,CLOCKS,[]),
     code_machines(SUBMACHINES, MACHINES)
    ].

code_machines([M1,M2|Ms], MACHINES) ->
    M = lists:keyfind(M1,#machine.name, MACHINES),
    [ code_update(M1, M2, M#machine.name, M#machine.def, M#machine.out,
		  M#machine.clocks, M#machine.trans) |
      code_machines([M2|Ms], MACHINES)];
code_machines([M1], MACHINES) ->
    M = lists:keyfind(M1,#machine.name, MACHINES),
    [ code_update(M1, "main", M#machine.in, M#machine.def, M#machine.out,
		  M#machine.clocks, M#machine.trans)].
    
    
code_init(ID,FIRST,IN,OUT,STATES) ->
    IList = [Name || {Name,_Ln,_Pred} <- IN],
    OList = [Name || {Name,_Ln,_Sat} <- OUT],
    S0 = case STATES of
	     [] -> "undefined";
	     [{Sn,_Ln}|_] -> ["s_",Sn]
	 end,
    ["-module(", ID, ")", ?E,
     "-export([main/0])", ?E,
     "-export([main/3])", ?E,
     ?N,
     "main() ->",?N,
     ?T, "main(", S0, ",",
     "#{ ", alist("i_", "=>false", IList), "}", ",", 
     "#{ ", alist("o_", "=>false", OList), "})",?E,
     ?N,
     "main(S0,I0,O0) ->",?N,
     ?T,"receive",?N,
     ?T,?T, "{timeout,_Ref,_Name} -> update(S0,I0,O0);",?N,
     ?T,?T, "I -> update_",FIRST,"(S0,maps:merge(I0,I),O0)",?N,
     ?T,"end",?E,
     ?N].

code_update(ID, NEXT, IN, DEF, OUT, CLOCKS, TRANS) ->
    IList = [Name || {Name,_Ln,_Pred} <- IN],
    OList = [Name || {Name,_Ln,_Sat} <- OUT],
    [
     "update_",ID,"(S0,I1 = #{ ",alist("i_",":=","I_",IList), "},","O0) -> ",?N,
     [ [?T,"D_"++D," = ", formula(Sat), ",",?N] ||
	 {D,_,Sat} <- DEF],
     [ [?T,"T_"++T," = ", "read_timer('",T,"')",",",?N] ||
	 {T,_,_Clock} <- CLOCKS],
     ?T,"S1 = case S0 of",?N,
     [ [?T,?T,"s_",From, " when ", 
	formula(Cond), " -> ", tlist(Start),"s_",To,";",?N]
       || {From,Cond,To,Start} <- expand_trans(TRANS) ],
     [?T,?T, "_ -> S0",?N],
     [?T,"end,",?N],
     [ [?T,"O_"++O," = ", formula(OSat), ",",?N] || {O,_,OSat} <- OUT],
     [?T,"O1 = O0#{ ", alist("o_","=>","O_",OList), "},",?N ],
     ?T,"update_",NEXT,"(S1, I1, O1)",?E
    ].

expand_trans([{To,_Ln0,FromList} | TRs]) ->
    [{From,Cond,To,Start} || {From,_Ln1,Cond,Start} <- FromList] ++
	expand_trans(TRs);
expand_trans([]) ->
    [].

tlist([]) -> [];
tlist([{T,_Ln}]) -> ["start_timer('",T,"'), "];
tlist([{T,_Ln}|Ts]) ->
    ["start_timer('",T,"')", "," | tlist(Ts)].
    

alist(_Prefix,_Op,_Postfix,[]) -> [];
alist(Prefix,Op,Postfix,[I]) -> [Prefix,I,Op,Postfix,I];
alist(Prefix,Op,Postfix,[I|Is]) ->
    [Prefix,I,Op,Postfix,I,"," | alist(Prefix,Op,Postfix,Is)].

alist(_Prefix,_Value,[]) -> [];
alist(Prefix,Value,[I]) -> [Prefix,I,Value];
alist(Prefix,Value,[I|Is]) ->
    [Prefix,I,Value,"," | alist(Prefix,Value,Is)].

formula(0)    -> "false";
formula(1)    -> "true";
formula({in,ID})  -> "I_"++ID;
formula({out,ID}) -> "O_"++ID;
formula({def,ID}) -> "D_"++ID;
formula({state,ID}) -> ["(S1 =:=","s_",ID,")"];
formula({timeout,ID}) -> ["(T_"++ID," =:= timeout)"];
formula({'&&',L,R}) -> ["(",formula(L),") andalso (",formula(R),")"];
formula({'||',L,R}) -> ["(",formula(L),") orelse (",formula(R),")"];
formula({'->',L,R}) -> ["not (",formula(L),") orelse (",formula(R),")"];
formula({'<->',L,R}) -> ["(",formula(L),") =:= (",formula(R),")"];
formula({'!',F}) -> ["not (",formula(F),")"].
