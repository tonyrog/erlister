%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Generate erlister C code
%%% @end
%%% Created :  16 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister_c_code).

-export([code/1]).

-include("../include/erlister.hrl").

-define(T, "    ").
-define(N, "\n").
-define(E, ";\n").

code(#machine{name=ID,in=IN,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=undefined,
	      states=STATES,trans=TRANS}) ->
    [
     declare(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     init(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     final(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
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
     main(ID,IN,DEF,OUT,[],[],CLOCKS,Ms)
    ].

declare(ID,IN,_DEF,OUT,STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     "typedef unsigned char  digital_t",?E,
     "typedef unsigned short analog_t",?E,
     "typedef unsigned long  timer_t",?E,
     "typedef unsigned char  state_t",?E,
     ?N,
     enum_in(ID, IN),?N,
     enum_out(ID, OUT, [{M#machine.name,M#machine.out}||M<-MACHINES]),?N,
     enum_state(ID, STATES),?N,
     [ [enum_state(M#machine.name, M#machine.states),?N] || M <- MACHINES ],
     [ "typedef struct {", ?N,
       [ ?T, "digital_t input[",ID,"_","NUM_INPUT]",?E],
       [ ?T, "digital_t output[",ID,"_","NUM_OUTPUT]",?E],
       [ [[?T,"timer_t   ", "clk_",M#machine.name,"_",T,?E] ||
	     {T,_,_} <- M#machine.clocks] || M <- MACHINES ],
       [[?T,"timer_t   clk_",ID,"_",T,?E] || {T,_,_} <- CLOCKS],
       [ [?T,"state_t  st_", M#machine.name,?E] || M <- MACHINES ],
       if STATES =:= [] ->
	       [];
	  true ->
	       [?T, "state_t   st_", ID, ?E]
       end,
       "} ", ID, "_ctx_t", ?E
     ],
     ?N,
     [ "extern int timer_init(timer_t* tp)", ?E],
     [ "extern int timer_start(timer_t* tp)", ?E],
     [ "extern int timer_stop(timer_t* tp)", ?E],
     [ "extern int timer_timeout(timer_t* tp)",?E]
    ].

final(ID,_IN,_DEF,_OUT,_STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     ?N,
     "void final(", ID, "_ctx_t* ctx)",?N,
     "{",?N,
     [ [[?T,"timer_stop(&ctx->clk_",M#machine.name,"_",T,")",?E] ||
	   {T,_,_} <- M#machine.clocks] || M <- MACHINES ],
     [[?T,"timer_stop(&ctx->clk_",ID,"_",T,")",?E] || {T,_,_} <- CLOCKS],
     "}",?N
    ].


init(ID,IN,_DEF,OUT,STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     ?N,
     "void init(", ID, "_ctx_t* ctx)",?N,
     "{",?N,
     %% load input from formula
     [ [?T,["ctx->input[IN_",ID,"_",Name],"] = 0",?E] ||
	 {Name,_,_Pred} <- IN],
     [ [?T,"ctx->output[OUT_",[ID,"_",Name],"] = 0",?E] ||
	 {Name,_,Sat} <- OUT,Sat =/= undefined],

     [ [[?T,"timer_init(&ctx->clk_",M#machine.name,"_",T,")",?E] ||
	   {T,_,_} <- M#machine.clocks] || M <- MACHINES ],
     [[?T,"timer_init(&ctx->clk_",ID,"_",T,")",?E] || {T,_,_} <- CLOCKS],
     
     [ [?T,"ctx->st_",M#machine.name," = ",
	[M#machine.name,"_",hd(state_names(M#machine.states))],?E] ||
	 M <- MACHINES],
     if STATES =:= [] ->
	     [];
	true ->
	     [?T,"ctx->st_",ID," = ",
	      [ID,"_",hd(state_names(STATES))],?E]
     end,
     "}",?N
    ].


main(ID,IN,DEF,OUT,_STATES,TRANS,_CLOCKS,MACHINES) ->
    [
     ?N,
     "void machine(", ID, "_ctx_t* ctx)",?N,
     "{",?N,
     [ [?T,"digital_t ", [ID,"_",Name], ?E] || 
	 {Name,_,_Pred} <- IN],
     [ [?T,"digital_t ", [ID,"_",Name], ?E] || 
	 {Name,_,Sat} <- DEF,Sat =/= undefined],
     [ [[?T,"digital_t ", [M#machine.name,"_",Name], ?E] || 
	   {Name,_,_} <- M#machine.in] || M <- MACHINES],
     ?N,
     %% load input from formula
     [ [?T,[ID,"_",Name]," = ",formula(ID,in,Name,Pred),?E] ||
	 {Name,_,Pred} <- IN],

     [ [?T,[ID,"_",Name]," = ",formula(ID,def,Sat),?E] ||
	 {Name,_,Sat} <- DEF, Sat =/= undefined ],

     [ begin
	   Mid = M#machine.name,
	   [ [[?T,[Mid,"_",Name]," = ",formula(ID,in,Pred),?E] || 
		 {Name,_,Pred} <- M#machine.in,Pred =/= undefined],
	     [[?T,[Mid,"_",Name]," = ",formula(ID,def,Sat),?E] || 
		 {Name,_,Sat} <- M#machine.def,Sat =/= undefined],
	     code_trans(Mid, M#machine.trans),
	     [ [?T,"ctx->output[OUT_",[Mid,"_",Name],"] = ",
		formula(Mid,out,Sat),?E] || 
		 {Name,_,Sat} <- M#machine.out,Sat =/= undefined]
	   ]
       end || M <- MACHINES],
     
     code_trans(ID, TRANS),
     [ [?T,"ctx->output[OUT_",[ID,"_",Name],"] = ",
	formula(ID,out,Sat),?E] || 
	 {Name,_,Sat} <- OUT,Sat =/= undefined],
     "}",?N
    ].

code_trans(_ID,[]) ->
    [];
code_trans(ID,TRs) ->
    [?T,"switch(ctx->st_",ID,") {",?N,
     [[ [?T,"case ",[ID,"_",From],":",?N,
	 [ [?T,?T,"if (", formula(ID,trans,Cond), ") {",?N,
	    ?T,?T,?T,"ctx->st_",ID," = ",
	    [ID,"_",To],";",
	    tlist(ID,Start,[?N,?T,?T,?T]),?N,
	    ?T,?T,?T,"break",?E,
	    ?T,?T,"}",?N] || {Cond,To,Start} <- FromGroup],
	 [?T,?T,"break",?E]]
      ] || {From,FromGroup} <- group_trans(TRs)],
     [?T,"default: break",?E],
     [?T,"}",?N]].

     
enum_in(_MID, []) ->
    [];
enum_in(MID, IN) ->
    Input = [["IN_",MID,"_",I] || {I,_Ln,_Pred} <- IN] ++
	[[MID,"_","NUM_INPUT"]],
    ["enum {",?N,
     arglist(Input,?T,",",?N),?N,
     "}", ?E].

enum_out(_MID, [], _Ms) ->
    [];
enum_out(MID, OUT, Ms) ->
    Ouput = [["OUT_",MID,"_",O] || {O,_Ln,_Sat} <- OUT] ++
	lists:append([ [["OUT_",Mid,"_",Mo] || {Mo,_Ln,_Sat} <- Mout] || {Mid,Mout} <- Ms]) ++
	[[MID,"_","NUM_OUTPUT"]],
    ["enum {",?N,
     arglist(Ouput,?T,",",?N),?N,
     "}", ?E].

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
    [S || {S,_Ln} <- STATES].
    
enum_state(_MID, []) -> 
    [];
enum_state(MID, STATES) ->
    States = [[MID,"_",S] || S <- state_names(STATES)],
    ["enum {",?N,
     arglist(States,?T,",",?N),?N,
     "}", ?E].

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

tlist(_ID,[],_Sep) -> [];
tlist(ID,[{T,_Ln}|Ts],Sep) ->
    [Sep,"timer_start(&ctx->clk_",ID,"_",T,");" | tlist(ID,Ts,Sep)].

arglist([], _Pre, _Sep, _Del) -> [];
arglist([A], Pre, _Sep, _Del) -> [Pre,A];
arglist([A|As],Pre,Sep,Del) -> [Pre,A,Sep,Del | arglist(As,Pre,Sep,Del)].

formula(SELF,in,Name,undefined) -> ["ctx->input[IN_",fid(SELF,Name),"]"];
formula(SELF,Type,_Name,F) -> formula(SELF,Type,F).

formula(_SELF,_Type,0)    -> "0";
formula(_SELF,_Type,1)    -> "1";
formula(SELF,trans,{in,ID})  -> fid(SELF,ID);
formula(SELF,out,{in,ID})    -> fid(SELF,ID);
formula(SELF,def,{in,ID})    -> fid(SELF,ID);
formula(SELF,_Type,{in,ID})  -> ["ctx->input[IN_",fid(SELF,ID),"]"];
formula(SELF,_Type,{out,ID}) -> ["ctx->output[OUT_",fid(SELF,ID),"]"];
formula(SELF,_Type,{def,ID}) -> fid(SELF,ID);
formula(SELF,_Type,{state,ID}) ->  ["(ctx->st_",mid(SELF,ID)," == ",fid(SELF,ID),")"];
formula(SELF,_Type,{timeout,ID}) ->
    ["timer_timeout(&ctx->clk_",fid(SELF,ID),")"];
formula(SELF,Type,{'&&',L,R}) ->
    ["(",formula(SELF,Type,L),") && (",formula(SELF,Type,R),")"];
formula(SELF,Type,{'||',L,R}) ->
    ["(",formula(SELF,Type,L),") || (",formula(SELF,Type,R),")"];
formula(SELF,Type,{'->',L,R}) ->
    ["!(",formula(SELF,Type,L),") || (",formula(SELF,Type,R),")"];
formula(SELF,Type,{'<->',L,R}) -> 
    ["(",formula(SELF,Type,L),") == (",formula(SELF,Type,R),")"];
formula(SELF,Type,{'!',F}) -> 
    ["!(",formula(SELF,Type,F),")"].

mid(_SELF,[ID,".",_FLD]) -> ID;
mid(SELF, _) -> SELF.

fid(_SELF,[ID,".",FLD]) -> [ID,"_",FLD];
fid(SELF,ID) -> [SELF,"_",ID].
