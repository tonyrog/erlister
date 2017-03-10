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
     "#include <stdint.h>",?N,
     "typedef uint8_t  boolean",?E,
     "typedef uint8_t  unsigned8",?E,
     "typedef uint16_t unsigned16",?E,
     "typedef uint32_t unsigned32",?E,
     "typedef int8_t   integer8",?E,
     "typedef int16_t  integer16",?E,
     "typedef int32_t  integer32",?E,
     "typedef unsigned long  timer_t",?E,
     "typedef unsigned char  state_t",?E,
     ?N,
     %% enum_in(ID, IN),?N,
     %% enum_out(ID, OUT, [{M#machine.name,M#machine.out}||M<-MACHINES]),?N,
     enum_state(ID, STATES),?N,
     [ [enum_state(M#machine.name, M#machine.states),?N] || M <- MACHINES ],
     [ "typedef struct {", ?N,
       [ [ ?T, atom_to_list(Type), " ", "in_",ID,"_",Name,?E] ||
	   #var{id=Name,type=Type} <- IN],
       [ [ ?T, atom_to_list(Type), " ", "out_",ID,"_",Name,?E] ||
	   #var{id=Name,type=Type} <- OUT],
       [ [[?T,"timer_t   ", "clk_",M#machine.name,"_",T,?E] ||
	     #clock{id=T} <- M#machine.clocks] || M <- MACHINES ],
       [ [?T,"timer_t  clk_",ID,"_",T,?E] || #clock{id=T} <- CLOCKS],
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
	   #clock{id=T} <- M#machine.clocks] || M <- MACHINES ],
     [[?T,"timer_stop(&ctx->clk_",ID,"_",T,")",?E] || #clock{id=T} <- CLOCKS],
     "}",?N
    ].


init(ID,IN,_DEF,OUT,STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     ?N,
     "void init(", ID, "_ctx_t* ctx)",?N,
     "{",?N,
     %% load input from formula
     [ [?T,["ctx->in_",ID,"_",Name]," = 0",?E] ||
	 #var{id=Name} <- IN],
     [ [?T,"ctx->out_",[ID,"_",Name]," = 0",?E] ||
	 #var{id=Name,expr=Expr} <- OUT,Expr =/= undefined],

     [ [[?T,"timer_init(&ctx->clk_",M#machine.name,"_",T,")",?E] ||
	   #clock{id=T} <- M#machine.clocks] || M <- MACHINES ],
     [[?T,"timer_init(&ctx->clk_",ID,"_",T,")",?E] || #clock{id=T} <- CLOCKS],
     
     [if M#machine.states =:= [] -> [];
	 true -> [?T,"ctx->st_",M#machine.name," = ",
		  [M#machine.name,"_",hd(state_names(M#machine.states))],?E]
      end || M <- MACHINES],
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
     [ [?T,atom_to_list(Type)," ",[ID,"_",Name], ?E] || 
	 #var{id=Name,type=Type} <- IN],
     [ [?T,atom_to_list(Type)," ", [ID,"_",Name], ?E] || 
	 #var{id=Name,type=Type,expr=Expr} <- DEF,Expr =/= undefined],
     [ [[?T,atom_to_list(Type)," ", [M#machine.name,"_",Name], ?E] || 
	   #var{id=Name,type=Type} <- M#machine.in] || M <- MACHINES],
     ?N,
     %% load input from formula
     [ [?T,[ID,"_",Name]," = ",formula(ID,in,Name,Expr),?E] ||
	 #var{id=Name,expr=Expr} <- IN],

     [ [?T,[ID,"_",Name]," = ",formula(ID,def,Expr),?E] ||
	 #var{id=Name,expr=Expr} <- DEF,Expr =/= undefined ],

     [ begin
	   Mid = M#machine.name,
	   [ [[?T,[Mid,"_",Name]," = ",formula(ID,in,Pred),?E] || 
		 #var{id=Name,expr=Pred} <- M#machine.in,Pred =/= undefined],
	     [[?T,[Mid,"_",Name]," = ",formula(ID,def,Sat),?E] || 
		 #var{id=Name,expr=Sat} <- M#machine.def,Sat =/= undefined],
	     code_trans(Mid, M#machine.trans),
	     [ [?T,"ctx->out_",[Mid,"_",Name]," = ",
		formula(Mid,out,Sat),?E] || 
		 #var{id=Name,expr=Sat} <- M#machine.out,Sat =/= undefined]
	   ]
       end || M <- MACHINES],
     
     code_trans(ID, TRANS),
     [ [?T,"ctx->out_",[ID,"_",Name]," = ",
	formula(ID,out,Sat),?E] ||
	 #var{id=Name,expr=Sat} <- OUT,Sat =/= undefined],
     "}",?N
    ].

code_trans(_ID,[]) ->
    [];
code_trans(ID,TRs) ->
    [?T,"switch(ctx->st_",ID,") {",?N,
     [[ [?T,"case ",[ID,"_",From],":",?N,
	 [ [?T,?T,"if (", formula(ID,trans,Cond), ") {",?N,
	    ?T,?T,?T,"ctx->st_",ID," = ",[ID,"_",To],";",
	    tlist(ID,Start,[?N,?T,?T,?T]),
	    action(Action,[?N,?T,?T,?T]),
	    ?N,
	    ?T,?T,?T,"break",?E,
	    ?T,?T,"}",?N] || {Cond,To,Start,Action} <- FromGroup],
	 [?T,?T,"break",?E]]
      ] || {From,FromGroup} <- group_trans(TRs)],
     [?T,"default: break",?E],
     [?T,"}",?N]].

     
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
    
enum_state(_MID, []) -> 
    [];
enum_state(MID, STATES) ->
    States = [[MID,"_",S] || #state{id=S} <- STATES],
    ["enum {",?N,
     arglist(States,?T,",",?N),?N,
     "}", ?E].

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

tlist(_ID,[],_Sep) -> [];
tlist(ID,[{T,_Ln}|Ts],Sep) ->
    [Sep,"timer_start(&ctx->clk_",ID,"_",T,");" | tlist(ID,Ts,Sep)].

action([],_Sep) -> [];
action([A|As],Sep) ->
    [Sep,"// ",io_lib:format("~p;",[A]) | action(As,Sep)].

arglist([], _Pre, _Sep, _Del) -> [];
arglist([A], Pre, _Sep, _Del) -> [Pre,A];
arglist([A|As],Pre,Sep,Del) -> [Pre,A,Sep,Del | arglist(As,Pre,Sep,Del)].

formula(SELF,in,Name,undefined) -> ["ctx->in_",fid(SELF,Name)];
formula(SELF,Class,_Name,F) -> formula(SELF,Class,F).

formula(_SELF,_Class,{const,true})     -> "1";
formula(_SELF,_Class,{const,false})     -> "0";
formula(_SELF,_Class,{const,C})        -> integer_to_list(C);
formula(SELF,trans,{in,ID,boolean})    -> fid(SELF,ID);
formula(SELF,trans,{in,ID,_Type})      -> fid(SELF,ID);
formula(SELF,out,{in,ID,boolean})      -> fid(SELF,ID);
formula(SELF,out,{in,ID,_Type})        -> fid(SELF,ID);
formula(SELF,def,{in,ID,boolean})      -> fid(SELF,ID);
formula(SELF,def,{in,ID,_Type})        -> fid(SELF,ID);
formula(SELF,_Class,{in,ID,boolean})   -> ["ctx->in_",fid(SELF,ID)];
formula(SELF,_Class,{in,ID,_Type})     -> ["ctx->in_",fid(SELF,ID)];
formula(SELF,_Class,{out1,ID,boolean}) -> ["ctx->out_",fid(SELF,ID)];
formula(SELF,_Class,{out1,ID,_Type})   -> ["ctx->out_",fid(SELF,ID)];
formula(SELF,_Class,{out,ID,boolean})  -> ["ctx->out_",fid(SELF,ID)];
formula(SELF,_Class,{out,ID,_Type})    -> ["ctx->out_",fid(SELF,ID)];
formula(SELF,_Class,{def,ID,boolean})  -> fid(SELF,ID);
formula(SELF,_Class,{def,ID,_Type})    -> fid(SELF,ID);
formula(SELF,_Class,{state,ID}) ->  ["(ctx->st_",mid(SELF,ID)," == ",fid(SELF,ID),")"];
formula(SELF,_Class,{timeout,ID}) ->
    ["timer_timeout(&ctx->clk_",fid(SELF,ID),")"];
formula(SELF,Class,{'?',C,T,E}) ->
    [ "(",formula(SELF,Class,C), ") ? (", formula(SELF,Class,T), ") : (",
      formula(SELF,Class,E), ")"];
formula(SELF,Class,{'&&',L,R}) -> logic(SELF,Class,"&&",L,R);
formula(SELF,Class,{'||',L,R}) -> logic(SELF,Class,"||",L,R);
formula(SELF,Class,{'->',L,R}) -> logic(SELF,Class,"||",{'!',L},R);
formula(SELF,Class,{'<->',L,R}) -> logic(SELF,Class,"==",L,R);
formula(SELF,Class,{'<',L,R}) -> comp(SELF,Class,"<",L,R);
formula(SELF,Class,{'<=',L,R}) -> comp(SELF,Class,"<=",L,R);
formula(SELF,Class,{'>',L,R}) -> comp(SELF,Class,">",L,R);
formula(SELF,Class,{'>=',L,R}) -> comp(SELF,Class,">=",L,R);
formula(SELF,Class,{'==',L,R}) -> comp(SELF,Class,"==",L,R);
formula(SELF,Class,{'!=',L,R}) -> comp(SELF,Class,"!=",L,R);
formula(SELF,Class,{'+',L,R}) -> arith(SELF,Class,"+",L,R);
formula(SELF,Class,{'-',L,R}) -> arith(SELF,Class,"-",L,R);
formula(SELF,Class,{'*',L,R}) -> arith(SELF,Class,"*",L,R);
formula(SELF,Class,{'/',L,R}) -> arith(SELF,Class,"/",L,R);
formula(SELF,Class,{'%',L,R}) -> arith(SELF,Class,"%",L,R);
formula(SELF,Class,{'&',L,R}) -> arith(SELF,Class,"&",L,R);
formula(SELF,Class,{'|',L,R}) -> arith(SELF,Class,"|",L,R);
formula(SELF,Class,{'^',L,R}) -> arith(SELF,Class,"^",L,R);
formula(SELF,Class,{'<<',L,R}) -> arith(SELF,Class,"<<",L,R);
formula(SELF,Class,{'>>',L,R}) -> arith(SELF,Class,">>",L,R);
formula(SELF,Class,{'-',F}) -> arith(SELF,Class,"-",F);
formula(SELF,Class,{'~',F}) -> arith(SELF,Class,"~",F);
formula(SELF,Class,{'!',F}) -> arith(SELF,Class,"!",F).

logic(SELF,Class,OP,L,R) ->
    ["(",formula(SELF,Class,L),") ",OP," (",formula(SELF,Class,R),")"].

comp(SELF,Class,OP,L,R) ->
    ["(",formula(SELF,Class,L),") ",OP," (",formula(SELF,Class,R),")"].

arith(SELF,Class,OP,F) ->
    [OP,"(",formula(SELF,Class,F),")"].

arith(SELF,Class,OP,L,R) ->
    ["(",formula(SELF,Class,L),") ",OP," (",formula(SELF,Class,R),")"].

mid(_SELF,[ID,".",_FLD]) -> ID;
mid(SELF, _) -> SELF.

fid(_SELF,[ID,".",FLD]) -> [ID,"_",FLD];
fid(SELF,ID) -> [SELF,"_",ID].
