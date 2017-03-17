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

%%  ["param_fetch","(",integer_to_list(Index),",","0",")"];

code(#machine{name=ID,in=IN,param=PAR,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=undefined,
	      states=STATES,trans=TRANS}) ->
    [
     declare(ID,IN,PAR,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     load(ID,PAR,[]),
     init(ID,IN,PAR,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     final(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     run(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[])
    ];
code(#machine{name=ID,in=IN,param=PAR,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=SUBMACHINES,
	      machines=MACHINES,
	      states=undefined,trans=undefined}) ->
    Ms = sort_by_keys(MACHINES,#machine.name,SUBMACHINES),
    [
     declare(ID,IN,PAR,DEF,OUT,[],[],CLOCKS,Ms),
     load(ID,PAR,MACHINES),
     init(ID,IN,PAR,DEF,OUT,[],[],CLOCKS,Ms),
     final(ID,IN,DEF,OUT,[],[],CLOCKS,Ms),
     run(ID,IN,DEF,OUT,[],[],CLOCKS,Ms)
    ].

declare(ID,IN,PAR,_DEF,OUT,STATES,_TRANS,CLOCKS,MACHINES) ->
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
       [ [ [ ?T, atom_to_list(Type), " ", "out_",M#machine.name,"_",Name,?E] ||
	     #var{id=Name,type=Type} <- M#machine.out] ||  M <- MACHINES],

       [ [ ?T, atom_to_list(Type), " ", "par_",ID,"_",Name,?E] ||
	   #var{id=Name,type=Type} <- PAR],
       [ [ [ ?T, atom_to_list(Type), " ", "par_",M#machine.name,"_",Name,?E] ||
	     #var{id=Name,type=Type} <- M#machine.param] ||  M <- MACHINES],

       [ [?T,"timer_t  clk_",ID,"_",T,?E] || #clock{id=T} <- CLOCKS],
       [ [[?T,"timer_t   ", "clk_",M#machine.name,"_",T,?E] ||
	     #clock{id=T} <- M#machine.clocks] || M <- MACHINES ],

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
     "static void final(", ID, "_ctx_t* ctx)",?N,
     "{",?N,
     [ [[?T,"timer_stop(&ctx->clk_",M#machine.name,"_",T,")",?E] ||
	   #clock{id=T} <- M#machine.clocks] || M <- MACHINES ],
     [[?T,"timer_stop(&ctx->clk_",ID,"_",T,")",?E] || #clock{id=T} <- CLOCKS],
     "}",?N
    ].

load(ID,PAR,MACHINES) ->
    [
     ?N,
     "static void load(", ID, "_ctx_t* ctx)",?N,
     "{",?N,
     [ [[?T,"ctx->par_",ID,"_",Name," = ",
	 "param_fetch(",integer_to_list(Index),",0)",?E] ||
	   #var{id=Name,expr=Index} <- M#machine.param] || M <- MACHINES ],
     [[?T,"ctx->par_",ID,"_",Name," = ",
       "param_fetch(",integer_to_list(Index),",0)",?E] ||
	 #var{id=Name,expr=Index} <- PAR],
     "}",?N
    ].

init(ID,IN,_PAR,_DEF,OUT,STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     ?N,
     "static void init(", ID, "_ctx_t* ctx)",?N,
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
     [?T,"load(ctx)",?E],
     "}",?N
    ].



run(ID,IN,DEF,OUT,_STATES,TRANS,_CLOCKS,MACHINES) ->
    [
     ?N,
     "static void run(", ID, "_ctx_t* ctx)",?N,
     "{",?N,
     [ [?T,atom_to_list(Type)," ",[ID,"_",Name], ?E] || 
	 #var{id=Name,type=Type} <- IN],
     [ [?T,atom_to_list(Type)," ", [ID,"_",Name], ?E] || 
	 #var{id=Name,type=Type,expr=Expr} <- DEF,Expr =/= undefined],
     [ [[?T,atom_to_list(Type)," ", [M#machine.name,"_",Name], ?E] || 
	   #var{id=Name,type=Type} <- M#machine.in] || M <- MACHINES],
     ?N,
     %% load input from formula
     [ [?T,[ID,"_",Name]," = ",expr(ID,in,Name,Expr),?E] ||
	 #var{id=Name,expr=Expr} <- IN],

     [ [?T,[ID,"_",Name]," = ",expr(ID,def,Expr),?E] ||
	 #var{id=Name,expr=Expr} <- DEF,Expr =/= undefined ],

     [ begin
	   Mid = M#machine.name,
	   [ [[?T,[Mid,"_",Name]," = ",expr(ID,in,Pred),?E] || 
		 #var{id=Name,expr=Pred} <- M#machine.in,Pred =/= undefined],
	     [[?T,[Mid,"_",Name]," = ",expr(ID,def,Sat),?E] || 
		 #var{id=Name,expr=Sat} <- M#machine.def,Sat =/= undefined],
	     code_trans(Mid, M#machine.trans),
	     [ [?T,"ctx->out_",[Mid,"_",Name]," = ",
		expr(Mid,out,Sat),?E] || 
		 #var{id=Name,expr=Sat} <- M#machine.out,Sat =/= undefined]
	   ]
       end || M <- MACHINES],
     
     code_trans(ID, TRANS),
     [ [?T,"ctx->out_",[ID,"_",Name]," = ",
	expr(ID,out,Sat),?E] ||
	 #var{id=Name,expr=Sat} <- OUT,Sat =/= undefined],
     "}",?N
    ].

code_trans(_ID,[]) ->
    [];
code_trans(ID,TRs) ->
    [?T,"switch(ctx->st_",ID,") {",?N,
     [[ [?T,"case ",[ID,"_",From],":",?N,
	 [ [?T,?T,"if (", expr(ID,trans,Cond), ") {",?N,
	    ?T,?T,?T,"ctx->st_",ID," = ",[ID,"_",To],";",
	    tlist(ID,Start,[?N,?T,?T,?T]),
	    action(ID,Action,[?N,?T,?T,?T]),
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

action(_SELF,[],_Sep) -> [];
action(SELF,[A|As],Sep) ->
    [Sep,"// ",action_(SELF,A),";" | action(SELF,As,Sep)].

action_(_SELF,{decl,#var{id=Var,type=Type,expr=undfined}}) ->
    [atom_to_list(Type)," ",Var];
action_(SELF,{decl,#var{id=Var,type=Type,expr=Expr}}) ->
    [atom_to_list(Type)," ",Var," = ",expr(SELF,action,Expr)];
action_(SELF,{store,#var{id=Var,type=_Type},Expr}) -> 
    [Var," = ",expr(SELF,action,Expr)];
action_(SELF,{call,Func,Args}) ->
    {_RType,Name,ArgTypes} = erlister_lint:find_signature(Func),
    call(SELF,action,Name,ArgTypes,Args).

arglist([], _Pre, _Sep, _Del) -> [];
arglist([A], Pre, _Sep, _Del) -> [Pre,A];
arglist([A|As],Pre,Sep,Del) -> [Pre,A,Sep,Del | arglist(As,Pre,Sep,Del)].

expr(SELF,in,Name,undefined) -> ["ctx->in_",fid(SELF,Name)];
expr(SELF,Class,_Name,F) -> expr(SELF,Class,F).

expr(_SELF,_Class,{const,true})     -> "1";
expr(_SELF,_Class,{const,false})    -> "0";
expr(_SELF,_Class,{const,C})        -> integer_to_list(C);
expr(SELF,trans,{in,ID,_Type})      -> fid(SELF,ID);
expr(SELF,out,{in,ID,_Type})        -> fid(SELF,ID);
expr(SELF,def,{in,ID,_Type})        -> fid(SELF,ID);
expr(SELF,_Class,{in,ID,_Type})     -> ["ctx->in_",fid(SELF,ID)];
expr(SELF,_Class,{out1,ID,_Type})   -> ["ctx->out_",fid(SELF,ID)];
expr(SELF,_Class,{out,ID,_Type})    -> ["ctx->out_",fid(SELF,ID)];
expr(SELF,_Class,{def,ID,_Type})    -> fid(SELF,ID);
expr(_SELF,_Class,{var,ID,_Type})    -> ID;
expr(SELF,_Class,{param,ID,_Index,_Type}) -> ["ctx->par_",fid(SELF,ID)];
expr(SELF,_Class,{state,ID}) ->  ["(ctx->st_",mid(SELF,ID)," == ",fid(SELF,ID),")"];
expr(SELF,_Class,{timeout,ID}) ->
    ["timer_timeout(&ctx->clk_",fid(SELF,ID),")"];
expr(SELF,Class,{call,Func,Args}) ->
    {_RType,Name,ArgTypes} = erlister_lint:find_signature(Func),
    call(SELF,Class,Name,ArgTypes,Args);
expr(SELF,Class,{'?',C,T,E}) ->
    [ "(",expr(SELF,Class,C), ") ? (", expr(SELF,Class,T), ") : (",
      expr(SELF,Class,E), ")"];
expr(SELF,Class,{'&&',L,R}) -> logic(SELF,Class,"&&",L,R);
expr(SELF,Class,{'||',L,R}) -> logic(SELF,Class,"||",L,R);
expr(SELF,Class,{'->',L,R}) -> logic(SELF,Class,"||",{'!',L},R);
expr(SELF,Class,{'<->',L,R}) -> logic(SELF,Class,"==",L,R);
expr(SELF,Class,{'<',L,R}) -> comp(SELF,Class,"<",L,R);
expr(SELF,Class,{'<=',L,R}) -> comp(SELF,Class,"<=",L,R);
expr(SELF,Class,{'>',L,R}) -> comp(SELF,Class,">",L,R);
expr(SELF,Class,{'>=',L,R}) -> comp(SELF,Class,">=",L,R);
expr(SELF,Class,{'==',L,R}) -> comp(SELF,Class,"==",L,R);
expr(SELF,Class,{'!=',L,R}) -> comp(SELF,Class,"!=",L,R);
expr(SELF,Class,{'+',L,R}) -> arith(SELF,Class,"+",L,R);
expr(SELF,Class,{'-',L,R}) -> arith(SELF,Class,"-",L,R);
expr(SELF,Class,{'*',L,R}) -> arith(SELF,Class,"*",L,R);
expr(SELF,Class,{'/',L,R}) -> arith(SELF,Class,"/",L,R);
expr(SELF,Class,{'%',L,R}) -> arith(SELF,Class,"%",L,R);
expr(SELF,Class,{'&',L,R}) -> arith(SELF,Class,"&",L,R);
expr(SELF,Class,{'|',L,R}) -> arith(SELF,Class,"|",L,R);
expr(SELF,Class,{'^',L,R}) -> arith(SELF,Class,"^",L,R);
expr(SELF,Class,{'<<',L,R}) -> arith(SELF,Class,"<<",L,R);
expr(SELF,Class,{'>>',L,R}) -> arith(SELF,Class,">>",L,R);
expr(SELF,Class,{'-',F}) -> arith(SELF,Class,"-",F);
expr(SELF,Class,{'~',F}) -> arith(SELF,Class,"~",F);
expr(SELF,Class,{'!',F}) -> arith(SELF,Class,"!",F).

expr_list(_SELF,_Class,[]) -> [];
expr_list(SELF,Class,[E]) -> expr(SELF,Class,E);
expr_list(SELF,Class,[E|Es]) ->
    [expr(SELF,Class,E),",",expr_list(SELF,Class,Es)].
    
logic(SELF,Class,OP,L,R) ->  pform(SELF,Class,OP,L,R).

comp(SELF,Class,OP,L,R) -> pform(SELF,Class,OP,L,R).

arith(SELF,Class,OP,F) -> pform(SELF,Class,OP,F).

arith(SELF,Class,OP,L,R) -> pform(SELF,Class,OP,L,R).

pform(SELF,Class,OP,F) ->
    [OP,pf(SELF,Class,{OP,1},F)].
    
pform(SELF,Class,OP,L,R) ->
    [ pf(SELF,Class,{OP,2},L)," ",OP," ",pf(SELF,Class,{OP,2},R) ].

pf(SELF,Class,OPP,F) ->
    PrioOp   = priority(OPP),
    PrioF = priority(operator(F)),
    case (PrioF>0) andalso (PrioOp > PrioF) of
	true ->
	    ["(",expr(SELF,Class,F),")"];
	false ->
	    expr(SELF,Class,F)
    end.

mid(_SELF,[ID,".",_FLD]) -> ID;
mid(SELF, _) -> SELF.

fid(_SELF,[ID,".",FLD]) -> [ID,"_",FLD];
fid(SELF,ID) -> [SELF,"_",ID].

%%
%% fixme: control output style with options
%%  right now we assume arduino/wiringPI (for fun)
%%    
call(SELF,Class,Name,_ArgTypes,Args) ->
    case Name of
	param_fetch ->
	    ["param_fetch","(",expr_list(SELF,Class,Args),")"];
	param_store ->
	    ["param_store","(",expr_list(SELF,Class,Args),")"];
	uart_send ->
	    ["Serial.write","(",expr_list(SELF,Class,Args),")"];
	uart_recv ->
	    ["Serial.read","(",expr_list(SELF,Class,Args),")"];
	timer_now ->
	    ["millis","(",expr_list(SELF,Class,Args),")"];
	gpio_input ->
	    ["pinMode","(",expr_list(SELF,Class,Args),",INPUT)"];
	gpio_output ->
	    ["pinMode","(",expr_list(SELF,Class,Args),",OUTPUT)"];
	gpio_set ->
	    ["digitalWrite","(",expr_list(SELF,Class,Args),",HIGH)"];
	gpio_clr ->
	    ["digitalWrite","(",expr_list(SELF,Class,Args),",LOW)"];
	gpio_get ->
	    ["digitalRead","(",expr_list(SELF,Class,Args),")"];
	analog_send ->
	    ["analogWrite","(",expr_list(SELF,Class,Args),">>8",")"];
	analog_recv ->
	    ["analogRead","(",expr_list(SELF,Class,Args),") << 6"]
    end.

operator(T) when is_tuple(T) ->
    A = tuple_size(T)-1,
    case element(1,T) of
	'&&' -> {"&&",A};
	'||' -> {"||",A};
	'!' -> {"!",A};
	'<' -> {"<",A};
	'<=' -> {"<=",A};
	'>' -> {">",A};
	'>=' -> {">=",A};
	'==' -> {"==",A};
	'!=' -> {"!=",A};
	'+' -> {"+",A};
	'-' -> {"-",A};
	'*' -> {"*",A};
	'/' -> {"/",A};
	'%' -> {"%",A};
	'&' -> {"&",A};
	'|' -> {"|",A};
	'^' -> {"^",A};
	'<<' -> {"<<",A};
	'>>' -> {">>",A};
	'~' -> {"~",A};
	_ -> {undefined,0}
    end.

priority({"->",2}) ->  200;
priority({"<->",2}) -> 250;
priority({"?",3}) -> 260;    
priority({"||",2}) -> 300;
priority({"&&",2}) -> 400;
priority({"|",2}) -> 410;
priority({"^",2}) -> 420;
priority({"&",2}) -> 430;
priority({"==",2}) -> 440;
priority({"!=",2}) -> 440;
priority({"<",2}) -> 450;
priority({">",2}) -> 450;
priority({"<=",2}) -> 450;
priority({">=",2}) -> 450;
priority({"<<",2}) -> 500;
priority({">>",2}) -> 500;
priority({"+",2}) -> 600;
priority({"-",2}) -> 600;
priority({"*",2}) -> 700;
priority({"/",2}) -> 700;
priority({"%",2}) -> 700;
priority({"!",1}) -> 900;
priority({"~",1}) -> 900;
priority({"+",1}) -> 900;
priority({"-",1}) -> 900;
priority({"ALL",_}) -> 900;
priority({"SOME",_}) -> 900;
priority(_) -> 0.
