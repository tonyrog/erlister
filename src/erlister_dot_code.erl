%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Generate erlister DOT code, to display statemachine in
%%%    program like omnigraffle
%%% @end
%%% Created :  16 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister_dot_code).

-export([code/1]).

-include("../include/erlister.hrl").

-define(T, "    ").
-define(N, "\n").
-define(E, ";\n").

code(#machine{name=ID,in=IN,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=undefined,
	      states=STATES,trans=TRANS}) ->
    [
     main(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[])
    ];
code(#machine{name=ID,in=IN,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=SUBMACHINES,
	      machines=MACHINES,
	      states=undefined,trans=undefined}) ->
    Ms = sort_by_keys(MACHINES,#machine.name,SUBMACHINES),
    [
     main(ID,IN,DEF,OUT,[],[],CLOCKS,Ms)
    ].


main(ID,_IN,_DEF,_OUT,_STATES,TRANS,_CLOCKS,MACHINES) ->
    [
     ?N,
     "digraph machine {",?N,

     [ begin
	   trans(M#machine.name, M#machine.trans)
       end || M <- MACHINES],
     
     trans(ID, TRANS),
     "}",?N
    ].

trans(_ID,[]) ->
    [];
trans(ID,TRs) ->
    [ 
      [[?T,[ID,"_",From]," -> ",[ID,"_",To],
	" [label=\"", formula(ID,trans,Cond), "\"];", ?N] 
       || {Cond,To,_Start,_Action} <- FromGroup]
      || {From,FromGroup} <- group_trans(TRs)].
     
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


formula(_SELF,_Class,{const,true})     -> "true";
formula(_SELF,_Class,{const,false})     -> "false";
formula(_SELF,_Class,{const,C})        -> integer_to_list(C);
formula(SELF,_Class,{in,ID,_Type})     -> fid(SELF,ID);
formula(SELF,_Class,{out1,ID,_Type})   -> fid(SELF,ID);
formula(SELF,_Class,{out,ID,_Type})    -> fid(SELF,ID);
formula(SELF,_Class,{def,ID,_Type})    -> fid(SELF,ID);
formula(SELF,_Class,{state,ID})        ->  mid(SELF,ID);
formula(SELF,_Class,{param,ID,_Index,_Type}) -> fid(SELF,ID);

formula(SELF,_Class,{timeout,ID}) -> ["timeout(",fid(SELF,ID),")"];
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
    pform(SELF,Class,OP,L,R).

comp(SELF,Class,OP,L,R) ->
    pform(SELF,Class,OP,L,R).


arith(SELF,Class,OP,F) ->
    pform(SELF,Class,OP,F).

arith(SELF,Class,OP,L,R) ->
    pform(SELF,Class,OP,L,R).


pform(SELF,Class,OP,F) ->
    [OP,case priority(operator(F)) > priority(OP) of
	    true ->
		["(",formula(SELF,Class,F),")"];
	    false ->
		formula(SELF,Class,F)
	end ].
    
pform(SELF,Class,OP,L,R) ->
    [ case priority(operator(L)) > priority(OP) of
	  true ->
	      ["(",formula(SELF,Class,L),")"];
	  false ->
	      formula(SELF,Class,L)
      end,
      " ",OP," ",
      case priority(operator(R)) > priority(OP) of
	  true ->
	      ["(",formula(SELF,Class,R),")"];
	  false ->
	      formula(SELF,Class,R)
      end].
    
    

mid(_SELF,[ID,".",_FLD]) -> ID;
mid(SELF, _) -> SELF.

fid(_SELF,[ID,".",FLD]) -> [ID,"_",FLD];
fid(SELF,ID) -> [SELF,"_",ID].

operator(T) when is_tuple(T) ->
    atom_to_list(element(1,T)).


priority("->") ->  200;
priority("<->") -> 250;
priority("?") -> 260;    
priority("||") -> 300;
priority("&&") -> 400;
priority("|") -> 410;
priority("^") -> 420;
priority("&") -> 430;
priority("==") -> 440;
priority("!=") -> 440;
priority("<") -> 450;
priority(">") -> 450;
priority("<=") -> 450;
priority(">=") -> 450;
priority("<<") -> 500;
priority(">>") -> 500;
priority("+") -> 600;
priority("-") -> 600;
priority("*") -> 700;
priority("/") -> 700;
priority("%") -> 700;
priority("!") -> 900;
priority("~") -> 900;
priority("ALL") -> 900;
priority("SOME") -> 900;
priority(_) -> 100.

    
