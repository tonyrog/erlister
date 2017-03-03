%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Generate erlister chine code
%%% @end
%%% Created :  16 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister_chine_code).

-export([code/1]).

-include("../include/erlister.hrl").

code(M) ->
    Code = code_(M),
    FlatCode = code_flatten(Code),
    io_lib:format("~p\n", [FlatCode]).

code_(#machine{name=ID,in=IN,def=DEF,out=OUT,clocks=CLOCKS,
	      submachines=undefined,
	      states=STATES,trans=TRANS}) ->
    [
     declare(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     init(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     final(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[]),
     main(ID,IN,DEF,OUT,STATES,TRANS,CLOCKS,[])
    ];
code_(#machine{name=ID,in=IN,def=DEF,out=OUT,clocks=CLOCKS,
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

declare(ID,_IN,_DEF,_OUT,STATES,_TRANS,_CLOCKS,MACHINES) ->
    [
     {enum,[ID | [M#machine.name || M <- MACHINES]]},
     enum_state(ID,STATES),
     [ enum_state(M#machine.name, M#machine.states) || M <- MACHINES ]
    ].

final(ID,_IN,_DEF,_OUT,_STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     {label,final},
     [ [[{const,id([M#machine.name,T])},timer_stop] ||
	   #clock{id=T} <- M#machine.clocks] || M <- MACHINES ],
     [[{const,id([ID,T])},timer_stop] || #clock{id=T} <- CLOCKS],
     exit
    ].

init(ID,_IN,_DEF,_OUT,STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     {label,init},
     [ [[{const,id([M#machine.name,T])},timer_init] ||
	   #clock{id=T} <- M#machine.clocks] || M <- MACHINES],
     [[{const,id([ID,T])},timer_init] || #clock{id=T} <- CLOCKS],
     
     [if M#machine.states =:= [] -> [];
	 true -> [{state,M#machine.name},
		  {const,id([M#machine.name,
			     hd(state_names(M#machine.states))])},'!']
	 end || M <- MACHINES],
     if STATES =:= [] ->
	     [];
	true ->
	     [{state,ID},{const,id([ID,hd(state_names(STATES))])},'!']
     end,
     exit
    ].

main(ID,_IN,DEF,OUT,_STATES,TRANS,_CLOCKS,MACHINES) ->
    [
     {label, main},
     [ begin 
	   put({def,fid(ID,Name)},Expr),
	   [{def,id([ID,Name])},formula(ID,def,Expr),'!'] 
       end ||
	 #var{id=Name,expr=Expr} <- DEF,Expr =/= undefined ],
     [ begin
	   Mid = M#machine.name,
	   [ [ begin 
		   [{def,id([Mid,Name])},formula(ID,in,Pred),'!'] 
	       end || 
		 #var{id=Name,expr=Pred} <- M#machine.in,Pred =/= undefined],
	     [begin
		  put({def,fid(Mid,Name)},Expr),
		  [{def,id([Mid,Name])},formula(ID,def,Expr),'!']
	      end || 
		 #var{id=Name,expr=Expr} <- M#machine.def,Expr =/= undefined],
	     code_trans(Mid, M#machine.trans),
	     [ [{output,id([Mid,Name])},
		formula(Mid,out,Sat),'!'] || 
		 #var{id=Name,expr=Sat} <- M#machine.out,Sat =/= undefined]
	   ]
       end || M <- MACHINES],
     code_trans(ID, TRANS),
     [ [{output,id([ID,Name])},
	formula(ID,out,Sat),'!'] ||
	 #var{id=Name,expr=Sat} <- OUT,Sat =/= undefined],
     exit
    ].

code_trans(_ID,[]) ->
    [];
code_trans(ID,TRs) ->
    GroupTrans = group_trans(TRs),
    Fs = [id([ID,F,1]) || {F,_} <- GroupTrans],
    [{state,ID},'@',
     {ibranch,Fs},
     exit, %% FIME: bad branch/fail ?
     code_from_group(ID, GroupTrans)].

code_from_group(ID, [{From,FromGroup}|GroupTrans]) ->
    Set = {ordsets:new(),ordsets:new()},
    [ code_to_group(ID, From, 1, Set, [], FromGroup) | 
      code_from_group(ID,GroupTrans)];
code_from_group(ID, []) ->
    [{label,id([ID,"out"])}].

code_to_group(ID, From, I, Set, Acc, [{Cond,To,Start} | ToGroup]) ->
    Set1 = select(ID,Set,Cond),
    Acc1 = [[{label,id([ID,From,I])},
	     formula(ID,trans,Cond),
	     {zbranch,id([ID,From,I+1])},
	     {state,ID},{const,id([ID,To])},'!',
	     tlist(ID,Start),
	     %% FIXME: add actions,
	     {branch,id([ID,"out"])}] | Acc],
    code_to_group(ID, From, I+1, Set1, Acc1, ToGroup);
code_to_group(ID, From, N, {ISet,TSet}, Acc, []) ->
    %% When all transitions fails then we end up here, here we have
    %% computed Set = {ISet,TSet} sets of inputs and timers that can
    %% effect the state machine. issue select statements that will 
    %% have the effect that the state machine is executed when either
    %% events trigger
    Acc1 = [[{label,id([ID,From,N])},
	     [[{const,id([ID,I])},select_input] || I <- ordsets:to_list(ISet)],
	     [[{const,id([ID,T])},select_timer] || T <- ordsets:to_list(TSet)],
	     yield,
	     {branch,main}
	    ] | Acc],
    lists:reverse(Acc1).
     
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
    {enum,[]};
enum_state(MID, STATES) ->
    States = [id([MID,S]) || #state{id=S} <- STATES],
    {enum, States}.
    
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

tlist(_ID,[]) -> [];
tlist(ID,[{T,_Ln}|Ts]) ->
    [{const,id([ID,T])},timer_start | tlist(ID,Ts)].

%% formula(SELF,in,Name,undefined) -> {input,fid(SELF,Name)};%
% formula(SELF,Class,_Name,F) -> formula(SELF,Class,F).

formula(_SELF,_Class,{const,true})    -> [{const,1}];
formula(_SELF,_Class,{const,false})   -> [{const,0}];
formula(_SELF,_Class,{const,C})       -> [{const,C}];
formula(SELF,_Class,{in,ID,Type})     -> 
    [{const,fid(SELF,ID)},{const,Type},'input@'];
formula(SELF,_Class,{param,ID,_Index,_Type})  ->
    [{const,fid(SELF,ID)},'param@'];
formula(SELF,_Class,{out1,ID,Type})   ->
    [{const,fid(SELF,ID)},{const,Type},'output1@'];
formula(SELF,_Class,{out,ID,Type})    ->
    [{const,fid(SELF,ID)},{const,Type},'output!'];
formula(SELF,_Class,{def,ID,_Type})   ->
    [{def,fid(SELF,ID)},'@'];
formula(SELF,_Class,{state,ID})       ->
    [{state,mid(SELF,ID)},'@',{const,fid(SELF,ID)},'='];
formula(SELF,_Class,{timeout,ID})     ->
    [{const,fid(SELF,ID)},timer_timeout];
formula(SELF,Class,{'?',C,T,E}) ->
    [formula(SELF,Class,C),{'if',formula(SELF,Class,T),formula(SELF,Class,E)}];
formula(SELF,Class,{'&&',L,R}) ->
    [formula(SELF,Class,L), dup,
     {'if', [drop, formula(SELF,Class,R)]}];
formula(SELF,Class,{'||',L,R}) ->
    [formula(SELF,Class,L), dup, 'not',
     {'if', [drop, formula(SELF,Class,R)]}];
formula(SELF,Class,{'->',L,R}) ->
    [formula(SELF,Class,L), dup, '0=', 
     {'if', [drop, formula(SELF,Class,R)]}];
formula(S,C,{'<->',L,R}) ->
    [formula(S,C,L), formula(S,C,R), '='];
formula(S,C,{'<',L,R}) ->
    [formula(S,C,L),formula(S,C,R),'<'];
formula(S,C,{'<=',L,R}) ->
    [formula(S,C,L),formula(S,C,R),'<='];
formula(S,C,{'>',L,R}) ->
    [formula(S,C,R),formula(S,C,L),'<'];
formula(S,C,{'>=',L,R}) ->
    [formula(S,C,R),formula(S,C,L),'<='];
formula(S,C,{'==',L,R}) ->
    [formula(S,C,L),formula(S,C,R),'='];
formula(S,C,{'!=',L,R}) ->
    [formula(S,C,L),formula(S,C,R),'=','not'];
formula(S,C,{'+',L,R}) -> [formula(S,C,L),formula(S,C,R),'+'];
formula(S,C,{'-',L,R}) -> [formula(S,C,L),formula(S,C,R),'-'];
formula(S,C,{'*',L,R}) -> [formula(S,C,L),formula(S,C,R),'*'];
formula(S,C,{'/',L,R}) -> [formula(S,C,L),formula(S,C,R),'/'];
formula(S,C,{'%',L,R}) -> [formula(S,C,L),formula(S,C,R),'mod'];
formula(S,C,{'&',L,R}) -> [formula(S,C,L),formula(S,C,R),'and'];
formula(S,C,{'|',L,R}) -> [formula(S,C,L),formula(S,C,R),'or'];
formula(S,C,{'^',L,R}) -> [formula(S,C,L),formula(S,C,R),'xor'];
formula(S,C,{'<<',L,R}) -> [formula(S,C,L),formula(S,C,R),lshift];
formula(S,C,{'>>',L,R}) -> [formula(S,C,L),formula(S,C,R),arshift];
formula(S,C,{'-',F}) -> [formula(S,C,F),negate];
formula(S,C,{'~',F}) -> [formula(S,C,F),invert];
formula(S,C,{'!',F}) -> [formula(S,C,F),'not'].


%% calculate selection set for inputs and timers
select(_SELF,Set,{const,_C})   -> Set;
select(SELF,{ISet,TSet},{in,ID,_Type})  -> 
    {ordsets:add_element(fid(SELF,ID),ISet),TSet};
select(_SELF,Set,{out1,_ID,_Type}) -> Set;
select(_SELF,Set,{out,_ID,_Type})  -> Set;
select(_SELF,Set,{param,_ID,_Index,_Type})  -> Set;
select(SELF,Set,{def,ID,_Type}) ->
    case get({def,fid(SELF,ID)}) of
	undefined -> Set;
	Expr -> select(SELF,Set,Expr)
    end;
select(_SELF,Set,{state,_ID})     -> Set;
select(SELF,{ISet,TSet},{timeout,ID})   ->
    {ISet,ordsets:add_element(fid(SELF,ID),TSet)};
select(SELF,Set,{Op,L,R}) when is_atom(Op) ->
    select(SELF,select(SELF,Set,L),R);
select(SELF,Set,{Op,F}) when is_atom(Op) ->
    select(SELF,Set,F).


mid(_SELF,[ID,".",_FLD]) -> ID;
mid(SELF, _) -> SELF.

fid(_SELF,[ID,".",FLD]) -> id([ID,FLD]);
fid(SELF,ID) -> id([SELF,ID]).

id([A]) -> to_string(A);
id([A|As]) -> to_string(A)++"_"++id(As).

to_string(A) when is_atom(A) -> atom_to_list(A);
to_string(A) when is_integer(A),A>=0 -> integer_to_list(A);
to_string(A) when is_list(A) -> A.

code_flatten(List) ->
    code_flatten_(lists:flatten(List)).

code_flatten_([{'if',Then}|Code]) ->
    [{'if',code_flatten(Then)} | code_flatten_(Code)];
code_flatten_([{'if',Then,Else}|Code]) ->
    [{'if',code_flatten(Then),code_flatten(Else)} | code_flatten_(Code)];
code_flatten_([Op|Code]) ->
    [Op | code_flatten(Code)];
code_flatten_([]) ->
    [].








