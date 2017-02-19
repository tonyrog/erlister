%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Generate erlister seazone code
%%% @end
%%% Created :  16 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister_sz_code).

-export([code/1]).

-include("../include/erlister.hrl").

code(M) ->
    Code = code_(M),
    FlatCode = lists:flatten(Code),
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
     [ [[{timer_stop,id([M#machine.name,T])}] ||
	   #clock{id=T} <- M#machine.clocks] || M <- MACHINES ],
     [[{timer_stop,id([ID,T])}] || #clock{id=T} <- CLOCKS],
     exit
    ].

init(ID,_IN,_DEF,_OUT,STATES,_TRANS,CLOCKS,MACHINES) ->
    [
     {label,init},
     [ [[{timer_init,id([M#machine.name,T])}] ||
	   #clock{id=T} <- M#machine.clocks] || M <- MACHINES],
     [[{timer_init,id([ID,T])}] || #clock{id=T} <- CLOCKS],
     
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
     [ [{def,id([ID,Name])},formula(ID,def,Expr),'!'] ||
	 #var{id=Name,expr=Expr} <- DEF,Expr =/= undefined ],
     [ begin
	   Mid = M#machine.name,
	   [ [[{def,id([Mid,Name])},formula(ID,in,Pred),'!'] || 
		 #var{id=Name,expr=Pred} <- M#machine.in,Pred =/= undefined],
	     [[{def,id([Mid,Name])},formula(ID,def,Sat),'!'] || 
		 #var{id=Name,expr=Sat} <- M#machine.def,Sat =/= undefined],
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
     exit, %% FIME: bad branch
     [[
       [[{label,id([ID,From,I])},
	 formula(ID,trans,Cond),
	 {zbranch,id([ID,From,I+1])},
	 {state,ID},{const,id([ID,To])},'!',
	 tlist(ID,Start),
	 %% FIXME: add actions,
	 {branch,id([ID,"out"])}]
	|| {{Cond,To,Start},I} <-
	       lists:zip(FromGroup,lists:seq(1,length(FromGroup)))],
       {label,id([ID,From,length(FromGroup)+1])}
       %% FIXME: setup input/timer mask
       %% leave
      ]
      || {From,FromGroup} <- GroupTrans],
     {label,id([ID,"out"])}].
     
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
    [{timer_start,id([ID,T])} | tlist(ID,Ts)].

%% formula(SELF,in,Name,undefined) -> {input,fid(SELF,Name)};%
% formula(SELF,Class,_Name,F) -> formula(SELF,Class,F).

formula(_SELF,_Class,{const,true})    -> -1;
formula(_SELF,_Class,{const,false})   -> 0;
formula(_SELF,_Class,{const,C})       -> C;
formula(SELF,_Class,{in,ID,Type})     -> {input,fid(SELF,ID),Type};
formula(SELF,_Class,{out1,ID,Type})   -> {output1,fid(SELF,ID),Type};
formula(SELF,_Class,{out,ID,Type})    -> {output,fid(SELF,ID),Type};
formula(SELF,_Class,{def,ID,_Type})   -> [{def,fid(SELF,ID)},'@'];
formula(SELF,_Class,{state,ID})       -> [{state,mid(SELF,ID)},'@',
					  {const,fid(SELF,ID)},'='];
formula(SELF,_Class,{timeout,ID})     -> {timeout,fid(SELF,ID)};
formula(SELF,Class,{'&&',L,R}) ->
    Label = new_label(),
    [formula(SELF,Class,L), dup, {zbranch,Label},
     drop, formula(SELF,Class,R),
     {label,Label}];
formula(SELF,Class,{'||',L,R}) ->
    Label = new_label(),
    [formula(SELF,Class,L), dup, 'not', '0=', {zbranch,Label},
     drop, formula(SELF,Class,R),
     {label,Label}];
formula(SELF,Class,{'->',L,R}) ->
    Label = new_label(),
    [formula(SELF,Class,L), dup, '0=', {zbranch,Label},
     drop, formula(SELF,Class,R),
     {label,Label}];
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
formula(S,C,{'%',L,R}) -> [formula(S,C,L),formula(S,C,R),mod];
formula(S,C,{'&',L,R}) -> [formula(S,C,L),formula(S,C,R),'and'];
formula(S,C,{'|',L,R}) -> [formula(S,C,L),formula(S,C,R),'or'];
formula(S,C,{'^',L,R}) -> [formula(S,C,L),formula(S,C,R),'xor'];
formula(S,C,{'<<',L,R}) -> [formula(S,C,L),formula(S,C,R),lshift];
formula(S,C,{'>>',L,R}) -> [formula(S,C,L),formula(S,C,R),arshift];
formula(S,C,{'-',F}) -> [formula(S,C,F),negate];
formula(S,C,{'~',F}) -> [formula(S,C,F),invert];
formula(S,C,{'!',F}) -> [formula(S,C,F),'not'].

mid(_SELF,[ID,".",_FLD]) -> ID;
mid(SELF, _) -> SELF.

fid(_SELF,[ID,".",FLD]) -> id([ID,FLD]);
fid(SELF,ID) -> id([SELF,ID]).

id([A]) -> to_string(A);
id([A|As]) -> to_string(A)++"_"++id(As).

to_string(A) when is_atom(A) -> atom_to_list(A);
to_string(A) when is_integer(A),A>=0 -> integer_to_list(A);
to_string(A) when is_list(A) -> A.
    

new_label() ->
    L = case get(next_label) of
	    undefined -> 1;
	    L0 -> L0+1
	end,
    put(next_label, L+1),
    L.

    
	    
