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
    [{const,id([ID,T])},timer_start | tlist(ID,Ts)].

%% formula(SELF,in,Name,undefined) -> {input,fid(SELF,Name)};%
% formula(SELF,Class,_Name,F) -> formula(SELF,Class,F).

formula(_SELF,_Class,{const,true})    -> {const,1};
formula(_SELF,_Class,{const,false})   -> {const,0};
formula(_SELF,_Class,{const,C})       -> {const,C};
formula(SELF,_Class,{in,ID,Type})     -> [{const,fid(SELF,ID)},{const,Type},'input@'];
formula(SELF,_Class,{out1,ID,Type})   -> [{const,fid(SELF,ID)},{const,Type},'output1@'];
formula(SELF,_Class,{out,ID,Type})    -> [{const,fid(SELF,ID)},{const,Type},'output!'];
formula(SELF,_Class,{def,ID,_Type})   -> [{def,fid(SELF,ID)},'@'];
formula(SELF,_Class,{state,ID})       -> [{state,mid(SELF,ID)},'@',
					  {const,fid(SELF,ID)},'='];
formula(SELF,_Class,{timeout,ID})     -> [{const,fid(SELF,ID)},timer_timeout];
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


%% calculate input mask for expression
imask(_SELF,Acc,{const,_C})   -> Acc;
imask(SELF,Acc,{in,ID,_Type})  -> ordsets:add_element(fid(SELF,ID),Acc);
imask(_SELF,Acc,{out1,_ID,_Type}) -> Acc;
imask(_SELF,Acc,{out,_ID,_Type})  -> Acc;
imask(_SELF,Acc,{def,_ID,_Type}) -> Acc; %% FIXME: add(in(fid(SELF,ID)))
imask(_SELF,Acc,{state,_ID})     -> Acc;
imask(_SELF,Acc,{timeout,_ID})   -> Acc;
imask(SELF,Acc,{Op,L,R}) when is_atom(Op) ->
    Acc1 = imask(SELF,Acc,L),
    imask(SELF,Acc1,R);
imask(SELF,Acc,{Op,F}) when is_atom(Op) ->
    imask(SELF,Acc,F).

%% calculate timeout mask for expression
tmask(_SELF,Acc,{const,_C})    -> Acc;
tmask(_SELF,Acc,{in,_ID,_Type})  -> Acc;
tmask(_SELF,Acc,{out1,_ID,_Type}) -> Acc;
tmask(_SELF,Acc,{out,_ID,_Type}) -> Acc;
tmask(_SELF,Acc,{def,_ID,_Type}) -> Acc;
tmask(_SELF,Acc,{state,_ID})       -> Acc;
tmask(SELF,Acc,{timeout,ID})     -> ordsets:add_element(fid(SELF,ID),Acc);
tmask(SELF,Acc,{Op,L,R}) when is_atom(Op) ->
    Acc1 = tmask(SELF,Acc,L),
    tmask(SELF,Acc1,R);
tmask(SELF,Acc,{Op,F}) when is_atom(Op) ->
    tmask(SELF,Acc,F).

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
