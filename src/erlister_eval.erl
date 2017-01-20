%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Erlister eval predicate
%%% @end
%%% Created : 18 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister_eval).

-export([machine/1]).

-include("../include/erlister.hrl").

machine(M = #machine { in=IN }) ->
    IN1 = [{I,Ln,formula(Pred)} || {I,Ln,Pred} <- IN],
    M#machine { in = IN1 }.

formula(undefined) -> undefined;
formula(F) -> formula(F,[]).

formula(0,_Vs) -> 0;
formula(1,_Vs) -> 1;
formula(I={in,_ID},_Vs) -> I;
formula(V={var,_X},_Vs) -> V;
formula({pred,P,As},Vs) -> 
    pred(list_to_atom(P),As,Vs);
formula({'ALL',{var,X},F},Vs) ->
    formula(F, [{all,X}|Vs]);
formula({'SOME',{var,X},F},Vs) ->
    formula(F, [{some,X}|Vs]);
formula({'&&',L,R},Vs) ->
    case formula(L,Vs) of
	0 -> 0;
	1 -> formula(R,Vs);
	L1 ->
	    case formula(R,Vs) of
		0 -> 0;
		1 -> L1;
		R1 -> {'&&',L1,R1}
	    end
    end;
formula({'||',L,R},Vs) ->
    case formula(L,Vs) of
	1 -> 1;
	0 -> formula(R,Vs);
	L1 ->
	    case formula(R,Vs) of
		1 -> 1;
		0 -> L1;
		R1 -> {'||',L1,R1}
	    end
    end;
formula({'->',L,R},Vs) ->
    formula({'||',{'!',L},R}, Vs);
formula({'<->',L,R},Vs) ->
    Li = formula(L,Vs),
    Ri = formula(R,Vs),
    case {Li,Ri} of
	{X,X} -> 1;
	{0,1} -> 0;
	{1,0} -> 0;
	_ -> {'<->',Li,Ri}
    end.
%%
%% data looks like: {foo,1,2}, {foo,1,3}, {foo,1,1}, {foo,2,2}
%% predicates may look like:
%%  {foo,1,2}
%%  {foo,{all,"x"},{all,"x"}}
%%  {foo,1,{some,"Y"}}
%% 
pred(P,As,Vs) ->
    As1 = eval_args(As, Vs),
    {ok,Ts} = application:get_env(erlister, true),
    {ok,Fs} = application:get_env(erlister, false),
    Tab = ets:new(match_tab, []),
    _ = [ ets:insert(Tab, {Ti,true}) || Ti <- Ts],
    _ = [ ets:insert(Tab, {Fi,false}) || Fi <- Fs],
    pred_match_(Tab, list_to_atom(P), As1).

pred_match_(_T, _P, _As) ->
    %% FIXME
    0.

eval_args([{var,X}|As], Vs) ->
    case lists:keyfind(X, 2, Vs) of
	false ->
	    [{var,X}|eval_args(As,Vs)];
	{all,X} ->
	    [{all,X}|eval_args(As,Vs)];
	{some,X} ->
	    [{some,X}|eval_args(As,Vs)]
    end;
eval_args([{const,V}|As], Vs) ->
    [V|eval_args(As,Vs)];
eval_args([], _Vs) ->
    [].



	
	    
    
    
