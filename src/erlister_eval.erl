%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Erlister eval predicate
%%% @end
%%% Created : 18 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister_eval).

-export([machine/1]).
-export([load_predicates/0]).

-include("../include/erlister.hrl").

machine(M = #machine { in=IN }) ->
    IN1 = [var(V) || V <- IN],
    M#machine { in = IN1 }.

var(V=#var{expr=Expr}) -> 
    V#var { expr=expr(Expr) }.
    
     
expr(undefined) -> undefined;
expr(F) -> expr(F,[]).

expr(C={const,_C},_Vs) -> C;
expr(I={in,_ID, _Type},_Vs) -> I;
expr(V={var,_X},_Vs) -> V;
expr({pred,P,As},Vs) -> 
    {const, pred(list_to_atom(P),As,Vs)};
expr({'ALL',{var,X},F},Vs) ->
    expr(F, [{all,X}|Vs]);
expr({'SOME',{var,X},F},Vs) ->
    expr(F, [{some,X}|Vs]);
expr({'&&',L,R},Vs) ->
    case expr(L,Vs) of
	0 -> 0;
	1 -> expr(R,Vs);
	L1 ->
	    case expr(R,Vs) of
		0 -> 0;
		1 -> L1;
		R1 -> {'&&',L1,R1}
	    end
    end;
expr({'||',L,R},Vs) ->
    case expr(L,Vs) of
	1 -> 1;
	0 -> expr(R,Vs);
	L1 ->
	    case expr(R,Vs) of
		1 -> 1;
		0 -> L1;
		R1 -> {'||',L1,R1}
	    end
    end;
expr({'->',L,R},Vs) ->
    expr({'||',{'!',L},R}, Vs);
expr({'<->',L,R},Vs) ->
    Li = expr(L,Vs),
    Ri = expr(R,Vs),
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
load_predicates() ->
    Data =
	case application:get_env(erlister, true) of
	    {ok,Ts} -> 
		[{T,true} || T <- Ts];
	    undefined -> 
		[]
	end ++
	case application:get_env(erlister, false) of
	    {ok,Fs} ->
		[{T,false} || T <- Fs];
	    undefined -> 
		[]
	end,
    Tab = ets:new(match_tab, []),
    ets:insert(Tab, Data),
    Tab.
    
pred(P,As,Vs) ->
    As1 = eval_args(As, Vs),
    Tab = load_predicates(),
    pred_match_(Tab, P, As1).

%%
%%  SOME x P(x)
%%     M = [ {{{'P','$1'}, true}, [], [true]} ]
%%     case ets:select(T, Match, 1) of
%%       '$end_of_table' -> false;
%%       {[true], _Cont} -> true
%%     end
%%
%%  ALL x P(x)
%%     M = [ {{{'P','$1'}, false}, [], [true]} ]
%%     case ets:select(T, Match, 1) of
%%       '$end_of_table' -> true;
%%       {[true], _Cont} -> false
%%     end
%%
%%  ALL x SOME y P(x,y)
%%     P(1,2) P(2,2) P(3,3) P(4,2) ...
%%  SOME x ALL y !P(x,y)
%%

%%
%%

%%
%% ALL x ALL y P(x,y) ==
%%   M = [ {{{'P','$1','$2'},true}, [], [true]} ]
%%   case ets:select(T, Match, 1) of
%%     '$end_of_table' -> false;
%%     {[true], _Cont} -> true
%%   end
%%
%% ALL x ALL y P(x,y) ==
%%   M = [ {{{'P','$1','$2'},false}, [], [true]} ]
%%   case ets:select(T, Match, 1) of
%%     '$end_of_table' -> true;
%%     {[true], _Cont} -> false
%%   end
%%
pred_match_(_T, _P, _As) ->
    %% FIXME
    false.

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
