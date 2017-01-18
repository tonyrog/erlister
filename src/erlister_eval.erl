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
    IN1 = [{I,Ln,pred(Pred)} || {I,Ln,Pred} <- IN],
    M#machine { in = IN1 }.

pred(undefined) -> undefined;
pred(F) -> pred(F,[]).

pred(0,_Vs) -> 0;
pred(1,_Vs) -> 1;
pred(I={in,_ID},_Vs) -> I;
pred(V={var,_X},_Vs) -> V;
pred({pred,_P,_As},_Vs) -> 0;  %% FIXME
pred({'ALL',{var,X},F},Vs) ->
    pred(F, [{all,X}|Vs]);
pred({'SOME',{var,X},F},Vs) ->
    pred(F, [{some,X}|Vs]);
pred({'&&',L,R},Vs) ->
    case pred(L,Vs) of
	0 -> 0;
	1 -> pred(R,Vs);
	L1 ->
	    case pred(R,Vs) of
		0 -> 0;
		1 -> L1;
		R1 -> {'&&',L1,R1}
	    end
    end;
pred({'||',L,R},Vs) ->
    case pred(L,Vs) of
	1 -> 1;
	0 -> pred(R,Vs);
	L1 ->
	    case pred(R,Vs) of
		1 -> 1;
		0 -> L1;
		R1 -> {'||',L1,R1}
	    end
    end;
pred({'->',L,R},Vs) ->
    pred({'||',{'!',L},R}, Vs);
pred({'<->',L,R},Vs) ->
    Li = pred(L,Vs),
    Ri = pred(R,Vs),
    case {Li,Ri} of
	{X,X} -> 1;
	{0,1} -> 0;
	{1,0} -> 0;
	_ -> {'<->',Li,Ri}
    end.
