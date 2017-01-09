-module(example).

%% code generation example for example.em
-record(in, { x1=false, x2=false, x3=false }).
-record(out, { z1 }).

-export([start/0]).

start() ->
    main(state1,#in{}, #out{}).

main(State0,In0,Out0) ->
    receive
	Input ->
	    case update_input(Input,In0) of
		In1=#in { x1=X1, x2=X2, x3=X3} ->
		    Y1 = X1 andalso X2,
		    T = read_timer('T'),
		    State1 = case State0 of
				 state2 when Y1 -> state1;
				 state1 when not Y1 and not X3 -> state2;
				 state3 when X3 -> start_timer('T'), state3;
				 state1 when not X3 andalso T =:= timeout ->
				     state1
			     end,
		    Z1 = Y1 andalso not (State1 =:= state1),
		    io:format("z1 = ~p\n", [Z1]),
		    main(State1, In1, Out0#out { z1 = Z1})
	    end
    end.

update_input([{Key,Value}|Is], In) ->
    case Key of
	x1 -> update_input(Is, In#in { x1=Value});
	x2 -> update_input(Is, In#in { x2=Value});
	x3 -> update_input(Is, In#in { x3=Value});
	_ -> update_input(Is, In)
    end;
update_input([], In) ->
    In.

start_timer(Name) ->
    T = erlang:start_timer(2000, undefined, undefined),
    put(Name, T).

read_timer(Name) ->
    case get(Name) of
	undefined -> undefined;
	T when is_reference(T) ->
	    case erlang:read_timer(T) of
		false -> timeout;
		_ -> running
	    end
    end.
