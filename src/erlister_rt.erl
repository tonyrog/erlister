%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Erlister runtime support
%%% @end
%%% Created : 22 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister_rt).

-export([timer_start/2]).
-export([timer_stop/1]).
-export([timer_read/1]).
-export([output/2]).
-export([param_fetch/2, param_store/3]).
-export([uart_send/1, uart_recv/0, uart_avail/0]).
-export([gpio_input/1, gpio_output/1, gpio_set/1, gpio_clr/1, gpio_get/1]).
-export([analog_send/2, analog_recv/1]).
-export([can_send/3]).

timer_start(Name,Time) ->
    %% crash if already running? probably fixme!
    T = erlang:start_timer(Time, self(), Name),
    put(Name, T).

timer_stop(Name) ->
    case get(Name) of
	undefined -> false;
	T when is_reference(T) ->
	    erase(Name),
	    case erlang:cancel_timer(T) of
		Remain when is_integer(Remain) -> true;
		false ->
		    receive
			{timeout,T,Name} ->
			    true
		    after 0 ->
			    false
		    end
	    end
    end.

timer_read(Name) ->
    case get(Name) of
	undefined ->
	    %% before a timer is started the first time it is 
	    %% defined that it is in timeout.
	    timeout; 
	T when is_reference(T) ->
	    case erlang:read_timer(T) of
		false -> timeout;
		_ -> running
	    end
    end.

%% emit output ...
output(Old, New) ->
    io:format("transmit output ~w (old = ~w)\n", [New,Old]),
    ok.

param_fetch(Index, Subind) ->
    co_api:value(0, {Index, Subind}).

param_store(Index, Subind, Value) ->
    co_api:set_value(0, {Index, Subind}, Value).

uart_send(Char) ->
    io:put_chars([Char]).

uart_recv() ->
    %% fixme: now a newline is needed
    [C] = io:get_chars('',1),
    C.

uart_avail() ->
    1.

gpio_input(Pin) ->
    gpio:input(Pin).

gpio_output(Pin) ->
    gpio:output(Pin).

gpio_set(Pin) ->
    gpio:set(Pin).

gpio_clr(Pin) ->
    gpio:clr(Pin).

gpio_get(Pin) ->
    {ok,Value} = gpio:get(Pin),
    Value.

analog_send(Pin,Value) ->
    analog:send(Pin,Value).

analog_recv(Pin) ->
    analog:recv(Pin).

can_send(Index, SubInd, Value) ->
    co_api:notify(0,Index,SubInd,Value).
