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
	undefined -> undefined;
	T when is_reference(T) ->
	    case erlang:read_timer(T) of
		false -> timeout;
		_ -> running
	    end
    end.
