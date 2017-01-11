-module(example).
-export([main/0]).
-export([main/3]).

main() ->
  main(s_state1,#{ i_x1=>false,i_x2=>false,i_x3=>false},#{ o_z1=>false}).

main(S0,I0,O0) ->
    receive
	{timeout,_Ref,_} -> update(S0,I0,O0);
	I -> update(S0,maps:merge(I0,I),O0)
    end.
      
update(S0,I1 = #{ i_x1:=I_x1,i_x2:=I_x2,i_x3:=I_x3},O0) ->
    D_y1 = (I_x2) andalso (I_x1),
    T_T = read_timer('T'),
    S1 = case S0 of
	     s_state1 when ((T_T =:= timeout)) andalso (not (I_x3)) -> s_state3;
	     s_state3 when I_x3 -> start_timer('T'), s_state2;
	     s_state1 when (not (I_x3)) andalso (not (D_y1)) -> s_state2;
	     s_state2 when D_y1 -> s_state1;
	     _ -> S0
	 end,
    O_z1 = (not ((S1 =:=s_state1))) andalso (D_y1),
    main(S1, I1, O0#{ o_z1=>O_z1}).

start_timer(Name) ->
    T = erlang:start_timer(2000, self(), Name),
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
