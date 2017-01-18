%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Scan, Parse and translate state machines
%%% @end
%%% Created :  6 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister).

-compile(export_all).

-include("../include/erlister.hrl").

start([File]) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    case erlister_scan:string(binary_to_list(Bin)) of
		{ok,Ts,_EndLine} ->
		    %% io:format("Tokens = ~p\n", [Ts]),
		    case erlister_parser:parse(Ts) of
			{error,{Ln,Mod,Message}} ->
			    io:format("~s:~w: ~s\n", 
				      [File,Ln,
				       apply(Mod,format_error,[Message])]),
			    halt(1);
			{ok,Machine} ->
			    io:format("machine ~p\n", [Machine]),
			    case erlister_lint:machine(Machine) of
				{Machine1,[]} ->
				    Machine2 = erlister_eval:machine(Machine1),
				    io:format("machine' ~p\n", [Machine2]),
				    io:format("erl_code=\n~s\n", 
					      [erlister_c_code:code(Machine2)]),
				    halt(0);
				{Machine1,ERR} ->
				    lists:foreach(
				      fun({Ln,Mod,Message}) ->
					      io:format("~s:~w: ~s\n", 
							[File,Ln,
							 apply(Mod,
							       format_error,
							       [Message])])
				      end, lists:keysort(1,ERR)),
				    io:format("machine' ~p\n", [Machine1]),
				    halt(1)
			    end
		    end;
		Error ->
		    io:format("error: ~p\n", [Error]),
		    halt(1)
	    end;
	Error ->
	    io:format("error: ~p\n", [Error]),
	    halt(1)
    end.
