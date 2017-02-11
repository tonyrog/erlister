%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Scan, Parse and translate state machines
%%% @end
%%% Created :  6 Jan 2017 by Tony Rogvall <tony@rogvall.se>

-module(erlister).

-compile(export_all).

-include("../include/erlister.hrl").

options() ->
    [ {lang,$l,"lang", {atom,c}, "output language" },
      {output,$o,"output-file", string, "output file name"},
      {version,$v,"version", undefined, "application version"},
      {help,$h,"help", undefined, "this help"}
    ].
    
start() ->
    start([]).

start(Args) ->
    application:load(erlister),
    case getopt:parse(options(), Args) of
	{ok,{Props,[]}} ->
	    case lists:member(help,Props) of
		true ->
		    getopt:usage(options(), "erlister"),
		    halt(0);
		false ->
		    io:format("~s: missing input file\n", ["erlister"]),
		    halt(1)
	    end;
	{ok,{Props,[File]}} ->
	    do_input(Props, File);
	{error,Error} ->
	    io:format("~s\n", 
		      [getopt:format_error(options(),Error)]),
	    getopt:usage(options(), "erlister"),
	    halt(1)
    end.

do_input(Props, File) ->
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
			    %% io:format("machine ~p\n", [Machine]),
			    case erlister_lint:machine(Machine) of
				{Machine1,[]} ->
				    do_emit(Props, Machine1);
				{_Machine1,ERR} ->
				    lists:foreach(
				      fun({Ln,Mod,Message}) ->
					      io:format("~s:~w: ~s\n", 
							[File,Ln,
							 apply(Mod,
							       format_error,
							       [Message])])
				      end, lists:keysort(1,ERR)),
				    %% io:format("machine' ~p\n", [Machine1]),
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

do_emit(Props, Machine) ->
    Machine1 = erlister_eval:machine(Machine),
    %% io:format("machine' ~p\n", [Machine1]),
    Code =
	case proplists:get_value(lang,Props,c) of
	    c -> erlister_c_code:code(Machine1);
	    erlang -> erlister_erl_code:code(Machine1);
	    seazone -> erlister_sz_code:code(Machine1)
	end,
    case proplists:get_value(output, Props) of
	undefined ->
	    io:put_chars([Code]),
	    halt(0);
	File ->
	    file:write_file(File, Code),
	    halt(0)
    end.
