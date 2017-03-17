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
      {debug,$d,"debug", undefined, "debug output"},
      {version,$v,"version", undefined, "application version"},
      {help,$h,"help", undefined, "this help"}
    ].
    
start() ->
    start([]).

start(Args) ->
    application:load(erlister),
    case getopt:parse(options(), Args) of
	{ok,{Opts,[]}} ->
	    case lists:member(help,Opts) of
		true ->
		    getopt:usage(options(), "erlister"),
		    halt(0);
		false ->
		    io:format("~s: missing input file\n", ["erlister"]),
		    halt(1)
	    end;
	{ok,{Opts,[File]}} ->
	    do_input(Opts, File);
	{error,Error} ->
	    io:format("~s\n", 
		      [getopt:format_error(options(),Error)]),
	    getopt:usage(options(), "erlister"),
	    halt(1)
    end.

do_input(Opts, File) ->
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
			{ok,M0} ->
			    debugf(Opts,"parse:machine ~p\n", [M0]),
			    case erlister_lint:machine(M0) of
				{M1,[]} ->
				    do_emit(Opts, M1);
				{M1,ERR} ->
				    debugf(Opts,"lint:machine: ~p\n",[M1]),
				    lists:foreach(
				      fun({Ln,Mod,Message}) ->
					      io:format("~s:~w: ~s\n", 
							[File,Ln,
							 apply(Mod,
							       format_error,
							       [Message])])
				      end, lists:keysort(1,ERR)),
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

do_emit(Opts, M) ->
    debugf(Opts,"lint:machine: ~p\n",[M]),
    M1 = erlister_eval:machine(M),
    %% debugf(Opts,"lint:eval: ~p\n",[M1]),
    Code =
	case proplists:get_value(lang,Opts,c) of
	    c -> erlister_c_code:code(M1);
	    erl -> erlister_erl_code:code(M);
	    erlang -> erlister_erl_code:code(M1);
	    chine -> erlister_chine_code:code(M1);
	    gv -> erlister_dot_code:code(M1);
	    dot -> erlister_dot_code:code(M1)
	end,
    case proplists:get_value(output,Opts) of
	undefined ->
	    io:put_chars([Code]),
	    halt(0);
	File ->
	    file:write_file(File, Code),
	    halt(0)
    end.

debugf(Opts,Fmt,As) ->
    case lists:member(debug, Opts) of
	true ->
	    io:format(Fmt, As);
	false ->
	    ok
    end.
