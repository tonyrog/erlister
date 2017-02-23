%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Compile & Assemble szemu code
%%% @end
%%% Created : 25 Dec 2016 by Tony Rogvall <tony@rogvall.se>

-module(szcomp).

-compile(export_all).

-include("szcomp.hrl").

opcodes() ->
    #{ 
       %% op:3
       'nop' => ?NOP,
       'push.h' => ?PUSH_H,
       'zbranch.h' => ?ZBRAN_H,
       'dup' => ?DUP,
       'rot' => ?ROT,
       'over' => ?OVER,
       'drop' => ?DROP,
       '-' => ?SUB,
       %% op:4
       '+' => ?ADD,
       '*' => ?MUL,
       '=' => ?EQ,
       'and' => ?AND,
       'or' => ?OR,
       '0='  => ?ZEQ,
       '0<'  => ?ZLT,
       'not' => ?NOT,
       %% op:7
       '/' => ?DIV,
       'mod' => ?MOD,
       'xor' => ?XOR,
       'negate' => ?NEG,
       'invert' => ?INV,
       '<<'  => ?BSL,
       '>>'  => ?BSR,
       '>>a'  => ?ASR,
       '1+'  => ?INC,
       '1-'  => ?DEC,
       'abs' => ?ABS,
       'min' => ?MIN,
       'max' => ?MAX,
       'u<'  => ?ULT,
       '<'   => ?LT,
       '!' => ?STORE,
       '@' => ?FETCH,
       'swap' => ?SWAP,
       '<='  => ?LTE,
       'u<=' => ?ULTE,
       ';' => ?RET,

       'push.b' => ?PUSH_B,
       'push.w' => ?PUSH_W,
       'push.l' => ?PUSH_L,

       'branch.b' => ?BRAN_B,
       'branch.w' => ?BRAN_W,

       'zbranch.b' => ?ZBRAN_B,
       'zbranch.w' => ?ZBRAN_W,

       'ibranch.b' => ?IBRAN_B,
       'ibranch.w' => ?IBRAN_W,

       'call.b' => ?CALL_B,
       'call.w' => ?CALL_W,
       
       'sys.b' => ?SYS_B,

       'exit' => ?EXIT
     }.

    
asm_file(File) ->
    {ok,Ls} = file:consult(File),
    [asm(L) || L <- Ls].

asm(Code) ->
    io:format("CODE = ~w\n", [Code]),
    Code1 = encode_const(Code),     %% generate byte code for constants
    io:format("CODE1 = ~w\n", [Code1]),
    Code2 = collect_blocks(Code1),  %% collect basic blocks
    io:format("CODE2 = ~w\n", [Code2]),
    Code3 = resolve_labels(Code2),
    io:format("CODE3 = ~w\n", [Code3]),
    Code4 = disperse_blocks(Code3),
    io:format("CODE5 = ~w\n", [Code4]),
    encode_opcodes(Code4).

%%
%% Replace branch labels with offsets
%% iterate until all labels are resolved
%%
resolve_labels(Code) ->
    io:format("RESOLVE = ~w\n", [Code]),
    {AddrMap,Code1} = map_labels(Code),
    case resolve_tiny(Code1,AddrMap) of
	{true,Code2} ->
	    resolve_labels(Code2);
	{false,Code2} ->
	    case resolve_near(Code2,AddrMap) of
		{true,Code3} ->
		    resolve_labels(Code3);
		{false,Code3} ->
		    Code4 = resolve_far(Code3),
		    io:format("RESOLVE FAR = ~w\n", [Code4]),
		    {AddrMap1,Code5} = map_labels(Code4),
		    resolve_labels_(Code5, [], AddrMap1, 0)
	    end
    end.
%%
%% Set the correct offsets when all jumps are determined
%%
resolve_labels_([{'branch.b',L}|Code], Acc, Map, Addr) ->
    [Target] = maps:get(L, Map),
    Offset = Target - (Addr + 2),
    resolve_labels_(Code, [{'branch.b',Offset}|Acc], Map, Addr+2);
resolve_labels_([{'branch.w',L}|Code], Acc, Map, Addr) ->
    [Target] = maps:get(L, Map),
    Offset = Target - (Addr + 3),
    resolve_labels_(Code, [{'branch.w',Offset}|Acc], Map, Addr+3);
resolve_labels_([{'call.b',L}|Code], Acc, Map, Addr) ->
    [Target] = maps:get(L, Map),
    Offset = Target - (Addr + 2),
    resolve_labels_(Code, [{'call.b',Offset}|Acc], Map, Addr+2);
resolve_labels_([{'call.w',L}|Code], Acc, Map, Addr) ->
    [Target] = maps:get(L, Map),
    Offset = Target - (Addr + 3),
    resolve_labels_(Code, [{'call.w',Offset}|Acc], Map, Addr+3);
resolve_labels_([{'zbranch.h',L}|Code], Acc, Map, Addr) ->
    [Target] = maps:get(L, Map),
    Offset = Target - (Addr + 1),
    resolve_labels_(Code, [{'zbranch.h',Offset}|Acc], Map, Addr+1);
resolve_labels_([{'zbranch.b',L}|Code], Acc, Map, Addr) ->
    [Target] = maps:get(L, Map),
    Offset = Target - (Addr + 2),
    resolve_labels_(Code, [{'zbranch.b',Offset}|Acc], Map, Addr+2);
resolve_labels_([{'zbranch.w',L}|Code], Acc, Map, Addr) ->
    [Target] = maps:get(L, Map),
    Offset = Target - (Addr + 3),
    resolve_labels_(Code, [{'zbranch.w',Offset}|Acc], Map, Addr+3);
resolve_labels_([{'ibranch.b',Ls}|Code], Acc, Map, Addr) ->
    N = length(Ls),
    Addr1 = Addr + (1+1+N),
    OffsetList = [ begin [Target] = maps:get(L,Map),
			 Target - Addr1
		   end || L <- Ls ],
    resolve_labels_(Code, [{'ibranch.b',OffsetList}|Acc], Map, Addr1);
resolve_labels_([{'ibranch.w',Ls}|Code], Acc, Map, Addr) ->
    N = length(Ls),
    Addr1 = Addr + (1+2+2*N),
    OffsetList = [ begin [Target] = maps:get(L,Map),
			 Target - Addr1
		   end || L <- Ls ],
    resolve_labels_(Code, [{'ibranch.w',OffsetList}|Acc], Map, Addr1);
resolve_labels_([{label,_L}|Code], Acc, Map, Addr) ->
    %% drop label, it is not used any more
    resolve_labels_(Code, Acc, Map, Addr);
resolve_labels_([Op={block,Block}|Code], Acc, Map, Addr) ->
    resolve_labels_(Code, [Op|Acc], Map, Addr+length(Block));
resolve_labels_([], Acc, _Map, _Addr) ->
    lists:reverse(Acc).

%%
%% Locate guaranteed tiny jumps
%%
resolve_tiny(Code,Map) ->
    resolve_tiny_(Code, [], Map, [0], false).

resolve_tiny_([{'zbranch',L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([1,2,3], Addr),
    Target = maps:get(L, Map),
    Offset = sub_addr(Target, Addr1),
    %% if all offsets are short then we select short
    case is_tiny_addr(Offset) of
	true ->
	    resolve_tiny_(Code, [{'zbranch.h',L}|Acc],Map,
			  add_addr(1,Addr),true);
	false ->
	    resolve_tiny_(Code, [{'zbranch',L}|Acc],Map,
			  Addr1,Res)
    end;
resolve_tiny_([{'branch',L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([2,3],Addr),
    resolve_tiny_(Code, [{'branch',L}|Acc],Map,Addr1,Res);
resolve_tiny_([{'call',L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([2,3],Addr),
    resolve_tiny_(Code, [{'call',L}|Acc],Map,Addr1,Res);
resolve_tiny_([{'ibranch',Ls}|Code],Acc,Map,Addr,Res) ->
    N = length(Ls),
    Addr1 = add_addr([1+1+N,1+2+2*N], Addr),
    resolve_tiny_(Code, [{'ibranch',Ls}|Acc],Map,Addr1,Res);
resolve_tiny_([Op={block,Block}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_tiny_(Code, [Op|Acc], LabelMap, add_addr(length(Block),Addr),Res);
resolve_tiny_([Op={label,_}|Code], Acc, Map, Addr, Res) ->
    resolve_tiny_(Code, [Op|Acc], Map, Addr, Res);
resolve_tiny_([Op|Code],Acc,Map,Addr,Res) ->
    Len = opcode_length(Op),
    resolve_tiny_(Code, [Op|Acc], Map, add_addr(Len, Addr),Res);
resolve_tiny_([], Acc, _LabelMap, _Addr,Res) ->
    {Res,lists:reverse(Acc)}.

%%
%% Locate guaranteed near jumps
%%
resolve_near(Code,Map) ->
    resolve_near_(Code, [], Map, [0], false).

resolve_near_([{'branch',L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([2,3], Addr),
    Target = maps:get(L, Map),
    Offset = sub_addr(Target, Addr1),
    case is_short_addr(Offset) of
	true ->
	    resolve_near_(Code, [{'branch.b',L}|Acc],Map,
			  add_addr(2,Addr),true);
	false ->
	    resolve_near_(Code, [{'branch',L}|Acc],Map,Addr1,Res)
    end;
resolve_near_([{'call',L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([2,3], Addr),
    Target = maps:get(L, Map),
    Offset = sub_addr(Target, Addr1),
    case is_short_addr(Offset) of
	true ->
	    resolve_near_(Code, [{'call.b',L}|Acc],Map,
			  add_addr(2,Addr),true);
	false ->
	    resolve_near_(Code, [{'call',L}|Acc],Map,
			  Addr1,Res)
    end;
resolve_near_([{'zbranch',L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([1,2,3], Addr),
    Target = maps:get(L, Map),
    Offset = sub_addr(Target, Addr1),
    %% if all offsets are short then we select short
    case is_short_addr(Offset) of
	true ->
	    resolve_near_(Code, [{'zbranch.b',L}|Acc],Map,
			  add_addr(2,Addr),true);
	false ->
	    resolve_near_(Code, [{'zbranch',L}|Acc],Map,
			  add_addr([2,3],Addr),Res)
    end;
resolve_near_([{'ibranch',Ls}|Code],Acc,Map,Addr,Res) ->
    N = length(Ls),
    if N > 255 ->
	    resolve_near_(Code, [{'ibranch.w',Ls}|Acc],Map,
			  add_addr(1+2+2*N,Addr),true);
       true ->
	    Addr1 = add_addr([1+1+N,1+2+2*N], Addr),
	    case lists:all(fun(X) -> X end,
			   [ begin
				 Target = maps:get(L, Map),
				 Offset = sub_addr(Target, Addr1),
				 is_short_addr(Offset)
			     end || L <- Ls ]) of
		true ->
		    resolve_near_(Code, [{'ibranch.b',Ls}|Acc],Map,
				  add_addr(1+1+N,Addr),true);
		false ->
		    resolve_near_(Code, [{'ibranch',Ls}|Acc],Map,
				  Addr1,Res)
	    end
    end;
resolve_near_([Op={block,Block}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_near_(Code, [Op|Acc], LabelMap, add_addr(length(Block),Addr),Res);
resolve_near_([Op={label,_}|Code], Acc, LabelMap, Addr, Res) ->
    resolve_near_(Code, [Op|Acc], LabelMap, Addr, Res);
resolve_near_([Op|Code],Acc,Map,Addr,Res) ->
    Len = opcode_length(Op),
    resolve_near_(Code, [Op|Acc], Map, add_addr(Len, Addr),Res);
resolve_near_([], Acc, _Map, _Addr,Res) ->
    {Res,lists:reverse(Acc)}.


%% replace all branch/zbranch/ibranch with far offset version
resolve_far([{'branch',L}|Code]) ->
    [{'branch.w',L}|resolve_far(Code)];
resolve_far([{'call',L}|Code]) ->
    [{'call.w',L}|resolve_far(Code)];
resolve_far([{'zbranch',L}|Code]) ->
    [{'zbranch.w',L}|resolve_far(Code)];
resolve_far([{'ibranch',Ls}|Code]) ->
    [{'ibranch.w',Ls}|resolve_far(Code)];
resolve_far([Op|Code]) ->
    [Op|resolve_far(Code)];
resolve_far([]) ->
    [].

%%
%% Calculate address table
%% map and remove labels
%%
map_labels(Code) ->
    map_labels_(Code, [], #{}, [0]).

map_labels_([Op={label,L}|Code], Acc, Map, Addr) ->
    case maps:find(L, Map) of
	error -> 
	    map_labels_(Code, [Op|Acc], Map#{ L => Addr }, Addr);
	{ok,_} ->
	    erlang:error({label_exist,L})
    end;
map_labels_([Op={block,Block}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr(length(Block),Addr));
map_labels_([Op={'branch.b',_}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr(2,Addr));
map_labels_([Op={'branch.w',_}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr(3,Addr));
map_labels_([Op={'call.b',_}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr(2,Addr));
map_labels_([Op={'call.w',_}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr(3,Addr));
map_labels_([Op={'zbranch.h',_}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr(1,Addr));
map_labels_([Op={'zbranch.b',_}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr(2,Addr));
map_labels_([Op={'zbranch.w',_}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr(3,Addr));
map_labels_([Op={'ibranch.b',Ls}|Code], Acc, Map, Addr) ->
    N = length(Ls),
    map_labels_(Code, [Op|Acc], Map, add_addr(1+1+N,Addr));
map_labels_([Op={'ibranch.w',Ls}|Code], Acc, Map, Addr) ->
    N = length(Ls),
    map_labels_(Code, [Op|Acc], Map, add_addr(1+2+2*N,Addr));
map_labels_([Op={branch,_L}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr([2,3],Addr));
map_labels_([Op={call,_L}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr([2,3],Addr));
map_labels_([Op={zbranch,_L}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr([2,3],Addr));
map_labels_([Op={ibranch,Ls}|Code], Acc, Map, Addr) ->
    N = length(Ls),
    map_labels_(Code, [Op|Acc], Map, add_addr([1+1+N,1+2+2*N],Addr));
map_labels_([], Acc, Map, _Addr) ->
    {Map, lists:reverse(Acc)}.

%% list of alternative addresses
add_addr(A, B) when is_list(A), is_list(B) ->
    lists:usort([ X + Y || X <- A, Y <- B ]);
add_addr(A, B) when is_list(A), is_integer(B) ->
    [ X + B || X <- A];
add_addr(A, B) when is_integer(A), is_list(B) ->
    [ A + Y || Y <- B];
add_addr(A, B) when is_integer(A), is_integer(B) ->
    [ A + B ].

sub_addr(A, B) when is_list(A), is_list(B) ->
    lists:usort([ X - Y || X <- A, Y <- B ]);
sub_addr(A, B) when is_list(A), is_integer(B) ->
    [ X - B || X <- A];
sub_addr(A, B) when is_integer(A), is_list(B) ->
    [ A - Y || Y <- B];
sub_addr(A, B) when is_integer(A), is_integer(B) ->
    [ A - B ].

is_tiny_addr([A|_]) when A < -8; A > 7 -> false;
is_tiny_addr([_|As]) -> is_tiny_addr(As);
is_tiny_addr([]) -> true.

is_short_addr([A|_]) when A < -128; A > 127 -> false;
is_short_addr([_|As]) -> is_short_addr(As);
is_short_addr([]) -> true.

%%
%% Collect code blocks
%%
collect_blocks(Code) ->
    collect_blocks_(Code, [], []).

collect_blocks_([Op={branch,_L}|Code], Block, Acc) ->
    collect_blocks_(Code, [], [Op | add_block(Block,Acc)]);
collect_blocks_([Op={call,_L}|Code], Block, Acc) ->
    collect_blocks_(Code, [], [Op | add_block(Block,Acc)]);
collect_blocks_([Op={zbranch,_Ls}|Code], Block, Acc) ->
    collect_blocks_(Code, [], [Op | add_block(Block,Acc)]);
collect_blocks_([Op={ibranch,_L}|Code], Block, Acc) ->
    collect_blocks_(Code, [], [Op | add_block(Block,Acc)]);
collect_blocks_([Op={label,_L}|Code], Block, Acc) ->
    collect_blocks_(Code, [], [Op | add_block(Block,Acc)]);
collect_blocks_([Op1|Code1=[Op2|Code2]], Block, Acc) 
  when is_atom(Op1),is_atom(Op2) ->
    Map = opcodes(),
    N3 = maps:get(Op1, Map),
    N4 = maps:get(Op2, Map),
    if N3 < 8, N4 < 16 ->
	    collect_blocks_(Code2, [{Op1,Op2}|Block], Acc);
       true ->
	    collect_blocks_(Code1, [Op1|Block], Acc)
    end;
collect_blocks_([Opcode|Code], Block, Acc) ->
    collect_blocks_(Code, [Opcode|Block], Acc);
collect_blocks_([], Block, Acc) ->
    lists:reverse(add_block(Block,Acc)).

add_block([],Code) -> Code;
add_block(Block,Code) -> [{block,lists:reverse(Block)}|Code].

%%
%% Encode blocks and branch instructions
%% L is in byte range then {branch,L} encodes into ['branch.b', L]
%%  0 {branch,L1} .N1. {branch,L2} .N2. {label,L2} ... {label,L1}
%%  {label,L1}  .N1. {branch,L2} ... {label,L2} ... {branch,L1}
%%  {label,L1}  ... {label,L2} .N1. {branch,L2} ... {branch,L1}
%%  {label,L1}  .N1. {branch,L2} .N2. {branch,L1} .N3. {label,L2}
%%
disperse_blocks(Code) ->
    disperse_blocks_(Code, []).

disperse_blocks_([{block,Block}|Code], Acc) ->
    disperse_blocks_(Code, lists:reverse(Block)++Acc);
disperse_blocks_([{'branch.b',L}|Code], Acc) ->
    <<B0>> = <<L:8>>,
    disperse_blocks_(Code, [B0,'branch.b'|Acc]);
disperse_blocks_([{'branch.w',L}|Code], Acc) ->
    <<B1,B0>> = <<L:16>>,
    disperse_blocks_(Code, [B0,B1,'branch.w'|Acc]);
disperse_blocks_([{'call.b',L}|Code], Acc) ->
    <<B0>> = <<L:8>>,
    disperse_blocks_(Code, [B0,'call.b'|Acc]);
disperse_blocks_([{'call.w',L}|Code], Acc) ->
    <<B1,B0>> = <<L:16>>,
    disperse_blocks_(Code, [B0,B1,'call.w'|Acc]);
disperse_blocks_([{'zbranch.h',L}|Code], Acc) ->
    <<B0:3>> = <<L:3>>,
    disperse_blocks_(Code, [{'zbranch.h',B0}|Acc]);
disperse_blocks_([{'zbranch.b',L}|Code], Acc) ->
    <<B0>> = <<L:8>>,
    disperse_blocks_(Code, [B0,'zbranch.b'|Acc]);
disperse_blocks_([{'zbranch.w',L}|Code], Acc) ->
    <<B1,B0>> = <<L:16>>,
    disperse_blocks_(Code, [B0,B1,'zbranch.w'|Acc]);
disperse_blocks_([{'ibranch.b',Ls}|Code], Acc) ->
    N = length(Ls),
    Bs = [ begin <<B0>> = <<L:8>>, B0 end || L <- Ls],
    disperse_blocks_(Code, lists:reverse(Bs)++[N,'ibranch.b'|Acc]);
disperse_blocks_([{'ibranch.w',Ls}|Code], Acc) ->
    N = length(Ls),
    <<N1,N0>> = <<N:16>>,
    Bs = lists:append([ begin <<B1,B0>> = <<L:16>>,[B1,B0] end || L <- Ls]),
    disperse_blocks_(Code, lists:reverse(Bs)++[N0,N1,'ibranch.w'|Acc]);
disperse_blocks_([], Acc) ->
    lists:reverse(Acc).

%%
%% Encode integer constants 0, 1 encode to instructions
%% while other constants encode into 2,3 or 5 byte instructions
%%    
encode_const(Code) ->
    encode_const_(Code,[]).

encode_const_([{const,I}|Code],Acc) when is_integer(I) ->
    if I >= -8, I =< 7 ->
	    <<H0:3>> = <<I:3>>,
	    encode_const_(Code, [{'push.h',H0}|Acc]);
       I >= -16#80, I =< 16#7f ->
	    <<B0>> = <<I:8>>,
	    encode_const_(Code, [B0,'push.b'|Acc]);
       I >= -16#8000, I =< 16#7fff ->
	    <<B1,B0>> = <<I:16>>,
	    encode_const_(Code, [B0,B1,'push.w'|Acc]);
       I >= -16#80000000, I =< 16#7fffffff ->
	    <<B3,B2,B1,B0>> = <<I:32>>,
	    encode_const_(Code, [B0,B1,B2,B3,'push.w'|Acc]);
       true ->
	    erlang:error(integer_to_big)
    end;
encode_const_([input_boolean|Code], Acc) ->
    encode_const_([{const,?INPUT_BOOLEAN}|Code],Acc);
encode_const_([input_analog|Code], Acc) ->
    encode_const_([{const,?INPUT_ANALOG}|Code],Acc);
encode_const_([input_encoder|Code], Acc) ->
    encode_const_([{const,?INPUT_ENCODER}|Code],Acc);
encode_const_([{sys, SysOp}|Code], Acc) ->
    Sys = case SysOp of
	      'param@' -> ?SYS_PARAM_FETCH;
	      'param!' -> ?SYS_PARAM_STORE;
	      timer_init -> ?SYS_TIMER_INIT;
	      timer_start -> ?SYS_TIMER_START;
	      timer_stop -> ?SYS_TIMER_STOP;
	      timer_timeout -> ?SYS_TIMER_TIMEOUT;
	      timer_running -> ?SYS_TIMER_RUNNING;
	      'input@' -> ?SYS_INPUT_FETCH;
	      select -> ?SYS_SELECT;
	      emit -> ?SYS_EMIT;
	      key -> ?SYS_KEY
	  end,
    encode_const_(Code, [Sys,?SYS_B|Acc]);
encode_const_([C|Code],Acc) ->
    encode_const_(Code, [C|Acc]);
encode_const_([],Acc) ->
    lists:reverse(Acc).

%%
%% Size of opcode
%%
opcode_length({'push.h',_}) -> 1;
opcode_length({'push.b',_}) -> 2;
opcode_length({'push.w',_}) -> 3;
opcode_length({'push.l',_}) -> 4;
opcode_length({'zbranch.h',_}) -> 1;
opcode_length({'zbranch.b',_}) -> 2;
opcode_length({'zbranch.w',_}) -> 3;
opcode_length({'branch.b',_})  -> 2;
opcode_length({'branch.w',_})  -> 3;
opcode_length({'ibranch.b',Ls}) -> 1+1+length(Ls);
opcode_length({'ibranch.w',Ls}) -> 1+2+2*length(Ls);
opcode_length({'sys.b',_}) -> 2;
opcode_length({Op3,Op4}) ->
    Map = opcodes(),
    N3 = maps:get(Op3, Map),
    N4 = maps:get(Op4, Map),
    if N3 < 8, N4 < 16 -> 1 end;
opcode_length(Op) ->
    Map = opcodes(),
    N7 = maps:get(Op, Map),    
    if N7 < 127-> 1 end.

%%
%% Encode all opcodes into bytes
%%
encode_opcodes(Code) ->
    encode_opcodes_(Code, [], opcodes()).

encode_opcodes_([Op|Code], Acc, Map) when 
      is_integer(Op), Op >= 0, Op =< 255 ->
    encode_opcodes_(Code,[Op|Acc],Map);
encode_opcodes_([{'push.h',I4}|Code], Acc, Map) ->
    encode_opcodes_(Code,[?OPCODE2(?PUSH_H,I4)|Acc],Map);
encode_opcodes_([{'zbranch.h',I4}|Code], Acc, Map) ->
    encode_opcodes_(Code,[?OPCODE2(?ZBRAN_H,I4)|Acc],Map);
encode_opcodes_([{Op3,Op4}|Code], Acc, Map) ->
    N3 = maps:get(Op3,Map),
    N4 = maps:get(Op4,Map),
    encode_opcodes_(Code,[?OPCODE2(N3,N4)|Acc],Map);
encode_opcodes_([Op|Code], Acc, Map) ->
    N7 = maps:get(Op,Map),
    encode_opcodes_(Code,[N7|Acc],Map);
encode_opcodes_([], Acc, _Map) ->
    list_to_binary(lists:reverse(Acc)).

new_label() ->
    case get(next_label) of
	undefined ->
	    put(next_label, 1),
	    0;
	I ->
	    put(next_label, I+1),
	    I
    end.
