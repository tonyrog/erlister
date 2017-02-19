%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Compile & Assemble szemu code
%%% @end
%%% Created : 25 Dec 2016 by Tony Rogvall <tony@rogvall.se>

-module(szcomp).

-compile(export_all).

-define( ZERO,        0).   %% ( -- 0 )
-define( ONE,         1).   %% ( -- 1 )
-define( ADD,         2).   %% ( a b -- (a+b) )
-define( SUB,         3).   %% ( a b -- (a-b) )
-define( MUL,         4).   %% ( a b -- (a*b) )
-define( DIV,         5).   %% ( a b -- (a/b) )
-define( MOD,         6).   %% ( a b -- (a%b) )
-define( AND,         7).   %% ( a b -- (a&b) )
-define( OR,          8).   %% ( a b -- (a|b) )
-define( XOR,         9).   %% ( a b -- (a^b) )
-define( NEG,         10).  %% ( a -- (-a) )
-define( INV,         11).  %% ( a -- (~a) )
-define( BSL,         12).  %% ( a n -- (a << n) )
-define( BSR,         13).  %% ( a n -- (a >> n) )
-define( ASR,         14).  %% ( a n -- (a >> n) )
-define( ZEQ,         16).  %% ( a -- (a==0) )
-define( ZLT,         17).  %% ( a -- (a<0) )
-define( INC,         18).  %% ( a -- (a+1) )
-define( DEC,         19).  %% ( a -- (a-1) )
-define( ABS,         20).  %% ( a -- |a| )
-define( MIN,         21).  %% ( a b -- min(a,b) )
-define( MAX,         22).  %% ( a b -- max(a,b) )
-define( ULT,         24).  %% ( a b -- (a < b) )
-define( LT,          25).  %% ( a b -- (a < b) )
-define( BRAN_B,      26).  %% ( -- )
-define( BRAN_W,      27).  %% ( -- )
-define( ZBRAN_B,     28).  %% ( cond -- )
-define( ZBRAN_W,     29).  %% ( cond -- )
-define( IBRAN_B,     30).  %% ( i -- )
-define( IBRAN_W,     31).  %% ( i -- )
-define( PUSH_B,      32).  %% ( -- b )
-define( PUSH_W,      33).  %% ( -- w )
-define( PUSH_L,      34).  %% ( -- l )
-define( DROP,        35).  %% ( a -- )
-define( STORE,       36).  %% ( a i -- )
-define( FETCH,       37).  %% ( i -- a )
-define( DUP,         38).  %% ( x -- x x )
-define( SWAP,        39).  %% ( x1 x2 -- x2 x1 )
-define( INPUT,       40).  %% ( i -- v )
-define( PARAM,       41).  %% ( i s -- v )
-define( START,       42).  %% ( t i -- )
-define( TIMEOUT,     43).  %% ( i -- bool )
-define( RUNNING,     44).  %% ( i -- bool )
-define( EQ,          45).  %% ( a b -- (a==b) )
-define( ULTE,        46).  %% ( a b -- (a <= b) )
-define( LTE,         47).  %% ( a b -- (a <= b) )
-define( NOT,         48).  %% ( a -- (not a) )
-define( CALL_B,      49).  %% ( -- ) R: ( -- addr )
-define( CALL_W,      50).  %% ( -- ) R: ( -- addr )
-define( RET,         51).  %% ( -- ) R: ( addr -- )
-define( ROT,         52).  %% ( x1 x2 x3 -- x2 x3 x1 )
-define( RROT,        53).  %% ( x1 x2 x3 -- x3 x2 x1 )
-define( OVER,        54).  %% ( x1 x2 -- x1 x2 x1 )
-define( NOP,        254).
-define( EXIT,       255).  %% ( -- )

opcodes() ->
    #{ 0 => ?ZERO,
       1 => ?ONE,
       '+' => ?ADD,
       '-' => ?SUB,
       '*' => ?MUL,
       '/' => ?DIV,
       'mod' => ?MOD,
       'and' => ?AND,
       'or' => ?OR,
       'xor' => ?XOR,
       'negate' => ?NEG,
       'invert' => ?INV,
       'lshift'  => ?BSL,
       'rshift'  => ?BSR,
       'arshift'  => ?ASR,
       '0='  => ?ZEQ,
       '0<'  => ?ZLT,
       '++'  => ?INC,
       '--'  => ?DEC,
       'abs' => ?ABS,
       'min' => ?MIN,
       'max' => ?MAX,
       'u<'  => ?ULT,
       '<'   => ?LT,
       'branch.b' => ?BRAN_B,
       'branch.w' => ?BRAN_W,
       'zbranch.b' => ?ZBRAN_B,
       'zbranch.w' => ?ZBRAN_W,
       'ibranch.b' => ?IBRAN_B,
       'ibranch.w' => ?IBRAN_W,
       'push.b' => ?PUSH_B,
       'push.w' => ?PUSH_W,
       'push.l' => ?PUSH_L,
       'drop' => ?DROP,
       'dup' => ?DUP,
       '!' => ?STORE,
       '@' => ?FETCH,
       'swap' => ?SWAP,
       'input' => ?INPUT,
       'param' => ?PARAM,
       'start' => ?START,
       'timeout' => ?TIMEOUT,
       'running' => ?RUNNING,
       '=' => ?EQ,
       'u<=' => ?ULTE,
       '<='  => ?LTE,
       'not' => ?NOT,
       'call.b' => ?CALL_B,
       'call.w' => ?CALL_W,
       ';' => ?RET,
       'rot' => ?ROT,
       '-rot' => ?RROT,
       'over' => ?OVER,
       'nop' => ?NOP,
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
    case resolve_near(Code1,AddrMap) of
	{true,Code2} ->
	    resolve_labels(Code2);
	{false,Code2} ->
	    Code3 = resolve_far(Code2),
	    io:format("RESOLVE FAR = ~w\n", [Code3]),
	    {AddrMap1,Code4} = map_labels(Code3),
	    resolve_labels_(Code4, [], AddrMap1, 0)
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
%% Locate guaranteed near jumps
%%
resolve_near(Code,Map) ->
    resolve_near_(Code, [], Map, [0], false).

resolve_near_([{'branch',L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr(1, Addr),
    Target = maps:get(L, Map),
    Offset = sub_addr(Target, Addr1),
    %% if all offsets are short then we select short
    case is_short_addr(Offset) of
	true ->
	    resolve_near_(Code, [{'branch.b',L}|Acc],Map,
			  add_addr(2,Addr),true);
	false ->
	    resolve_near_(Code, [{'branch',L}|Acc],Map,
			  add_addr([2,3],Addr),Res)
    end;
resolve_near_([{'call',L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr(1, Addr),
    Target = maps:get(L, Map),
    Offset = sub_addr(Target, Addr1),
    %% if all offsets are short then we select short
    case is_short_addr(Offset) of
	true ->
	    resolve_near_(Code, [{'call.b',L}|Acc],Map,
			  add_addr(2,Addr),true);
	false ->
	    resolve_near_(Code, [{'call',L}|Acc],Map,
			  add_addr([2,3],Addr),Res)
    end;
resolve_near_([{'zbranch',L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([2,3], Addr),
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
			  add_addr(1+1+N,Addr),true);
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
resolve_near_([Op={'branch.b',_L}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_near_(Code, [Op|Acc], LabelMap, add_addr(2, Addr),Res);
resolve_near_([Op={'branch.w',_L}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_near_(Code, [Op|Acc], LabelMap, add_addr(3, Addr),Res);
resolve_near_([Op={'call.b',_L}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_near_(Code, [Op|Acc], LabelMap, add_addr(2, Addr),Res);
resolve_near_([Op={'call.w',_L}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_near_(Code, [Op|Acc], LabelMap, add_addr(3, Addr),Res);
resolve_near_([Op={'zbranch.b',_L}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_near_(Code, [Op|Acc], LabelMap, add_addr(2, Addr),Res);
resolve_near_([Op={'zbranch.w',_L}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_near_(Code, [Op|Acc], LabelMap, add_addr(3, Addr),Res);
resolve_near_([Op={'ibranch.b',Ls}|Code], Acc, LabelMap, Addr,Res) ->
    N = length(Ls),
    resolve_near_(Code, [Op|Acc], LabelMap, add_addr(1+1+N, Addr),Res);
resolve_near_([Op={'ibranch.w',Ls}|Code], Acc, LabelMap, Addr,Res) ->
    N = length(Ls),
    resolve_near_(Code, [Op|Acc], LabelMap, add_addr(1+2+2*N, Addr),Res);
resolve_near_([Op={block,Block}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_near_(Code, [Op|Acc], LabelMap, add_addr(length(Block),Addr),Res);
resolve_near_([Op={label,_}|Code], Acc, LabelMap, Addr, Res) ->
    resolve_near_(Code, [Op|Acc], LabelMap, Addr, Res);
resolve_near_([], Acc, _LabelMap, _Addr,Res) ->
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
    if I =:= 0 ->
	    encode_const_(Code, [0|Acc]);
       I =:= 1 ->
	    encode_const_(Code, [1|Acc]);
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
encode_const_([C|Code],Acc) ->
    encode_const_(Code, [C|Acc]);
encode_const_([],Acc) ->
    lists:reverse(Acc).

%%
%% Encode all opcodes into bytes
%%
encode_opcodes(Code) ->
    encode_opcodes_(Code, [], opcodes()).

encode_opcodes_([Opcode|Code], Acc, Map) ->
    case maps:find(Opcode,Map) of
	{ok,C} ->
	    encode_opcodes_(Code,[C|Acc],Map);
	error when is_integer(Opcode), Opcode >= 0, Opcode =< 255 ->
	    encode_opcodes_(Code,[Opcode|Acc],Map)
    end;
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
