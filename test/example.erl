-module('example').
-export([main/0,wait/3,loop/3,final/0]).
-define(BOOL(X), if (X) -> 1; true -> 0 end).


main() -> 
    wait(
    #{
        'example_state' => 'state1'
    },
    #{
        'example_x1' => 0,
        'example_x2' => 0,
        'example_x3' => 0
    },
    #{
        'example_z1' => 0
    }).

final() ->
    erlister_rt:stop_timer(clk_example_T),
    ok.


wait(STATE,IN,OUT) ->
    receive
        {timeout,_Ref,_} -> loop(STATE,IN,OUT);
        INPUT -> loop(STATE,maps:merge(INPUT,IN),OUT)
    end.


loop(
    STATE = #{
        'example_state' := ST_example
    },
    IN = #{
        'example_x1' := I_example_x1,
        'example_x2' := I_example_x2,
        'example_x3' := I_example_x3
    },
    OUT = #{
        'example_z1' := O_example_z1
    }) ->
    IN_example_x1 = I_example_x1,
    IN_example_x2 = I_example_x2,
    IN_example_x3 = I_example_x3,
    DEF_example_y1 = (IN_example_x2) andalso (IN_example_x1),
    CLK_example_T = erlister_rt:timer_read(clk_example_T),
    ST_example1 = 
        case (ST_example) of
        'state1' ->
            if
                DEF_example_y1 ->
                    'state2';
                true -> ST_example
            end;
        'state2' ->
            if
                (not (IN_example_x3)) andalso (not (DEF_example_y1)) ->
                    'state1';
                IN_example_x3 ->
                    erlister_rt:timer_start(clk_example_T,2000),
                    'state3';
                true -> ST_example
            end;
        'state3' ->
            if
                ((CLK_example_T =:= timeout)) andalso (not (IN_example_x2)) ->
                    'state1';
                true -> ST_example
            end;
        _ -> ST_example
        end,
    OUT_example_z1 = ?BOOL((not ((ST_example =:= 'state1'))) andalso (DEF_example_y1)),
    wait(STATE#{
        'example_state' => ST_example1
    },
    IN,
    OUT#{
        'example_z1' => OUT_example_z1
    }).
