-module('example').
-export([main/0,wait/3,loop/3,final/0]).
-define(BOOL2INT(X), if (X) -> 1; true -> 0 end).


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
        INPUT -> loop(STATE,maps:merge(IN,INPUT),OUT)
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
        'example_z1' := OUT_example_z1
    }) ->
    IN_example_x1 = I_example_x1,
    IN_example_x2 = I_example_x2,
    IN_example_x3 = I_example_x3,
    DEF_example_y1 = ?BOOL2INT(((IN_example_x2 =/= 0)) andalso ((IN_example_x1 =/= 0))),
    CLK_example_T = erlister_rt:timer_read(clk_example_T),
    ST1_example = 
        case (ST_example) of
        'state1' ->
            if
                (DEF_example_y1 =/= 0) ->
                    'state2';
                true -> ST_example
            end;
        'state2' ->
            if
                (IN_example_x3 =/= 0) ->
                    erlister_rt:timer_start(clk_example_T,2000),
                    'state3';
                (not ((IN_example_x3 =/= 0))) andalso (not ((DEF_example_y1 =/= 0))) ->
                    'state1';
                true -> ST_example
            end;
        'state3' ->
            if
                ((CLK_example_T =:= timeout)) andalso (not ((IN_example_x2 =/= 0))) ->
                    'state1';
                true -> ST_example
            end;
        _ -> ST_example
        end,
    OUT1_example_z1 = ?BOOL2INT((not ((ST1_example =:= 'state1'))) andalso ((DEF_example_y1 =/= 0))),
    wait(STATE#{
        'example_state' => ST1_example
    },
    IN,
    OUT#{
        'example_z1' => OUT1_example_z1
    }).
