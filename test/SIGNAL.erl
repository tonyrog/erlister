-module('SIGNAL').
-export([main/0,wait/3,loop/3,final/0]).
-define(BOOL(X), if (X) -> 1; true -> 0 end).


main() -> 
    wait(
    #{
        'GREEN_state' => 'off',
        'RED_state' => 'on'    },
    #{
        'SIGNAL_goahead' => 0,
        'SIGNAL_lg' => 0,
        'SIGNAL_lr' => 0
    },
    #{
        'SIGNAL_g' => 0,
        'SIGNAL_r' => 0,
        'GREEN_ok' => 0,
        'GREEN_error' => 0,
        'RED_error' => 0
    }).

final() ->
    erlister_rt:stop_timer(clk_GREEN_T1),
    erlister_rt:stop_timer(clk_RED_T2),
    ok.


wait(STATE,IN,OUT) ->
    receive
        {timeout,_Ref,_} -> loop(STATE,IN,OUT);
        INPUT -> loop(STATE,maps:merge(INPUT,IN),OUT)
    end.


loop(
    STATE = #{
                'GREEN_state' := ST_GREEN,
                'RED_state' := ST_RED
    },
    IN = #{
        'SIGNAL_goahead' := I_SIGNAL_goahead,
        'SIGNAL_lg' := I_SIGNAL_lg,
        'SIGNAL_lr' := I_SIGNAL_lr
    },
    OUT = #{
        'SIGNAL_g' := OUT_SIGNAL_g,
        'SIGNAL_r' := OUT_SIGNAL_r,
        'GREEN_ok' := OUT_GREEN_ok,
        'GREEN_error' := OUT_GREEN_error,
        'RED_error' := OUT_RED_error
    }) ->
    IN_SIGNAL_goahead = I_SIGNAL_goahead,
    IN_SIGNAL_lg = I_SIGNAL_lg,
    IN_SIGNAL_lr = I_SIGNAL_lr,
    IN_GREEN_request_on = I_SIGNAL_goahead,
    IN_GREEN_light_control = I_SIGNAL_lg,
    IN_GREEN_error_in = OUT_RED_error,
    CLK_GREEN_T1 = erlister_rt:read_timer(clk_SIGNAL_T1),
    ST1_GREEN = 
        case (ST_GREEN) of
        'off' ->
            if
                (not (IN_GREEN_error_in)) andalso (IN_GREEN_request_on) ->
                    erlister_rt:timer_start(clk_GREEN_T1,2000),
                    'on';
                true -> ST_GREEN
            end;
        'on' ->
            if
                (IN_GREEN_error_in) orelse (not (IN_GREEN_request_on)) ->
                    'off';
                true -> ST_GREEN
            end;
        _ -> ST_GREEN
        end,
    OUT1_GREEN_ok = ?BOOL(not (IN_GREEN_light_control) orelse (IN_GREEN_request_on)),
    OUT1_GREEN_error = ?BOOL((not (IN_GREEN_error_in)) andalso (((CLK_GREEN_T1 =:= timeout)) andalso ((not (IN_GREEN_light_control)) andalso (IN_GREEN_request_on)))),
    IN_RED_request_on = not (I_SIGNAL_goahead),
    IN_RED_light_control = I_SIGNAL_lr,
    IN_RED_error_in = OUT1_GREEN_error,
    IN_RED_gok = OUT1_GREEN_ok,
    CLK_RED_T2 = erlister_rt:read_timer(clk_SIGNAL_T2),
    ST1_RED = 
        case (ST_RED) of
        'off' ->
            if
                (IN_RED_error_in) orelse (IN_RED_request_on) ->
                    'on';
                true -> ST_RED
            end;
        'on' ->
            if
                (IN_RED_gok) andalso (not (IN_RED_request_on)) ->
                    erlister_rt:timer_start(clk_RED_T2,2000),
                    'off';
                true -> ST_RED
            end;
        _ -> ST_RED
        end,
    OUT1_RED_error = ?BOOL((not (IN_RED_error_in)) andalso (((CLK_RED_T2 =:= timeout)) andalso ((IN_RED_light_control) andalso ((ST1_RED =:= 'off'))))),
    OUT1_SIGNAL_g = ?BOOL((ST1_GREEN =:= 'on')),
    OUT1_SIGNAL_r = ?BOOL((ST1_RED =:= 'on')),
    wait(STATE#{
                'GREEN_state' => ST1_GREEN,
                'RED_state' => ST1_RED
    },
    IN,
    OUT#{
        'SIGNAL_g' => OUT1_SIGNAL_g,
        'SIGNAL_r' => OUT1_SIGNAL_r,
        'GREEN_ok' => OUT1_GREEN_ok,
        'GREEN_error' => OUT1_GREEN_error,
        'RED_error' => OUT1_RED_error
    }).
