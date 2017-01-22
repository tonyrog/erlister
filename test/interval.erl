-module('interval').
-export([main/0,wait/3,loop/3,final/0]).
-define(BOOL(X), if (X) -> 1; true -> 0 end).


main() -> 
    wait(
    #{
        'springback_state' => 'off',
        'alternate_state' => 'low'    },
    #{
        'interval_button' => 0
    },
    #{
        'interval_value' => 0,
        'springback_value' => 0,
        'alternate_value' => 0
    }).

final() ->
    erlister_rt:stop_timer(clk_alternate_Th),
    erlister_rt:stop_timer(clk_alternate_Tl),
    ok.


wait(STATE,IN,OUT) ->
    receive
        {timeout,_Ref,_} -> loop(STATE,IN,OUT);
        INPUT -> loop(STATE,maps:merge(INPUT,IN),OUT)
    end.


loop(
    STATE = #{
                'springback_state' := ST_springback,
                'alternate_state' := ST_alternate    },
    IN = #{
        'interval_button' := I_interval_button
    },
    OUT = #{
        'interval_value' := OUT_interval_value,
        'springback_value' := OUT_springback_value,
        'alternate_value' := OUT_alternate_value
    }) ->
    IN_interval_button = I_interval_button,
    IN_springback_button = I_interval_button,
    ST1_springback = 
        case (ST_springback) of
        'off' ->
            if
                IN_springback_button ->
                    'on';
                true -> ST_springback
            end;
        'on' ->
            if
                IN_springback_button ->
                    'off';
                true -> ST_springback
            end;
        _ -> ST_springback
        end,
    OUT1_springback_value = ?BOOL((ST1_springback =:= 'on')),
    IN_alternate_enable = OUT_springback_value,
    CLK_alternate_Th = erlister_rt:read_timer(clk_interval_Th),
    CLK_alternate_Tl = erlister_rt:read_timer(clk_interval_Tl),
    ST1_alternate = 
        case (ST_alternate) of
        'high' ->
            if
                ((CLK_alternate_Tl =:= timeout)) andalso (IN_alternate_enable) ->
                    erlister_rt:timer_start(clk_alternate_Th,2000),
                    'low';
                true -> ST_alternate
            end;
        'low' ->
            if
                ((CLK_alternate_Th =:= timeout)) andalso (IN_alternate_enable) ->
                    erlister_rt:timer_start(clk_alternate_Tl,2000),
                    'high';
                true -> ST_alternate
            end;
        _ -> ST_alternate
        end,
    OUT1_alternate_value = ?BOOL((ST1_alternate =:= 'high')),
    OUT1_interval_value = ?BOOL((OUT_alternate_value) andalso (OUT_springback_value)),
    wait(STATE#{
                'springback_state' => ST1_springback,
                'alternate_state' => ST1_alternate    },
    IN,
    OUT#{
        'interval_value' => OUT1_interval_value,
        'springback_value' => OUT1_springback_value,
        'alternate_value' => OUT1_alternate_value
    }).
