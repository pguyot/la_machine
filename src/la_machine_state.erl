%
% This file is part of La Machine
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0
%

%%-----------------------------------------------------------------------------
%% @doc State of La Machine as stored in RTC slow Memory
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine_state).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("la_machine_definitions.hrl").

% State persistence
-export([
    load_state/0,
    save_state/1
]).

% Accessor to state
-export([
    get_wakeup_state/1,
    get_poke_index/1,
    get_last_play_time/1,
    get_last_play_seq/1,
    get_play_hours_count/1,
    get_play_hour/2,
    get_mood/1,
    get_gestures_count/1,
    get_total_gestures_count/1,
    get_battery_low/1,
    get_ms_since_last_on/1,
    get_click_count/1,
    get_play_info/1,
    get_seconds_since_boot/1
]).

% Modifiers
-export([
    set_wakeup_state/2,
    set_poke_index/2,
    append_play/4,
    set_mood_waiting/1,
    set_battery_low/2,
    set_last_on_now/1,
    set_click_count/2
]).

-export_type([
    state/0,
    wakeup_state/0,
    mood/0
]).

% Constructor for tests
-ifdef(TEST).
-export([
    new/0
]).
-endif.

-type wakeup_state() ::
    normal
    | provisioning
    | charging.

-type mood() ::
    waiting
    | imitation
    | dialectic
    | upset
    | calling
    | joy
    | tired
    | excited
    | calm.

-record(state, {
    % 64 bit erlang:system_time(second)
    boot_time = 0 :: non_neg_integer(),
    wakeup_state = normal :: wakeup_state(),
    % idem, used for sequence plays
    last_play_time = 0 :: non_neg_integer(),
    % index of last played scenario (32 bits), used to:
    % - play next part if we have several parts
    % - avoid playing one twice in a row
    % - or play scenarios sequentially (demo mode)
    last_play_seq = 0 :: non_neg_integer(),
    mood = waiting :: mood(),
    gesture_count = 0 :: non_neg_integer(),
    % number of gestures since last waiting
    total_gesture_count = 0 :: non_neg_integer(),
    battery_low = 0 :: non_neg_integer(),
    last_on = 0 :: non_neg_integer(),
    click_count = 0 :: non_neg_integer(),
    play_poke_index = 0 :: non_neg_integer(),
    % buffer with hours, appended to, and then rotated

    % the rest of the memory
    play_hours = <<>> :: binary()
}).

-type state() :: #state{}.

-define(MAX_PLAY_HOURS, 1000).

load_state() ->
    Now = erlang:system_time(second),
    deserialize_state(Now, get_state()).

save_state(State) ->
    Bin = serialize_state(State),
    ok = esp:rtc_slow_set_binary(Bin).

-spec get_state() -> binary() | undefined.
get_state() ->
    try
        case esp:reset_reason() of
            esp_rst_deepsleep -> esp:rtc_slow_get_binary();
            _ -> undefined
        end
    catch
        error:badarg ->
            undefined
    end.

-spec deserialize_state(non_neg_integer(), binary() | undefined) -> state().
deserialize_state(
    Now,
    <<WakeupStateInt:2, MoodInt:4, 0:2, BootTime:64, LastPlayTime:64, LastPlaySeq:32,
        GestureCount:8, TotalGestureCount:16, BatteryLow:8, LastOn:64, ClickCount:8,
        PlayPokeIndex:8, PlayHours/binary>>
) when
    WakeupStateInt =< 2 andalso MoodInt =< 8 andalso
        BootTime =< Now andalso byte_size(PlayHours) =< ?MAX_PLAY_HOURS
->
    %io:format("deserialize_state  : BootTime=~p Now=~p playhours_size=~p => Retrieving State\n", [BootTime, Now, byte_size(PlayHours)]),
    WakeupState = deserialize_wakeup_state(WakeupStateInt),
    Mood = deserialize_mood(MoodInt),
    #state{
        wakeup_state = WakeupState,
        boot_time = BootTime,
        last_play_time = LastPlayTime,
        last_play_seq = LastPlaySeq,
        mood = Mood,
        gesture_count = GestureCount,
        total_gesture_count = TotalGestureCount,
        battery_low = BatteryLow,
        last_on = LastOn,
        click_count = ClickCount,
        play_poke_index = PlayPokeIndex,
        play_hours = PlayHours
    };
deserialize_state(Now, _) ->
    #state{
        wakeup_state = normal,
        boot_time = Now,
        last_play_time = 0,
        last_play_seq = 0,
        mood = waiting,
        gesture_count = 0,
        total_gesture_count = 0,
        battery_low = 0,
        last_on = 0,
        click_count = 0,
        play_poke_index = 0,
        play_hours = <<>>
    }.

-spec serialize_state(state()) -> binary().
serialize_state(#state{
    wakeup_state = WakeupState,
    boot_time = BootTime,
    last_play_time = LastPlayTime,
    last_play_seq = LastPlaySeq,
    mood = Mood,
    gesture_count = GestureCount,
    total_gesture_count = TotalGestureCount,
    battery_low = BatteryLow,
    last_on = LastOn,
    click_count = ClickCount,
    play_poke_index = PlayPokeIndex,
    play_hours = PlayHours
}) ->
    WakeupStateInt = serialize_wakeup_state(WakeupState),
    MoodInt = serialize_mood(Mood),
    <<WakeupStateInt:2, MoodInt:4, 0:2, BootTime:64, LastPlayTime:64, LastPlaySeq:32,
        GestureCount:8, TotalGestureCount:16, BatteryLow:8, LastOn:64, ClickCount:8,
        PlayPokeIndex:8, PlayHours/binary>>.

-spec serialize_wakeup_state(wakeup_state()) -> 0..2.
serialize_wakeup_state(normal) -> 0;
serialize_wakeup_state(provisioning) -> 1;
serialize_wakeup_state(charging) -> 2.

-spec deserialize_wakeup_state(0..2) -> wakeup_state().
deserialize_wakeup_state(0) -> normal;
deserialize_wakeup_state(1) -> provisioning;
deserialize_wakeup_state(2) -> charging.

-spec serialize_mood(mood()) -> 0..8.
serialize_mood(waiting) -> 0;
serialize_mood(imitation) -> 1;
serialize_mood(dialectic) -> 2;
serialize_mood(upset) -> 3;
serialize_mood(calling) -> 4;
serialize_mood(joy) -> 5;
serialize_mood(tired) -> 6;
serialize_mood(excited) -> 7;
serialize_mood(calm) -> 8.

-spec deserialize_mood(0..8) -> mood().
deserialize_mood(0) -> waiting;
deserialize_mood(1) -> imitation;
deserialize_mood(2) -> dialectic;
deserialize_mood(3) -> upset;
deserialize_mood(4) -> calling;
deserialize_mood(5) -> joy;
deserialize_mood(6) -> tired;
deserialize_mood(7) -> excited;
deserialize_mood(8) -> calm.

% we just played
append_play(
    Mood,
    GestureCount,
    PlaySeqIndex,
    #state{boot_time = BootTime, play_hours = PlayHours0, total_gesture_count = TotalGestureCount} =
        State
) ->
    % Add the play hour
    Now = erlang:system_time(second),
    NowHour = ((Now - BootTime) div 3600) rem 24,
    PlayHours1 = append_hour(NowHour, PlayHours0),
    State#state{
        last_play_time = Now,
        last_play_seq = PlaySeqIndex,
        play_hours = PlayHours1,
        gesture_count = GestureCount,
        total_gesture_count = TotalGestureCount + 1,
        mood = Mood
    }.

-spec get_mood(state()) -> mood().
get_mood(#state{mood = Mood}) -> Mood.

%% To set the mood to waiting. Reset total_gesture_count and play_poke_index
set_mood_waiting(State) ->
    State#state{
        mood = waiting,
        total_gesture_count = 0,
        play_poke_index = 0
    }.

append_hour(Hour, Buffer) when byte_size(Buffer) < ?MAX_PLAY_HOURS ->
    <<Hour, Buffer/binary>>;
append_hour(Hour, Buffer) ->
    SubBuffer = binary:part(Buffer, 0, ?MAX_PLAY_HOURS - 1),
    <<Hour, SubBuffer/binary>>.

-spec get_wakeup_state(state()) -> wakeup_state().
get_wakeup_state(#state{wakeup_state = WakeupState}) -> WakeupState.

-spec set_wakeup_state(wakeup_state(), state()) -> state().
set_wakeup_state(WakeupState, State) -> State#state{wakeup_state = WakeupState}.

-spec get_play_info(state()) ->
    {
        Mood :: mood(),
        LastPlaySeq :: non_neg_integer() | undefined,
        GestureCount :: non_neg_integer(),
        LastPlayTime :: non_neg_integer()
    }.
get_play_info(
    #state{
        mood = Mood,
        last_play_time = LastPlayTime,
        gesture_count = GestureCount,
        last_play_seq = LastPlaySeq
    }
) ->
    {Mood, LastPlaySeq, GestureCount, LastPlayTime}.

-spec get_last_play_time(state()) -> non_neg_integer().
get_last_play_time(#state{last_play_time = LastPlayTime}) -> LastPlayTime.

-spec get_last_play_seq(state()) -> undefined | non_neg_integer().
get_last_play_seq(#state{last_play_time = 0}) -> undefined;
get_last_play_seq(#state{last_play_seq = LastPlaySeq}) -> LastPlaySeq.

-spec get_poke_index(state()) -> non_neg_integer().
get_poke_index(#state{last_play_time = 0, play_poke_index = PlayPokeIndex}) -> PlayPokeIndex;
get_poke_index(#state{last_play_time = _}) -> 0.
set_poke_index(PokeIndex, State) ->
    State#state{play_poke_index = PokeIndex, last_play_time = 0}.

-spec get_gestures_count(state()) -> non_neg_integer().
get_gestures_count(#state{gesture_count = GestureCount}) -> GestureCount.

-spec get_total_gestures_count(state()) -> non_neg_integer().
get_total_gestures_count(#state{total_gesture_count = TotalGestureCount}) -> TotalGestureCount.

-spec get_battery_low(state()) -> non_neg_integer().
get_battery_low(#state{battery_low = BatteryLow}) -> BatteryLow.

set_battery_low(BatteryLow, State) -> State#state{battery_low = BatteryLow}.

get_seconds_since_boot(#state{boot_time = BootTime}) ->
    Now_s = erlang:system_time(second),
    (Now_s - BootTime).

-spec get_ms_since_last_on(state()) -> non_neg_integer().
get_ms_since_last_on(#state{last_on = LastOn}) ->
    NowMS = erlang:system_time(millisecond),
    NowMS - LastOn.

set_last_on_now(State) ->
    NowMS = erlang:system_time(millisecond),
    State#state{last_on = NowMS}.

-spec get_click_count(state()) -> non_neg_integer().
get_click_count(#state{click_count = ClickCount}) -> ClickCount.

set_click_count(ClickCount, State) -> State#state{click_count = ClickCount}.

-spec get_play_hours_count(state()) -> non_neg_integer().
get_play_hours_count(#state{play_hours = PlayHours}) -> byte_size(PlayHours).

-spec get_play_hour(non_neg_integer(), state()) -> non_neg_integer().
get_play_hour(Ix, #state{play_hours = PlayHours}) -> binary:at(PlayHours, Ix).

-ifdef(TEST).
-spec new() -> state().
new() -> #state{boot_time = erlang:system_time(second)}.

deserialize_state_test_() ->
    [
        ?_assertEqual(
            #state{
                wakeup_state = normal,
                boot_time = 42,
                last_play_time = 0,
                play_poke_index = 0,
                play_hours = <<>>
            },
            deserialize_state(42, undefined)
        ),
        ?_assertEqual(
            #state{
                wakeup_state = normal,
                boot_time = 42,
                last_play_time = 0,
                play_poke_index = 0,
                play_hours = <<>>
            },
            deserialize_state(42, <<>>)
        ),
        ?_assertEqual(
            #state{
                wakeup_state = normal,
                boot_time = 42,
                last_play_time = 0,
                play_poke_index = 0,
                play_hours = <<>>
            },
            deserialize_state(42, <<0:(2048 * 8)>>)
        ),
        ?_assertEqual(
            #state{
                wakeup_state = provisioning,
                boot_time = 1,
                last_play_time = 2,
                last_play_seq = 3,
                mood = calling,
                gesture_count = 5,
                total_gesture_count = 6,
                battery_low = 7,
                last_on = 8,
                click_count = 9,
                play_poke_index = 11,
                play_hours = <<>>
            },
            deserialize_state(
                42, <<1:2, 4:4, 0:2, 1:64, 2:64, 3:32, 5:8, 6:16, 7:8, 8:64, 9:8, 11:8>>
            )
        ),
        ?_assertEqual(
            #state{
                wakeup_state = provisioning,
                boot_time = 1,
                last_play_time = 2,
                last_play_seq = 3,
                mood = waiting,
                gesture_count = 0,
                total_gesture_count = 0,
                battery_low = 0,
                last_on = 0,
                click_count = 0,
                play_poke_index = 4,
                play_hours = <<5>>
            },
            deserialize_state(42, <<1:2, 0:6, 1:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8, 5>>)
        ),
        ?_assertEqual(
            #state{
                wakeup_state = provisioning,
                boot_time = 1,
                last_play_time = 2,
                last_play_seq = 3,
                mood = waiting,
                gesture_count = 0,
                total_gesture_count = 0,
                battery_low = 0,
                last_on = 0,
                click_count = 0,
                play_poke_index = 4,
                play_hours = <<5, 6>>
            },
            deserialize_state(
                42, <<1:2, 0:6, 1:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8, 5, 6>>
            )
        ),
        ?_assertEqual(
            #state{
                wakeup_state = provisioning,
                boot_time = 1,
                last_play_time = 2,
                last_play_seq = 3,
                mood = waiting,
                gesture_count = 0,
                total_gesture_count = 0,
                battery_low = 0,
                last_on = 0,
                click_count = 0,
                play_poke_index = 4,
                play_hours = <<5, 6>>
            },
            deserialize_state(
                42, <<1:2, 0:6, 1:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8, 5, 6>>
            )
        ),
        % BootTime > Now should fall back to default
        ?_assertEqual(
            #state{
                wakeup_state = normal,
                boot_time = 42,
                last_play_time = 0,
                play_poke_index = 0,
                play_hours = <<>>
            },
            deserialize_state(
                42, <<1:2, 0:6, 142:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8, 5>>
            )
        ),
        % Out-of-range MoodInt (9) should fall back to default
        ?_assertEqual(
            #state{
                wakeup_state = normal,
                boot_time = 42,
                last_play_time = 0,
                play_poke_index = 0,
                play_hours = <<>>
            },
            deserialize_state(
                42, <<1:2, 9:4, 0:2, 1:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8>>
            )
        ),
        % Out-of-range WakeupStateInt (3) should fall back to default
        ?_assertEqual(
            #state{
                wakeup_state = normal,
                boot_time = 42,
                last_play_time = 0,
                play_poke_index = 0,
                play_hours = <<>>
            },
            deserialize_state(
                42, <<3:2, 0:4, 0:2, 1:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8>>
            )
        )
    ].

serialize_state_test_() ->
    [
        ?_assertMatch(
            <<0, _:64, 0:64, 0:32, 0:8, 0:16, 0:8, 0:64, 0:8, 0:8>>,
            serialize_state(
                new()
            )
        ),
        ?_assertEqual(
            <<0, 1:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8>>,
            serialize_state(
                #state{
                    wakeup_state = normal,
                    mood = waiting,
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<>>
                }
            )
        ),
        ?_assertEqual(
            <<1:2, 4:4, 0:2, 1:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8>>,
            serialize_state(
                #state{
                    wakeup_state = provisioning,
                    mood = calling,
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<>>
                }
            )
        ),
        ?_assertEqual(
            <<1:2, 5:4, 0:2, 1:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8, 5>>,
            serialize_state(
                #state{
                    wakeup_state = provisioning,
                    mood = joy,
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<5>>
                }
            )
        ),
        ?_assertEqual(
            <<1:2, 8:4, 0:2, 1:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8, 5>>,
            serialize_state(
                #state{
                    wakeup_state = provisioning,
                    mood = calm,
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<5>>
                }
            )
        ),
        ?_assertEqual(
            <<1:2, 6:4, 0:2, 1:64, 2:64, 3:32, 0:8, 0:16, 0:8, 0:64, 0:8, 4:8, 5, 6>>,
            serialize_state(
                #state{
                    wakeup_state = provisioning,
                    mood = tired,
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<5, 6>>
                }
            )
        )
    ].

serialize_mood_test() ->
    lists:foreach(
        fun(N) ->
            Mood = deserialize_mood(N),
            true = is_atom(Mood),
            Serialized = serialize_mood(Mood),
            ?assertEqual(Serialized, N)
        end,
        lists:seq(0, 8)
    ).

serialize_wakeup_state_test() ->
    lists:foreach(
        fun(N) ->
            WakeupState = deserialize_wakeup_state(N),
            true = is_atom(WakeupState),
            Serialized = serialize_wakeup_state(WakeupState),
            ?assertEqual(Serialized, N)
        end,
        lists:seq(0, 2)
    ).

-endif.
