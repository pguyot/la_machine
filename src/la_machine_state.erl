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
    get_is_paused/1,
    get_play_info/1,
    get_seconds_since_boot/1
]).

% Modifiers
-export([
    set_poke_index/2,
    append_play/4,
    set_mood_waiting/1,
    set_battery_low/2,
    set_last_on_now/1,
    set_click_count/2,
    set_is_paused/2
]).

-export_type([
    state/0
]).

% Constructor for tests
-ifdef(TEST).
-export([
    new/0
]).
-endif.

-record(state, {
    % 64 bit erlang:system_time(second)
    boot_time = 0 :: non_neg_integer(),
    % idem, used for sequence plays
    last_play_time = 0 :: non_neg_integer(),
    % index of last played scenario (32 bits), used to:
    % - play next part if we have several parts
    % - avoid playing one twice in a row
    % - or play scenarios sequentially (demo mode)
    last_play_seq = 0 :: non_neg_integer(),
    mood = 0 :: non_neg_integer(),
    gesture_count = 0 :: non_neg_integer(),
    % number of gestures since last waiting
    total_gesture_count = 0 :: non_neg_integer(),
    battery_low = 0 :: non_neg_integer(),
    last_on = 0 :: non_neg_integer(),
    click_count = 0 :: non_neg_integer(),
    is_paused = 0 :: non_neg_integer(),
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
    <<BootTime:64, LastPlayTime:64, LastPlaySeq:32, Mood:8, GestureCount:8, TotalGestureCount:16,
        BatteryLow:8, LastOn:64, ClickCount:8, IsPaused:8, PlayPokeIndex:8, PlayHours/binary>>
) when
    BootTime =< Now andalso byte_size(PlayHours) =< ?MAX_PLAY_HOURS
->
    %io:format("deserialize_state  : BootTime=~p Now=~p playhours_size=~p => Retrieving State\n", [BootTime, Now, byte_size(PlayHours)]),
    #state{
        boot_time = BootTime,
        last_play_time = LastPlayTime,
        last_play_seq = LastPlaySeq,
        mood = Mood,
        gesture_count = GestureCount,
        total_gesture_count = TotalGestureCount,
        battery_low = BatteryLow,
        last_on = LastOn,
        click_count = ClickCount,
        is_paused = IsPaused,
        play_poke_index = PlayPokeIndex,
        play_hours = PlayHours
    };
deserialize_state(Now, _) ->
    %io:format("deserialize_state : Now=~p=> Reinit State\n", [Now]),
    WaitingMoodInt = moodint_for_mood(waiting),
    #state{
        boot_time = Now,
        last_play_time = 0,
        last_play_seq = 0,
        mood = WaitingMoodInt,
        gesture_count = 0,
        total_gesture_count = 0,
        battery_low = 0,
        last_on = 0,
        click_count = 0,
        is_paused = 0,
        play_poke_index = 0,
        play_hours = <<>>
    }.

-spec serialize_state(state()) -> binary().
serialize_state(#state{
    boot_time = BootTime,
    last_play_time = LastPlayTime,
    last_play_seq = LastPlaySeq,
    mood = Mood,
    gesture_count = GestureCount,
    total_gesture_count = TotalGestureCount,
    battery_low = BatteryLow,
    last_on = LastOn,
    click_count = ClickCount,
    is_paused = IsPaused,
    play_poke_index = PlayPokeIndex,
    play_hours = PlayHours
}) ->
    <<BootTime:64, LastPlayTime:64, LastPlaySeq:32, Mood:8, GestureCount:8, TotalGestureCount:16,
        BatteryLow:8, LastOn:64, ClickCount:8, IsPaused:8, PlayPokeIndex:8, PlayHours/binary>>.

%%%%%%%%%%%% moods

mood_for_moodint(MoodInt) ->
    case MoodInt of
        0 -> waiting;
        1 -> imitation;
        2 -> dialectic;
        3 -> upset;
        4 -> calling;
        5 -> joy;
        6 -> tired;
        7 -> excited;
        _ -> waiting
    end.

moodint_for_mood(Mood) ->
    case Mood of
        waiting -> 0;
        imitation -> 1;
        dialectic -> 2;
        upset -> 3;
        calling -> 4;
        joy -> 5;
        tired -> 6;
        excited -> 7;
        _ -> 0
    end.

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
    MoodInt = moodint_for_mood(Mood),
    State#state{
        last_play_time = Now,
        last_play_seq = PlaySeqIndex,
        play_hours = PlayHours1,
        gesture_count = GestureCount,
        total_gesture_count = TotalGestureCount + 1,
        mood = MoodInt
    }.

-spec get_mood(state()) -> atom().
get_mood(#state{mood = MoodInt}) ->
    mood_for_moodint(MoodInt).

%% To set the mood to waiting. Reset total_gesture_count and play_poke_index
set_mood_waiting(State) ->
    WaitingMoodInt = moodint_for_mood(waiting),
    State#state{
        mood = WaitingMoodInt,
        total_gesture_count = 0,
        play_poke_index = 0
    }.

append_hour(Hour, Buffer) when byte_size(Buffer) < ?MAX_PLAY_HOURS ->
    <<Hour, Buffer/binary>>;
append_hour(Hour, Buffer) ->
    SubBuffer = binary:part(Buffer, 0, ?MAX_PLAY_HOURS - 1),
    <<Hour, SubBuffer/binary>>.

-spec get_play_info(state()) ->
    {
        Mood :: atom(),
        LastPlaySeq :: non_neg_integer() | undefined,
        GestureCount :: non_neg_integer(),
        LastPlayTime :: non_neg_integer()
    }.
get_play_info(
    #state{last_play_time = LastPlayTime, gesture_count = GestureCount, last_play_seq = LastPlaySeq} =
        State
) ->
    Mood = get_mood(State),
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

-spec get_is_paused(state()) -> non_neg_integer().
-if(?TRAVELMODE == 1).
% mode where it is in TRAVELMODE => always paused
get_is_paused(_State) -> 1.
-else.
get_is_paused(#state{is_paused = IsPaused}) -> IsPaused.
-endif.

set_is_paused(IsPaused, State) -> State#state{is_paused = IsPaused}.

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
            #state{boot_time = 42, last_play_time = 0, play_poke_index = 0, play_hours = <<>>},
            deserialize_state(42, undefined)
        ),
        ?_assertEqual(
            #state{boot_time = 42, last_play_time = 0, play_poke_index = 0, play_hours = <<>>},
            deserialize_state(42, <<>>)
        ),
        ?_assertEqual(
            #state{boot_time = 42, last_play_time = 0, play_poke_index = 0, play_hours = <<>>},
            deserialize_state(42, <<0:(2048 * 8)>>)
        ),
        % New format: <<BootTime:64, LastPlayTime:64, LastPlaySeq:32, Mood:8, GestureCount:8,
        %              TotalGestureCount:16, BatteryLow:8, LastOn:64, ClickCount:8, IsPaused:8,
        %              PlayPokeIndex:8, PlayHours/binary>>
        ?_assertEqual(
            #state{
                boot_time = 1,
                last_play_time = 2,
                last_play_seq = 3,
                mood = 4,
                gesture_count = 5,
                total_gesture_count = 6,
                battery_low = 7,
                last_on = 8,
                click_count = 9,
                is_paused = 10,
                play_poke_index = 11,
                play_hours = <<>>
            },
            deserialize_state(42, <<1:64, 2:64, 3:32, 4:8, 5:8, 6:16, 7:8, 8:64, 9:8, 10:8, 11:8>>)
        ),
        ?_assertEqual(
            #state{
                boot_time = 1,
                last_play_time = 2,
                last_play_seq = 3,
                mood = 0,
                gesture_count = 0,
                total_gesture_count = 0,
                battery_low = 0,
                last_on = 0,
                click_count = 0,
                is_paused = 0,
                play_poke_index = 4,
                play_hours = <<5>>
            },
            deserialize_state(42, <<1:64, 2:64, 3:32, 0:8, 0:8, 0:16, 0:8, 0:64, 0:8, 0:8, 4:8, 5>>)
        ),
        ?_assertEqual(
            #state{
                boot_time = 1,
                last_play_time = 2,
                last_play_seq = 3,
                mood = 0,
                gesture_count = 0,
                total_gesture_count = 0,
                battery_low = 0,
                last_on = 0,
                click_count = 0,
                is_paused = 0,
                play_poke_index = 4,
                play_hours = <<5, 6>>
            },
            deserialize_state(
                42, <<1:64, 2:64, 3:32, 0:8, 0:8, 0:16, 0:8, 0:64, 0:8, 0:8, 4:8, 5, 6>>
            )
        ),
        % BootTime > Now should fall back to default
        ?_assertEqual(
            #state{boot_time = 42, last_play_time = 0, play_poke_index = 0, play_hours = <<>>},
            deserialize_state(
                42, <<142:64, 2:64, 3:32, 0:8, 0:8, 0:16, 0:8, 0:64, 0:8, 0:8, 4:8, 5>>
            )
        )
    ].

serialize_state_test_() ->
    [
        % New format: <<BootTime:64, LastPlayTime:64, LastPlaySeq:32, Mood:8, GestureCount:8,
        %              TotalGestureCount:16, BatteryLow:8, LastOn:64, ClickCount:8, IsPaused:8,
        %              PlayPokeIndex:8, PlayHours/binary>>
        ?_assertMatch(
            <<_:64, 0:64, 0:32, 0:8, 0:8, 0:16, 0:8, 0:64, 0:8, 0:8, 0:8>>,
            serialize_state(
                new()
            )
        ),
        ?_assertEqual(
            <<1:64, 2:64, 3:32, 0:8, 0:8, 0:16, 0:8, 0:64, 0:8, 0:8, 4:8>>,
            serialize_state(
                #state{
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<>>
                }
            )
        ),
        ?_assertEqual(
            <<1:64, 2:64, 3:32, 0:8, 0:8, 0:16, 0:8, 0:64, 0:8, 0:8, 4:8, 5>>,
            serialize_state(
                #state{
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<5>>
                }
            )
        ),
        ?_assertEqual(
            <<1:64, 2:64, 3:32, 0:8, 0:8, 0:16, 0:8, 0:64, 0:8, 0:8, 4:8, 5, 6>>,
            serialize_state(
                #state{
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<5, 6>>
                }
            )
        )
    ].
-endif.
