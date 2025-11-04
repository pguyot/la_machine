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
%% @doc La Machine entry point
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine).

-include("la_machine_definitions.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% We use eunit:start/0 entry point for tests.
-ifndef(TEST).
-export([start/0]).
-endif.

-type action() ::
    reset
    | { % Mood, SecondsElapsed, LastPlaySeq, PlayIndex, GestureCount
        play,
        Mood :: atom(),
        Reason :: atom(),
        SecondsElapsed :: non_neg_integer(),
        LastPlaySeq :: non_neg_integer() | undefined,
        PlayIndex :: non_neg_integer(),
        GestureCount :: non_neg_integer()        
    }
    | {
        poke,
        PokeIndex :: non_neg_integer()
    }
    | {
        play,
        meuh
    }.

%% @doc Configure watchdog to panic after `WATCHDOG_TIMEOUT_MS' ms (1 minute).
%% La machine should be finished within 1 minute and unconfigure watchdog before
%% the timeout elapses, so this watchdog should never be triggered.
%% However, if Erlang code crashes and shutdown doesn't happen, the watchdog
%% will eventually be triggered.
%%
%% WatchdogUser is currrently not a resource, but the code is designed as if it
%% was a resource that would automatically delete itself when being garbage
%% collected: WatchdogUser handle is kept and used to unconfigure watchdog just
%% before sleeping.
%% @end
-spec configure_watchdog() -> esp:task_wdt_user_handle().
configure_watchdog() ->
    esp:task_wdt_reconfigure({?WATCHDOG_TIMEOUT_MS, 0, true}),
    {ok, WatchdogUser} = esp:task_wdt_add_user(<<"la_machine">>),
    WatchdogUser.

%% @doc Unconfigure watchdog.
%% This function is called just before calling `esp:deep_sleep/1'
%% @end
-spec unconfigure_watchdog(esp:task_wdt_user_handle()) -> ok.
unconfigure_watchdog(WatchdogUser) ->
    % Do not prevent deep sleep if ever esp:task_wdt_delete_user/1 returns an error.
    _ = esp:task_wdt_delete_user(WatchdogUser),
    ok.

-ifdef(ACC_IRQ_GPIO).

configure_gpios() ->
    ok = gpio:init(?BUTTON_GPIO),
    ok = gpio:set_pin_mode(?BUTTON_GPIO, input),
    ok = gpio:set_pin_pull(?BUTTON_GPIO, ?BUTTON_GPIO_PULL),
    ok = ?BUTTON_GPIO_HOLD(hold_dis),
    ok = gpio:init(?ACC_IRQ_GPIO),
    ok = gpio:set_pin_mode(?ACC_IRQ_GPIO, input),
    ok = gpio:set_pin_pull(?ACC_IRQ_GPIO, down),
    ok = esp:deep_sleep_enable_gpio_wakeup(
        (1 bsl ?BUTTON_GPIO) bor (1 bsl ?ACC_IRQ_GPIO), ?BUTTON_GPIO_WAKEUP_LEVEL
    ).

-else.

configure_gpios() ->
    ok = gpio:init(?BUTTON_GPIO),
    ok = gpio:set_pin_mode(?BUTTON_GPIO, input),
    ok = gpio:set_pin_pull(?BUTTON_GPIO, ?BUTTON_GPIO_PULL),
    ok = ?BUTTON_GPIO_HOLD(hold_dis),
    ok = esp:deep_sleep_enable_gpio_wakeup(1 bsl ?BUTTON_GPIO, ?BUTTON_GPIO_WAKEUP_LEVEL).

-endif.

read_button() ->
    case gpio:digital_read(?BUTTON_GPIO) of
        ?BUTTON_GPIO_OFF -> off;
        ?BUTTON_GPIO_ON -> on
    end.

button_sleep() ->
    ok = ?BUTTON_GPIO_HOLD(hold_en).

-ifdef(BATTERY_LEVEL_GPIO).
battery_report() ->
    la_machine_battery:init(),
    io:format("battery level = ~p\n", [la_machine_battery:get_level()]),
    io:format("battery is charging = ~p\n", [la_machine_battery:is_charging()]).
-else.
battery_report() ->
    ok.
-endif.

-spec run() -> no_return().
run() ->
    WatchdogUser = configure_watchdog(),

    battery_report(),

    AccelerometerState = la_machine_lis3dh:setup(),

    % Configure accelerometer and button GPIOs
    configure_gpios(),

    WakeupCause = esp:sleep_get_wakeup_cause(),
    ButtonState = read_button(),

    State0 = la_machine_state:load_state(),

    io:format("Waking up : WakeupCause=~s ButtonState=~s\n", [WakeupCause, ButtonState]),

    State1 =
        case action(WakeupCause, ButtonState, AccelerometerState, State0) of
            {play, meuh} ->
                play_meuh(),
                State0;

            {play, Reason, Mood, SecondsElapsed, LastPlaySeq, PlayIndex, GestureCount} ->
                io:format("Play Reason=~s Mood=~s GestureCount=~p LastPlaySeq=~p SecondsElapsed=~p\n", [Reason, Mood, GestureCount, LastPlaySeq, SecondsElapsed]),

                % change mood ?
                {Mood1, GestureCount1, LastPlaySeq1} = change_moodp(Mood, Reason, GestureCount, SecondsElapsed, LastPlaySeq),

                if
                    Mood1 == waiting ->
                        % waiting : don't play anything
                        % ?? Should Only change the mood
                        la_machine_state:set_mood(Mood1, State0);
                    true ->
                        % else : play and remember
                        {PlayedSeq, NextPlayIndex} = play(Mood1, SecondsElapsed, LastPlaySeq1, PlayIndex),
                        la_machine_state:append_play(Mood1, GestureCount1 + 1, PlayedSeq, NextPlayIndex, State0)
                end;

            {poke, PokeIndex} ->
                poke(PokeIndex),
                la_machine_state:set_poke_index(PokeIndex + 1, State0);

            reset ->
                la_machine_audio:reset(),
                la_machine_servo:reset(),
                State0
        end,
    SleepTimer = compute_sleep_timer(State1),
    la_machine_state:save_state(State1),
    SleepMS = setup_sleep(SleepTimer),
    unconfigure_watchdog(WatchdogUser),
    esp:deep_sleep(SleepMS).

-ifndef(TEST).
%% @doc Entry point
%% @end
-spec start() -> no_return().
start() ->
    run().
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec action(
    esp:esp_wakeup_cause(), on | off, ok | {play, meuh} | not_resting, la_machine_state:state()
) -> action().

action(_WakeupCause, _ButtonState, {play, meuh}, _State) ->
    {play, meuh};

action(_WakeupCause, _ButtonState, not_resting, _State) ->
    reset;

action(_WakeupCause, on, ok, State) ->
    LastPlaySeq = la_machine_state:get_last_play_seq(State),
    PlayIndex = la_machine_state:get_play_index(State),
    Mood = la_machine_state:get_mood(State),
    GestureCount = la_machine_state:get_gestures_count(State),
    LastPlayTime = la_machine_state:get_last_play_time(State),
    SecondsElapsed = erlang:system_time(second) - LastPlayTime,
    {play, player, Mood, SecondsElapsed, LastPlaySeq, PlayIndex, GestureCount};

action(sleep_wakeup_gpio, off, ok, _State) ->
    reset;

action(sleep_wakeup_timer, _ButtonState, ok, State) ->
    LastPlaySeq = la_machine_state:get_last_play_seq(State),
    PlayIndex = la_machine_state:get_play_index(State),
    Mood = la_machine_state:get_mood(State),
    GestureCount = la_machine_state:get_gestures_count(State),
    LastPlayTime = la_machine_state:get_last_play_time(State),
    SecondsElapsed = erlang:system_time(second) - LastPlayTime,
    {play, timer, Mood, SecondsElapsed, LastPlaySeq, PlayIndex, GestureCount};

% action(sleep_wakeup_timer, _ButtonState, ok, State) ->
%     {poke, la_machine_state:get_poke_index(State)};

action(undefined, _ButtonState, _AccelerometerState, _State) ->
    reset.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% poke (unused)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
poke(_PokeIndex) ->
    PokeScenario = la_machine_scenarios:get(poke, 1),
    PokeScenarioPart = hd(PokeScenario),
    {ok, Pid} = la_machine_player:start_link(),
    ok = la_machine_player:play(Pid, PokeScenarioPart),
    ok = la_machine_player:stop(Pid),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_meuh
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play_meuh() ->
    <<RandScenario>> = crypto:strong_rand_bytes(1),
    Count = la_machine_scenarios:count(meuh),
    ScenarioIx = 1 + (RandScenario rem Count),
    MeuhScenario = la_machine_scenarios:get(meuh, ScenarioIx),
    MeuhScenarioPart = hd(MeuhScenario),
    {ok, Pid} = la_machine_player:start_link(),
    ok = la_machine_player:play(Pid, MeuhScenarioPart),
    ok = la_machine_player:stop(Pid),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% check mood change
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% timer while waiting -> start missing
change_moodp(waiting, timer, _GestureCount, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Wakeup while waiting : missing\n"),
    {missing, 0, undefined};

% timer while missing -> continue missing
change_moodp(missing, timer, GestureCount, _SecondsElapsed, LastPlaySeq) ->
    io:format("Wakeup while missing\n"),
    if
        GestureCount >= ?MAX_MISSING_SOUNDS ->
            io:format("   max times => waiting\n"),
            {waiting, 0, undefined};
        true -> 
            io:format("   continue missing\n"),
            {missing, GestureCount, LastPlaySeq}
    end;

% timer while all other moods -> start missing
change_moodp(Mood, timer, _GestureCount, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Wakeup while in ~s => missing\n", [Mood]),
    {missing, 0, undefined};

% player plays while missing or waiting
change_moodp(missing, player, _GestureCount, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Long time no see : joy\n"),
    {joy, 0, undefined};

change_moodp(waiting, player, _GestureCount, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Long time no see : joy\n"),
    {joy, 0, undefined};

change_moodp(joy, player, GestureCount, _SecondsElapsed, LastPlaySeq) ->
    Mood = joy,
    % if more than JOY_MIN_GESTURES gestures, one chance out of 2 to go to imitation
    <<RandChange>> = crypto:strong_rand_bytes(1),
    if
        GestureCount > ?JOY_MIN_GESTURES ->
            if
                (RandChange rem ?JOY_IMIT_CHANCE) == 0 -> 
                    TmpMood = imitation,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                true -> {Mood, GestureCount, LastPlaySeq}
            end;
        true -> {Mood, GestureCount, LastPlaySeq}
    end;

change_moodp(imitation, player, GestureCount, _SecondsElapsed, LastPlaySeq) ->
    Mood = imitation,
    <<RandChange>> = crypto:strong_rand_bytes(1),
    <<RandChange2>> = crypto:strong_rand_bytes(1),
    if
        GestureCount > ?IMIT_MIN_GESTURES ->
            if
                (RandChange rem ?IMIT_DIAL_CHANCE) == 0 -> 
                    TmpMood = dialectics,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                (RandChange2 rem ?IMIT_UPSET_CHANCE) == 0 -> 
                    TmpMood = upset,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                true -> {Mood, GestureCount, LastPlaySeq}
            end;
        true -> {Mood, GestureCount, LastPlaySeq}
    end;

change_moodp(dialectics, player, GestureCount, _SecondsElapsed, LastPlaySeq) ->
    Mood = dialectics,
    <<RandChange>> = crypto:strong_rand_bytes(1),
    <<RandChange2>> = crypto:strong_rand_bytes(1),
    if
        GestureCount > ?DIAL_MIN_GESTURES ->
            if
                (RandChange rem ?DIAL_IMIT_CHANCE) == 0 -> 
                    TmpMood = imitation,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                (RandChange2 rem ?DIAL_UPSET_CHANCE) == 0 -> 
                    TmpMood = upset,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                true -> {Mood, GestureCount, LastPlaySeq}
            end;
        true -> {Mood, GestureCount, LastPlaySeq}
    end;

change_moodp(upset, player, GestureCount, _SecondsElapsed, LastPlaySeq) ->
    Mood = upset,
    <<RandChange>> = crypto:strong_rand_bytes(1),
    if
        GestureCount > ?DIAL_MIN_GESTURES ->
            if
                (RandChange rem ?UPSET_IMIT_CHANCE) == 0 -> 
                    TmpMood = imitation,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                true -> {Mood, GestureCount, LastPlaySeq}
            end;
        true -> {Mood, GestureCount, LastPlaySeq}
    end;

% catch all : no change
change_moodp(Mood, _Reason, GestureCount, _SecondsElapsed, LastPlaySeq) ->
    {Mood, GestureCount, LastPlaySeq}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% unused
play(_Mood, ElapsedSeconds, LastPlaySeq, PlayIndex) when
    PlayIndex > 0 andalso ElapsedSeconds < ?PARTS_MAX_ELAPSE_INTERVAL
->
    % play next part of scenario
    ScenarioParts = la_machine_scenarios:get(play, LastPlaySeq),
    ScenarioPart = lists:nth(PlayIndex, ScenarioParts),
    {ok, Pid} = la_machine_player:start_link(),
    ok = la_machine_player:play(Pid, ScenarioPart),
    ok = la_machine_player:stop(Pid),
    if
        length(ScenarioParts) > PlayIndex ->
            {LastPlaySeq, PlayIndex + 1};
        true ->
            {LastPlaySeq, 0}
    end;

play(imitation, ElapsedSeconds, LastPlaySeq, _PlayIndex) ->
    Mood = imitation,
    io:format("playing mood : ~s ElapsedSeconds=~p\n", [Mood, ElapsedSeconds]),

    MoodScenar =
        if
            ElapsedSeconds =< ?GAME_SHORT_DUR_S ->
                game_short;
            ElapsedSeconds =< ?GAME_MEDIUM_DUR_S ->
                game_medium;
            true -> game_long
        end,

    ScenarioCount = la_machine_scenarios:count(MoodScenar),
    io:format("MoodScenar=~p ScenarioCount=~p\n", [MoodScenar, ScenarioCount]),

    % play random scenario, but different from LastPlaySeq
    ScenarioIx = random_num_upto_butnot(ScenarioCount, LastPlaySeq),
    io:format("ScenarioIx=~p\n", [ScenarioIx]),
    play_scenario(MoodScenar, ScenarioIx);

play(dialectics, ElapsedSeconds, LastPlaySeq, _PlayIndex) ->
    Mood = dialectics,
    io:format("playing mood : ~s ElapsedSeconds=~p\n", [Mood, ElapsedSeconds]),
    % play inverse
    MoodScenar =
        if
            ElapsedSeconds =< ?GAME_MEDIUM_DUR_S ->
                game_long;
            true -> game_short
        end,

    ScenarioCount = la_machine_scenarios:count(MoodScenar),
    io:format("MoodScenar=~p ScenarioCount=~p\n", [MoodScenar, ScenarioCount]),

    % play random scenario, but different from LastPlaySeq
    ScenarioIx = random_num_upto_butnot(ScenarioCount, LastPlaySeq),
    io:format("ScenarioIx=~p\n", [ScenarioIx]),
    play_scenario(MoodScenar, ScenarioIx);

play(Mood, _ElapsedSeconds, LastPlaySeq, _PlayIndex) ->
    io:format("playing mood : ~s\n", [Mood]),

    MoodScenar = Mood,

    ScenarioCount = la_machine_scenarios:count(MoodScenar),
    io:format("MoodScenar=~p ScenarioCount=~p\n", [MoodScenar, ScenarioCount]),

    % play random scenario, but different from LastPlaySeq
    ScenarioIx = random_num_upto_butnot(ScenarioCount, LastPlaySeq),
    io:format("ScenarioIx=~p\n", [ScenarioIx]),
    play_scenario(MoodScenar, ScenarioIx).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_scenario : play scenario ScenarioIx of MoodScenar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play_scenario(MoodScenar, ScenarioIx) ->
    ScenarioParts = la_machine_scenarios:get(MoodScenar, ScenarioIx),
    ScenarioPart = hd(ScenarioParts),
    {ok, Pid} = la_machine_player:start_link(),
    ok = la_machine_player:play(Pid, ScenarioPart),
    ok = la_machine_player:stop(Pid),
    if
        length(ScenarioParts) > 1 ->
            {ScenarioIx, 2};
        true ->
            {ScenarioIx, 0}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% setup_sleep
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setup_sleep(SleepSecs) ->
    gpio:deep_sleep_hold_en(),
    button_sleep(),
    SleepMs = SleepSecs * 1000,
    SleepMs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compute_sleep_timer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_sleep_timer(State) ->
    Mood = la_machine_state:get_mood(State),
    case Mood of
        % if waiting => 24h (for now)
        waiting ->
            io:format("waiting : sleep 24h\n"),
            3600 * 24;

        % if missing => compute delay from GestureCount
        missing -> 
            GestureCount = la_machine_state:get_gestures_count(State),
            <<RandS:56>> = crypto:strong_rand_bytes(7),
            DelayRange = ?MISSING_MAX_DELAY_S - ?MISSING_MIN_DELAY_S,
            Delay = ?MISSING_MIN_DELAY_S + (RandS rem DelayRange),
            DelayFull = GestureCount * Delay,
            io:format("missing : sleep ~ps (GestureCount=~p)\n", [DelayFull, GestureCount]),
            DelayFull;

        % else : delay first missing
        _ ->
            Delay = ?MISSING_START_DELAY_S,
            io:format("in ~s : sleep default ~ps\n", [Mood, Delay]),
            Delay
    end.

% compute_sleep_timer(State) ->
%     % AtomVM integers are up to signed 64 bits
%     <<RandHour:56>> = crypto:strong_rand_bytes(7),
%     <<RandSec:56>> = crypto:strong_rand_bytes(7),
%     compute_sleep_timer(RandHour, RandSec, State).

% compute_sleep_timer(RandHour, RandSec, State) ->
%     NextPokeIndex = la_machine_state:get_poke_index(State),
%     PlayHoursCount = la_machine_state:get_play_hours_count(State),
%     PokeHour =
%         case PlayHoursCount of
%             0 ->
%                 0;
%             N ->
%                 la_machine_state:get_play_hour(RandHour rem N, State)
%         end,
%     % starts with 24 hours followed by 48 hours
%     PokeBase = fib(2 + NextPokeIndex) * 24,
%     PokeTime = PokeHour + PokeBase,
%     PokeTime * 3600 + (RandSec rem 3600) - 1800.

% recursive implementation of random number
% return a random number 1 <= N <= MaxNumber, but not equel to NotNumber (if defined)
random_num_upto_butnot(1, _NotNumber) -> 1;

random_num_upto_butnot(MaxNumber, NotNumber) when NotNumber =:= undefined
    ->
    <<RndNum:56>> = crypto:strong_rand_bytes(7),
    1 + (RndNum rem MaxNumber);

random_num_upto_butnot(MaxNumber, NotNumber) ->
    Trial = random_num_upto_butnot(MaxNumber, undefined),
    if
        NotNumber =:= undefined -> Trial;
        NotNumber == Trial -> random_num_upto_butnot(MaxNumber, NotNumber); % recurse, try again
        true -> Trial
    end.

% fib(X) -> fib(X, 0, 1).
% fib(1, _A, B) -> B;
% fib(I, A, B) -> fib(I - 1, B, A + B).

-ifdef(TEST).
action_test_() ->
    [
        ?_assertEqual({poke, 0}, action(sleep_wakeup_timer, off, ok, la_machine_state:new())),
        ?_assertEqual(reset, action(undefined, off, ok, la_machine_state:new())),
        ?_assertMatch(
            {play, _ElapsedSecs, undefined, 0},
            action(sleep_wakeup_timer, on, ok, la_machine_state:new())
        ),
        ?_assertMatch(
            {play, _ElapsedSecs, undefined, 0},
            action(sleep_wakeup_gpio, on, ok, la_machine_state:new())
        ),
        ?_assertMatch(
            {play, _ElapsedSecs, undefined, 0}, action(undefined, on, ok, la_machine_state:new())
        )
    ].

fib_test_() ->
    [
        ?_assertEqual(1, fib(1)),
        ?_assertEqual(1, fib(2)),
        % 2 days
        ?_assertEqual(2, fib(3)),
        % 3 days
        ?_assertEqual(3, fib(4)),
        % 5 days
        ?_assertEqual(5, fib(5)),
        % 8 days, etc.
        ?_assertEqual(8, fib(6))
    ].

compute_sleep_timer_test_() ->
    [
        {
            "La Machine pokes 24 hours after reboot",
            ?_assertEqual(24 * 3600, compute_sleep_timer(0, 1800, la_machine_state:new()))
        },
        {
            "La Machine pokes between 23.5 and 24.5 hours after reboot",
            [
                ?_assert(compute_sleep_timer(la_machine_state:new()) >= 23 * 3600 + 1800),
                ?_assert(compute_sleep_timer(la_machine_state:new()) =< 24 * 3600 + 1800)
            ]
        },
        {
            "La Machine pokes 48 hours after first poke",
            ?_assertEqual(
                48 * 3600,
                compute_sleep_timer(
                    0, 1800, la_machine_state:set_poke_index(1, la_machine_state:new())
                )
            )
        },
        {
            "La Machine pokes 24 hours after first play",
            ?_assertEqual(
                24 * 3600,
                compute_sleep_timer(
                    0, 1800, la_machine_state:append_play(1, 0, la_machine_state:new())
                )
            )
        }
    ].
-endif.
