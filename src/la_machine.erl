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
    | {
        play,
        LastPlayTime :: non_neg_integer(),
        LastPlaySeq :: non_neg_integer() | undefined,
        PlayIndex :: non_neg_integer()
    }
    | {
        poke,
        PokeIndex :: non_neg_integer()
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

configure_button() ->
    ok = gpio:init(?BUTTON_GPIO),
    ok = gpio:set_pin_mode(?BUTTON_GPIO, input),
    ok = gpio:set_pin_pull(?BUTTON_GPIO, ?BUTTON_GPIO_PULL),
    ok = ?BUTTON_GPIO_HOLD(hold_dis),
    ok = esp:deep_sleep_enable_gpio_wakeup(1 bsl ?BUTTON_GPIO, ?BUTTON_GPIO_WAKEUP_LEVEL).

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

    % Configure button GPIO
    configure_button(),

    WakeupCause = esp:sleep_get_wakeup_cause(),
    ButtonState = read_button(),

    State0 = la_machine_state:load_state(),

    State1 =
        case action(WakeupCause, ButtonState, State0) of
            {play, SecondsElapsed, LastPlaySeq, PlayIndex} ->
                {PlayedSeq, NextPlayIndex} = play(SecondsElapsed, LastPlaySeq, PlayIndex),
                la_machine_state:append_play(PlayedSeq, NextPlayIndex, State0);
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

-spec action(esp:esp_wakeup_cause(), on | off, la_machine_state:state()) -> action().
action(_WakeupCause, on, State) ->
    LastPlayTime = la_machine_state:get_last_play_time(State),
    LastPlaySeq = la_machine_state:get_last_play_seq(State),
    PlayIndex = la_machine_state:get_play_index(State),
    SecondsElapsed = erlang:system_time(second) - LastPlayTime,
    {play, SecondsElapsed, LastPlaySeq, PlayIndex};
action(sleep_wakeup_gpio, off, _State) ->
    reset;
action(sleep_wakeup_timer, _ButtonState, State) ->
    {poke, la_machine_state:get_poke_index(State)};
action(undefined, _ButtonState, _State) ->
    reset.

poke(_PokeIndex) ->
    PokeScenario = la_machine_scenarios:get(poke, 1),
    PokeScenarioPart = hd(PokeScenario),
    {ok, Pid} = la_machine_player:start_link(),
    ok = la_machine_player:play(Pid, PokeScenarioPart),
    ok = la_machine_player:stop(Pid),
    ok.

-ifdef(DEMO_SEQ_SCENARIO).
play(ElapsedSeconds, LastPlaySeq, PlayIndex) when
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
play(_ElapsedSeconds, LastPlaySeq, _PlayIndex) ->
    % play next scenario
    ScenarioIx =
        case LastPlaySeq of
            undefined ->
                1;
            _ ->
                ScenarioCount = la_machine_scenarios:count(play),
                1 + (LastPlaySeq rem ScenarioCount)
        end,
    ScenarioParts = la_machine_scenarios:get(play, ScenarioIx),
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
-else.
play(ElapsedSeconds, LastPlaySeq, PlayIndex) when
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
play(_ElapsedSeconds, LastPlaySeq, _PlayIndex) ->
    % play random scenario, but different from LastPlaySeq
    <<RandScenario:56>> = crypto:strong_rand_bytes(7),
    ScenarioCount = la_machine_scenarios:count(play),
    ScenarioChoiceCount =
        case LastPlaySeq of
            undefined -> ScenarioCount;
            _ -> ScenarioCount - 1
        end,
    ScenarioIx0 = 1 + (RandScenario rem ScenarioChoiceCount),
    ScenarioIx =
        if
            LastPlaySeq =:= undefined ->
                ScenarioIx0;
            ScenarioIx0 >= LastPlaySeq ->
                ScenarioIx0 + 1;
            true ->
                ScenarioIx0
        end,
    ScenarioParts = la_machine_scenarios:get(play, ScenarioIx),
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
-endif.

setup_sleep(SleepSecs) ->
    gpio:deep_sleep_hold_en(),
    button_sleep(),
    SleepMs = SleepSecs * 1000,
    SleepMs.

compute_sleep_timer(State) ->
    % AtomVM integers are up to signed 64 bits
    <<RandHour:56>> = crypto:strong_rand_bytes(7),
    <<RandSec:56>> = crypto:strong_rand_bytes(7),
    compute_sleep_timer(RandHour, RandSec, State).

compute_sleep_timer(RandHour, RandSec, State) ->
    NextPokeIndex = la_machine_state:get_poke_index(State),
    PlayHoursCount = la_machine_state:get_play_hours_count(State),
    PokeHour =
        case PlayHoursCount of
            0 ->
                0;
            N ->
                la_machine_state:get_play_hour(RandHour rem N, State)
        end,
    % starts with 24 hours followed by 48 hours
    PokeBase = fib(2 + NextPokeIndex) * 24,
    PokeTime = PokeHour + PokeBase,
    PokeTime * 3600 + (RandSec rem 3600) - 1800.

fib(X) -> fib(X, 0, 1).

fib(1, _A, B) -> B;
fib(I, A, B) -> fib(I - 1, B, A + B).

-ifdef(TEST).
action_test_() ->
    [
        ?_assertEqual({poke, 0}, action(sleep_wakeup_timer, off, la_machine_state:new())),
        ?_assertEqual(reset, action(undefined, off, la_machine_state:new())),
        ?_assertMatch(
            {play, _ElapsedSecs, undefined, 0},
            action(sleep_wakeup_timer, on, la_machine_state:new())
        ),
        ?_assertMatch(
            {play, _ElapsedSecs, undefined, 0},
            action(sleep_wakeup_gpio, on, la_machine_state:new())
        ),
        ?_assertMatch(
            {play, _ElapsedSecs, undefined, 0}, action(undefined, on, la_machine_state:new())
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
