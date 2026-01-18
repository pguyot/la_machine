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

-export([prune_workaround/0]).

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

%% @doc Configure GPIOs on setup.
%% Button and accelerometer/calibration button GPIOs are configured according
%% to various prototype versions.
%% GPIO deep sleep is alway configured to wake up on button and accelerometer
%% @end
configure_gpios() ->
    ok = gpio:init(?BUTTON_GPIO),
    ok = gpio:set_pin_mode(?BUTTON_GPIO, input),
    ok = gpio:set_pin_pull(?BUTTON_GPIO, ?BUTTON_GPIO_PULL),
    ok = gpio:init(?ACC_IRQ_GPIO),
    ok = gpio:set_pin_mode(?ACC_IRQ_GPIO, input),
    ok = gpio:set_pin_pull(?ACC_IRQ_GPIO, ?ACC_IRQ_GPIO_PULL),
    ok = esp:deep_sleep_enable_gpio_wakeup(
        (1 bsl ?BUTTON_GPIO) bor (1 bsl ?ACC_IRQ_GPIO), ?BUTTON_GPIO_WAKEUP_LEVEL
    ).

read_button() ->
    case gpio:digital_read(?BUTTON_GPIO) of
        ?BUTTON_GPIO_OFF -> off;
        ?BUTTON_GPIO_ON -> on
    end.

-spec run() -> no_return().
run() ->
    WatchdogUser = configure_watchdog(),

    la_machine_battery:init(),
    WakeupCause = esp:sleep_get_wakeup_cause(),

    configure_gpios(),
    ButtonState = read_button(),

    Config = la_machine_configuration:load(),
    Charging = la_machine_battery:is_charging(),
    BatteryLevel = la_machine_battery:get_level(),

    case WakeupCause of
        undefined ->
            % Not a regular wake-up, report self-test
            la_machine_selftest:report(Config, true);
        sleep_wakeup_timer when ButtonState =:= off ->
            case {Charging, BatteryLevel} of
                {true, _} ->
                    la_machine_selftest:report(Config, true);
                {false, {ok, 100}} ->
                    la_machine_selftest:report(Config, true);
                _ ->
                    la_machine_selftest:report(Config, false)
            end;
        _ ->
            ok
    end,

    SleepTimer =
        case la_machine_configuration:configured(Config) of
            true ->
                run_configured(Config, WakeupCause, ButtonState);
            false ->
                la_machine_selftest:run(Config, WakeupCause, ButtonState)
        end,
    SleepMS = setup_sleep(SleepTimer),
    unconfigure_watchdog(WatchdogUser),
    esp:deep_sleep(SleepMS).

-spec run_configured(la_machine_configuration:config(), esp:esp_wakeup_cause(), on | off) ->
    non_neg_integer().
run_configured(Config, WakeupCause, ButtonState) ->
    AccelerometerState = la_machine_lis3dh:setup(),

    State0 = la_machine_state:load_state(),
    State1 =
        case action(WakeupCause, ButtonState, AccelerometerState, State0) of
            {play, meuh} ->
                play_meuh(Config),
                State0;
            {play, SecondsElapsed, LastPlaySeq, PlayIndex} ->
                {PlayedSeq, NextPlayIndex} = play(SecondsElapsed, LastPlaySeq, PlayIndex, Config),
                la_machine_state:append_play(PlayedSeq, NextPlayIndex, State0);
            {poke, PokeIndex} ->
                poke(PokeIndex, Config),
                la_machine_state:set_poke_index(PokeIndex + 1, State0);
            reset ->
                la_machine_audio:reset(),
                la_machine_servo:reset(Config),
                State0
        end,
    SleepTimer = compute_sleep_timer(State1),
    la_machine_state:save_state(State1),
    SleepTimer.

-ifndef(TEST).
%% @doc Entry point
%% @end
-spec start() -> no_return().
start() ->
    run().
-endif.

% See https://github.com/atomvm/atomvm_packbeam/issues/58
-spec prune_workaround() -> ok.
prune_workaround() ->
    _ = code_server:module_info(),
    ok.

-spec action(
    esp:esp_wakeup_cause(), on | off, ok | {play, meuh} | not_resting, la_machine_state:state()
) -> action().
action(_WakeupCause, _ButtonState, {play, meuh}, _State) ->
    {play, meuh};
action(_WakeupCause, _ButtonState, not_resting, _State) ->
    reset;
action(_WakeupCause, on, ok, State) ->
    LastPlayTime = la_machine_state:get_last_play_time(State),
    LastPlaySeq = la_machine_state:get_last_play_seq(State),
    PlayIndex = la_machine_state:get_play_index(State),
    SecondsElapsed = erlang:system_time(second) - LastPlayTime,
    {play, SecondsElapsed, LastPlaySeq, PlayIndex};
action(sleep_wakeup_gpio, off, ok, _State) ->
    reset;
action(sleep_wakeup_timer, _ButtonState, ok, State) ->
    {poke, la_machine_state:get_poke_index(State)};
action(undefined, _ButtonState, _AccelerometerState, _State) ->
    reset.

poke(_PokeIndex, Config) ->
    PokeScenario = la_machine_scenarios:get(poke, 1),
    PokeScenarioPart = hd(PokeScenario),
    {ok, Pid} = la_machine_player:start_link(Config),
    ok = la_machine_player:play(Pid, PokeScenarioPart),
    ok = la_machine_player:stop(Pid),
    ok.

play_meuh(Config) ->
    <<RandScenario>> = crypto:strong_rand_bytes(1),
    Count = la_machine_scenarios:count(meuh),
    ScenarioIx = 1 + (RandScenario rem Count),
    MeuhScenario = la_machine_scenarios:get(meuh, ScenarioIx),
    MeuhScenarioPart = hd(MeuhScenario),
    {ok, Pid} = la_machine_player:start_link(Config),
    ok = la_machine_player:play(Pid, MeuhScenarioPart),
    ok = la_machine_player:stop(Pid),
    ok.

play(ElapsedSeconds, LastPlaySeq, PlayIndex, Config) when
    PlayIndex > 0 andalso ElapsedSeconds < ?PARTS_MAX_ELAPSE_INTERVAL
->
    % play next part of scenario
    ScenarioParts = la_machine_scenarios:get(play, LastPlaySeq),
    ScenarioPart = lists:nth(PlayIndex, ScenarioParts),
    {ok, Pid} = la_machine_player:start_link(Config),
    ok = la_machine_player:play(Pid, ScenarioPart),
    ok = la_machine_player:stop(Pid),
    if
        length(ScenarioParts) > PlayIndex ->
            {LastPlaySeq, PlayIndex + 1};
        true ->
            {LastPlaySeq, 0}
    end;
play(_ElapsedSeconds, LastPlaySeq, _PlayIndex, Config) ->
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
    {ok, Pid} = la_machine_player:start_link(Config),
    ok = la_machine_player:play(Pid, ScenarioPart),
    ok = la_machine_player:stop(Pid),
    if
        length(ScenarioParts) > 1 ->
            {ScenarioIx, 2};
        true ->
            {ScenarioIx, 0}
    end.

setup_sleep(SleepSecs) ->
    gpio:deep_sleep_hold_en(),
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
