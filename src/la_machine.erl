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

play_scenarios() ->
    [
        [
            {aac, <<"gears/17243.aac">>},
            {servo, 20},
            {aac, <<"hits/party-blower-fail-soundroll-1-1-00-01.aac">>},
            {servo, 100},
            {servo, 0}
        ],
        [
            {servo, 5},
            {aac, <<"gears/8665.aac">>},
            {servo, 25, 8000},
            {wait, sound},
            {servo, 100},
            {aac, <<"hits/glass.aac">>},
            {wait, sound},
            {servo, 0}
        ]
    ].

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

-ifdef(MODEL_PROTO_PAPER_TOY).
configure_button() ->
    ok = gpio:init(?BUTTON_GPIO),
    ok = gpio:set_pin_mode(?BUTTON_GPIO, input),
    ok = gpio:set_pin_pull(?BUTTON_GPIO, up),
    ok = esp:deep_sleep_enable_gpio_wakeup(1 bsl ?BUTTON_GPIO, 0).

read_button() ->
    case gpio:digital_read(?BUTTON_GPIO) of
        high -> off;
        low -> on
    end.
-endif.

-ifdef(MODEL_PROTO_20240718).
configure_button() ->
    ok = gpio:init(?BUTTON_GPIO),
    ok = gpio:set_pin_mode(?BUTTON_GPIO, input),
    ok = gpio:set_pin_pull(?BUTTON_GPIO, down),
    ok = esp:deep_sleep_enable_gpio_wakeup(1 bsl ?BUTTON_GPIO, 1).

read_button() ->
    case gpio:digital_read(?BUTTON_GPIO) of
        high -> on;
        low -> off
    end.
-endif.

-spec run() -> no_return().
run() ->
    WatchdogUser = configure_watchdog(),
    % Configure button GPIO
    configure_button(),

    WakeupCause = esp:sleep_get_wakeup_cause(),
    ButtonState = read_button(),

    State0 = la_machine_state:load_state(),

    State1 =
        case action(WakeupCause, ButtonState, State0) of
            {play, LastPlayTime, LastPlayIndex, PlayIndex} ->
                play(LastPlayTime, LastPlayIndex, PlayIndex),
                la_machine_state:append_play(1, 0, State0);
            {poke, PokeIndex} ->
                poke(),
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
    {play, LastPlayTime, LastPlaySeq, PlayIndex};
action(sleep_wakeup_gpio, off, _State) ->
    reset;
action(sleep_wakeup_timer, _ButtonState, State) ->
    {poke, la_machine_state:get_poke_index(State)};
action(undefined, _ButtonState, _State) ->
    reset.

poke() ->
    {ok, Pid} = la_machine_player:start_link(),
    ok = la_machine_player:play(Pid, [{servo, 20}, {servo, 0}]),
    ok = la_machine_player:stop(Pid),
    ok.

play(_LastPlayTime, _LastPlayIndex, _PlayIndex) ->
    {ok, Pid} = la_machine_player:start_link(),
    <<RandScenario:56>> = crypto:strong_rand_bytes(7),
    Scenarios = play_scenarios(),
    Scenario = lists:nth(1 + (RandScenario rem length(Scenarios)), Scenarios),
    ok = la_machine_player:play(Pid, Scenario),
    ok = la_machine_player:stop(Pid),
    ok.

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
        ?_assertEqual({poke, 0}, action(sleep_wakeup_timer, off, la_machine_state:new())),
        ?_assertEqual(reset, action(undefined, off, la_machine_state:new())),
        ?_assertEqual(
            {play, 0, undefined, 0}, action(sleep_wakeup_timer, on, la_machine_state:new())
        ),
        ?_assertEqual(
            {play, 0, undefined, 0}, action(sleep_wakeup_gpio, on, la_machine_state:new())
        ),
        ?_assertEqual({play, 0, undefined, 0}, action(undefined, on, la_machine_state:new()))
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
