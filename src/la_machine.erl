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
    | play_poke
    | play_meuh
    | play_wait_5000
    | {
        play_scenario,
        Mood :: atom(),
        Index :: non_neg_integer()
    }
    | {
        play,
        Reason :: atom(),
        Mood :: atom(),
        SecondsElapsed :: non_neg_integer(),
        LastPlaySeq :: non_neg_integer() | undefined,
        GestureCount :: non_neg_integer()
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
    SelfTestState = la_machine_configuration:self_test_state(Config),

    SleepTimer =
        case SelfTestState of
            uncalibrated ->
                la_machine_selftest:run(Config, WakeupCause, ButtonState);
            unreported ->
                Timer = la_machine_selftest:report(Config),
                Config1 = la_machine_configuration:set_self_test_reported(Config),
                la_machine_configuration:save(Config1),
                Timer;
            calibrated ->
                run_configured(Config, WakeupCause, ButtonState)
        end,
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

% See https://github.com/atomvm/atomvm_packbeam/issues/58
-spec prune_workaround() -> ok.
prune_workaround() ->
    _ = code_server:module_info(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% do_process_click(WakeupCause, ButtonState, State)
%% process_click(WakeupCause, ButtonState, DurMs, ClickCnt, IsPaused, State)
%% BROKEN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec do_process_click(esp:esp_wakeup_cause() | undefined, on | off, la_machine_state:state()) ->
    la_machine_state:state().

-if(?TRIPLECLICK == 1).
-spec process_click(
    esp:esp_wakeup_cause() | undefined,
    on | off,
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    la_machine_state:state()
) -> la_machine_state:state().

do_process_click(WakeupCause, ButtonState, State0) ->
    DurMS = la_machine_state:get_ms_since_last_on(State0),
    ClickCnt = la_machine_state:get_click_count(State0),
    IsPaused = la_machine_state:get_is_paused(State0),
    process_click(WakeupCause, ButtonState, DurMS, ClickCnt, IsPaused, State0).

process_click(sleep_wakeup_gpio, on, _DurMs, _ClickCnt, _IsPaused, State) ->
    % on : remember time
    la_machine_state:set_last_on_now(State);
process_click(sleep_wakeup_gpio, off, DurMs, ClickCnt, IsPaused, State) when
    DurMs =< 1000 andalso ClickCnt == 2
->
    % off : triple click -> inverse paused
    NewPaused = 1 - IsPaused,
    io:format("NewPaused=~p\n", [NewPaused]),
    la_machine_state:set_is_paused(NewPaused, State);
process_click(sleep_wakeup_gpio, off, DurMs, ClickCnt, _IsPaused, State) when
    % ClickCnt =< 2
    DurMs =< 1000
->
    % off : click, and < three => increase count
    la_machine_state:set_click_count(ClickCnt + 1, State);
process_click(sleep_wakeup_gpio, off, _DurMs, _ClickCnt, _IsPaused, State) ->
    % DurMs > 1000
    % off : no click => set count to 0
    la_machine_state:set_click_count(0, State);
% all other cases
process_click(_, _, _, _, _, State) ->
    State.

-else.

do_process_click(_, _, State) -> State.

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec compute_action(
    non_neg_integer(),
    esp:esp_wakeup_cause() | undefined,
    on | off,
    ok | {play, meuh} | not_resting | replaced,
    la_machine_state:state()
) -> action().

-if(?DEBUG_PLAY_ONLY_ONE_MOOD == 1).
% DEBUG
compute_action(0, _WakeupCause, on, ok, _State) ->
    io:format("play one mood : ~p, Index =~p\n", [
        ?DEBUG_PLAY_ONLY_ONE_MOOD_MOOD, ?DEBUG_PLAY_ONLY_ONE_MOOD_INDEX
    ]),
    {play_scenario, ?DEBUG_PLAY_ONLY_ONE_MOOD_MOOD, ?DEBUG_PLAY_ONLY_ONE_MOOD_INDEX};
compute_action(IsPausedNow, WakeupCause, ButtonState, AccelerometerState, State) ->
    action(IsPausedNow, WakeupCause, ButtonState, AccelerometerState, State).

%% erlfmt:ignore-begin
-if(?DEBUG_PLAY_ONLY_ONE_MOOD_INDEX < 0).
-define(DEBUG_PLAY_SCENARIO_CASE(Config, StateX),
    {play_scenario, DebugMood, _DebugIndex} ->
        play_random_scenario_with_hit(DebugMood, undefined, Config),
        StateX;
).
-else.
-define(DEBUG_PLAY_SCENARIO_CASE(Config, StateX),
    {play_scenario, DebugMood, DebugIndex} ->
        play_scenario_with_hit(DebugMood, DebugIndex, Config),
        StateX;
).
-endif.
%% erlfmt:ignore-end

-else.

compute_action(IsPausedNow, WakeupCause, ButtonState, AccelerometerState, State) ->
    action(IsPausedNow, WakeupCause, ButtonState, AccelerometerState, State).

%% erlfmt:ignore-begin
-define(DEBUG_PLAY_SCENARIO_CASE(Config, StateX),).
%% erlfmt:ignore-end

-endif.

-spec run_configured(
    la_machine_configuration:config(),
    esp:esp_wakeup_cause() | undefined,
    on | off
) ->
    non_neg_integer().
run_configured(Config, WakeupCause, ButtonState) ->
    State0 = la_machine_state:load_state(),
    AccelerometerState = la_machine_lis3dh:setup(),

    %% process click
    StateX = do_process_click(WakeupCause, ButtonState, State0),
    IsPausedNow = la_machine_state:get_is_paused(StateX),

    State1 =
        case compute_action(IsPausedNow, WakeupCause, ButtonState, AccelerometerState, StateX) of
            ?DEBUG_PLAY_SCENARIO_CASE(Config, StateX)
            play_meuh ->
                play_meuh(Config),
                StateX;
            play_poke ->
                % the poke
                PokeIndex = la_machine_state:get_poke_index(StateX),
                play_poke(Config),
                la_machine_state:set_poke_index(PokeIndex + 1, StateX);
            % normal play
            {play, Reason, Mood, SecondsElapsed, LastPlaySeq, GestureCount} ->
                io:format(
                    "Play Reason=~s Mood=~s GestureCount=~p LastPlaySeq=~p SecondsElapsed=~p\n", [
                        Reason, Mood, GestureCount, LastPlaySeq, SecondsElapsed
                    ]
                ),

                % change mood ?
                Total_Gesture_Count = la_machine_state:get_total_gestures_count(StateX),
                {Mood1, GestureCount1, LastPlaySeq1} = change_moodp(
                    Mood, Reason, GestureCount, Total_Gesture_Count, SecondsElapsed, LastPlaySeq
                ),

                if
                    Mood1 == waiting ->
                        % waiting : don't play anything
                        % Only change the mood
                        la_machine_state:set_mood_waiting(StateX);
                    true ->
                        % else : play and remember
                        PlayedSeq = play_mood(Mood1, SecondsElapsed, LastPlaySeq1, Config),
                        la_machine_state:append_play(Mood1, GestureCount1 + 1, PlayedSeq, StateX)
                end;
            reset ->
                io:format("resetting\n"),
                la_machine_audio:reset(),
                la_machine_servo:reset(Config),
                StateX;
            play_wait_5000 ->
                play_wait_5000(Config),
                la_machine_audio:reset(),
                la_machine_servo:reset(Config),
                StateX
        end,
    la_machine_state:save_state(State1),
    do_compute_sleep_timer(State1).

% action
-spec action(
    non_neg_integer(),
    esp:esp_wakeup_cause() | undefined,
    on | off,
    ok | {play, meuh} | not_resting | replaced,
    la_machine_state:state()
) -> action().

% paused
action(1, _WakeupCause, _ButtonState, _AccelerometerState, _State) ->
    io:format("We are paused\n"),
    play_wait_5000;
% meuh
action(_IsPausedNow, _WakeupCause, _ButtonState, {play, meuh}, _State) ->
    play_meuh;
% not resting
action(_IsPausedNow, _WakeupCause, _ButtonState, not_resting, _State) ->
    reset;
% replaced => play_poke
action(_IsPausedNow, _WakeupCause, _ButtonState, replaced, _State) ->
    play_poke;
% button on
action(_IsPausedNow, _WakeupCause, on, ok, State) ->
    {Mood, LastPlaySeq, GestureCount, LastPlayTime} = la_machine_state:get_play_info(State),
    SecondsElapsed = erlang:system_time(second) - LastPlayTime,
    {play, player, Mood, SecondsElapsed, LastPlaySeq, GestureCount};
% button off
action(_IsPausedNow, sleep_wakeup_gpio, off, ok, _State) ->
    reset;
% timer
action(_IsPausedNow, sleep_wakeup_timer, _ButtonState, ok, State) ->
    {Mood, LastPlaySeq, GestureCount, LastPlayTime} = la_machine_state:get_play_info(State),
    if
        Mood == waiting ->
            play_poke;
        true ->
            SecondsElapsed = erlang:system_time(second) - LastPlayTime,
            {play, timer, Mood, SecondsElapsed, LastPlaySeq, GestureCount}
    end;
% catch all
action(_IsPausedNow, undefined, _ButtonState, _AccelerometerState, _State) ->
    reset.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% check mood change
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec change_moodp(
    Mood :: atom(),
    Reason :: atom(),
    GestureCount :: non_neg_integer(),
    Total_Gesture_Count :: non_neg_integer(),
    SecondsElapsed :: non_neg_integer(),
    LastPlaySeq :: non_neg_integer() | undefined
) ->
    {
        NewMood :: atom(),
        NewGestureCount :: non_neg_integer(),
        NewLastPlaySeq :: non_neg_integer() | undefined
    }.

% timer while calling -> continue calling or start waiting
change_moodp(calling, timer, GestureCount, Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    io:format("Wakeup while calling\n"),
    % number of calls depends on Total_Gesture_Count
    Max_Calling_Times =
        if
            Total_Gesture_Count < 3 -> 1;
            Total_Gesture_Count < 10 -> 2;
            true -> ?MAX_CALLING_SOUNDS
        end,
    if
        GestureCount >= Max_Calling_Times ->
            io:format("   max times (TotalGesture=~p => MaxTimes=~p)=> waiting\n", [
                Total_Gesture_Count, Max_Calling_Times
            ]),
            {waiting, 0, undefined};
        true ->
            io:format("   continue calling\n"),
            {calling, GestureCount, LastPlaySeq}
    end;
% timer while all other moods -> start calling
change_moodp(Mood, timer, _GestureCount, _Total_Gesture_Count, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Wakeup while in ~s => calling\n", [Mood]),
    {calling, 0, undefined};
% player plays while calling -> game imitation
change_moodp(calling, player, _GestureCount, _Total_Gesture_Count, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Was calling => imitation\n"),
    {imitation, 0, undefined};
% player plays while waiting -> joy
change_moodp(waiting, player, _GestureCount, _Total_Gesture_Count, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Long time no see => joy\n"),
    {joy, 0, undefined};
% joy : mood change ?
change_moodp(joy, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    Mood = joy,
    % if more than JOY_MIN_GESTURES gestures, one chance out of ?JOY_IMIT_CHANCE to go to imitation
    if
        GestureCount > ?JOY_MIN_GESTURES ->
            <<RandChange:56>> = crypto:strong_rand_bytes(7),
            if
                (RandChange rem ?JOY_IMIT_CHANCE) == 0 ->
                    TmpMood = imitation,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                true ->
                    {Mood, GestureCount, LastPlaySeq}
            end;
        true ->
            {Mood, GestureCount, LastPlaySeq}
    end;
% imitation : mood change ?
change_moodp(imitation, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    Mood = imitation,
    <<RandChangeDial:56>> = crypto:strong_rand_bytes(7),
    <<RandChangeUpset:56>> = crypto:strong_rand_bytes(7),
    <<RandChangeTired:56>> = crypto:strong_rand_bytes(7),
    <<RandChangeExcited:56>> = crypto:strong_rand_bytes(7),
    if
        GestureCount > ?IMIT_MIN_GESTURES ->
            if
                (RandChangeDial rem ?IMIT_DIAL_CHANCE) == 0 ->
                    TmpMood = dialectic,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                (RandChangeUpset rem ?IMIT_UPSET_CHANCE) == 0 ->
                    TmpMood = upset,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                (RandChangeTired rem ?IMIT_TIRED_CHANCE) == 0 ->
                    TmpMood = tired,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                (RandChangeExcited rem ?IMIT_EXCITED_CHANCE) == 0 ->
                    TmpMood = excited,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                true ->
                    {Mood, GestureCount, LastPlaySeq}
            end;
        true ->
            {Mood, GestureCount, LastPlaySeq}
    end;
% dialectic : mood change ?
change_moodp(dialectic, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    Mood = dialectic,
    <<RandChangeImit:56>> = crypto:strong_rand_bytes(7),
    <<RandChangeUpset:56>> = crypto:strong_rand_bytes(7),
    <<RandChangeTired:56>> = crypto:strong_rand_bytes(7),
    <<RandChangeExcited:56>> = crypto:strong_rand_bytes(7),
    if
        GestureCount > ?DIAL_MIN_GESTURES ->
            if
                (RandChangeImit rem ?DIAL_IMIT_CHANCE) == 0 ->
                    TmpMood = imitation,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                (RandChangeUpset rem ?DIAL_UPSET_CHANCE) == 0 ->
                    TmpMood = upset,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                (RandChangeTired rem ?DIAL_TIRED_CHANCE) == 0 ->
                    TmpMood = tired,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                (RandChangeExcited rem ?DIAL_EXCITED_CHANCE) == 0 ->
                    TmpMood = excited,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                true ->
                    {Mood, GestureCount, LastPlaySeq}
            end;
        true ->
            {Mood, GestureCount, LastPlaySeq}
    end;
% upset, tired, excited : mood change ?
change_moodp(Mood, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) when
    Mood == upset orelse Mood == tired orelse Mood == excited
->
    <<RandChange:56>> = crypto:strong_rand_bytes(7),
    if
        GestureCount > ?DIAL_MIN_GESTURES ->
            if
                (RandChange rem ?MOODY_IMIT_CHANCE) == 0 ->
                    TmpMood = imitation,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                true ->
                    {Mood, GestureCount, LastPlaySeq}
            end;
        true ->
            {Mood, GestureCount, LastPlaySeq}
    end;
% catch all : no change
change_moodp(Mood, _Reason, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    {Mood, GestureCount, LastPlaySeq}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec play_mood(
    atom(), non_neg_integer(), non_neg_integer() | undefined, la_machine_configuration:config()
) ->
    pos_integer().
play_mood(imitation, ElapsedSeconds, LastPlaySeq, Config) ->
    Mood = imitation,
    io:format("playing mood : ~s ElapsedSeconds=~p\n", [Mood, ElapsedSeconds]),

    MoodScenar =
        if
            ElapsedSeconds =< ?GAME_SHORT_DUR_S ->
                game_short;
            ElapsedSeconds =< ?GAME_MEDIUM_DUR_S ->
                game_medium;
            true ->
                game_long
        end,
    play_random_scenario_with_hit(MoodScenar, LastPlaySeq, Config);
play_mood(dialectic, ElapsedSeconds, LastPlaySeq, Config) ->
    Mood = dialectic,
    io:format("playing mood : ~s ElapsedSeconds=~p\n", [Mood, ElapsedSeconds]),
    % play inverse
    MoodScenar =
        if
            ElapsedSeconds =< ?GAME_MEDIUM_DUR_S ->
                game_long;
            true ->
                game_short
        end,
    play_random_scenario_with_hit(MoodScenar, LastPlaySeq, Config);
% calling (no hit)
play_mood(calling, _ElapsedSeconds, LastPlaySeq, Config) ->
    Mood = calling,
    io:format("playing mood : ~s\n", [Mood]),
    MoodScenar = Mood,
    play_random_scenario(MoodScenar, LastPlaySeq, Config);
% all others : joy, tired, upset, excited
play_mood(Mood, _ElapsedSeconds, LastPlaySeq, Config) ->
    io:format("playing mood : ~s\n", [Mood]),
    MoodScenar = Mood,
    play_random_scenario_with_hit(MoodScenar, LastPlaySeq, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_wait_5000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_wait_5000(la_machine_configuration:config()) -> ok.
play_wait_5000(Config) ->
    io:format("play_wait_5000\n"),
    ScenarioPart = [{wait, 5000}],
    {ok, Pid} = la_machine_player:start_link(Config),
    ok = la_machine_player:play(Pid, ScenarioPart),
    ok = la_machine_player:stop(Pid),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_meuh
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_meuh(la_machine_configuration:config()) -> pos_integer().
play_meuh(Config) ->
    play_random_scenario(meuh, undefined, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_poke
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_poke(la_machine_configuration:config()) -> pos_integer().
play_poke(Config) ->
    play_random_scenario(poke, undefined, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_random_hit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_random_hit(pid()) -> pos_integer().
play_random_hit(Pid) ->
    MoodScenar = hits,
    ScenarioCount = la_machine_scenarios:count(MoodScenar),
    ScenarioIx = random_num_upto_butnot(ScenarioCount, undefined),
    io:format("play_random_hit=~p\n", [ScenarioIx]),
    Scenario = la_machine_scenarios:get(MoodScenar, ScenarioIx),
    % could be adapted to length of hit
    Scenario_Full = Scenario ++ [{servo, 100}, {wait, 200}, {servo, 0}],
    ok = la_machine_player:play(Pid, Scenario_Full),
    ScenarioIx.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_random_scenario : play random scenario of MoodScenar, but not LastPlaySeq if possible
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_random_scenario(
    atom(), non_neg_integer() | undefined, la_machine_configuration:config()
) ->
    pos_integer().
play_random_scenario(MoodScenar, LastPlaySeq, Config) ->
    ScenarioCount = la_machine_scenarios:count(MoodScenar),
    io:format("play_random_scenario MoodScenar=~p ScenarioCount=~p\n", [MoodScenar, ScenarioCount]),

    % play random scenario
    ScenarioIx = random_num_upto_butnot(ScenarioCount, LastPlaySeq),
    io:format("     ScenarioIx=~p\n", [ScenarioIx]),
    play_scenario(MoodScenar, ScenarioIx, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_random_scenario_with_hit : play_random_scenario + play hit if needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_random_scenario_with_hit(
    atom(), non_neg_integer() | undefined, la_machine_configuration:config()
) ->
    pos_integer().
play_random_scenario_with_hit(MoodScenar, LastPlaySeq, Config) ->
    ScenarioCount = la_machine_scenarios:count(MoodScenar),
    io:format("play_random_scenario_with_hit MoodScenar=~p ScenarioCount=~p\n", [
        MoodScenar, ScenarioCount
    ]),

    % play random scenario
    ScenarioIx = random_num_upto_butnot(ScenarioCount, LastPlaySeq),
    io:format("     ScenarioIx=~p\n", [ScenarioIx]),
    play_scenario_with_hit(MoodScenar, ScenarioIx, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_scenario : play scenario ScenarioIx of MoodScenar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_scenario(atom(), pos_integer(), la_machine_configuration:config()) -> pos_integer().
play_scenario(MoodScenar, ScenarioIx, Config) ->
    Scenario = la_machine_scenarios:get(MoodScenar, ScenarioIx),
    {ok, Pid} = la_machine_player:start_link(Config),
    ok = la_machine_player:play(Pid, Scenario),
    ok = la_machine_player:stop(Pid),
    % reset audio and servo
    la_machine_audio:reset(),
    la_machine_servo:reset(Config),
    ScenarioIx.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_scenario_with_hit : play scenario ScenarioIx of MoodScenar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_scenario_with_hit(atom(), pos_integer(), la_machine_configuration:config()) ->
    pos_integer().
play_scenario_with_hit(MoodScenar, ScenarioIx, Config) ->
    Scenario = la_machine_scenarios:get(MoodScenar, ScenarioIx),
    {ok, Pid} = la_machine_player:start_link(Config),
    ok = la_machine_player:play(Pid, Scenario),
    % play hit if needed
    ButtonState = read_button(),
    io:format("   after play ButtonState=~s\n", [ButtonState]),
    if
        ButtonState == on -> play_random_hit(Pid);
        true -> true
    end,
    ok = la_machine_player:stop(Pid),
    % reset audio and servo
    la_machine_audio:reset(),
    la_machine_servo:reset(Config),
    ScenarioIx.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% setup_sleep
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec setup_sleep(non_neg_integer()) -> non_neg_integer().
setup_sleep(SleepSecs) ->
    gpio:deep_sleep_hold_en(),
    SleepMs = SleepSecs * 1000,
    SleepMs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compute_sleep_timer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec do_compute_sleep_timer(la_machine_state:state()) -> integer().
do_compute_sleep_timer(State) ->
    Mood = la_machine_state:get_mood(State),
    compute_sleep_timer(State, Mood).

-spec compute_sleep_timer(la_machine_state:state(), atom()) -> integer().
% waiting
% starts with 24 hours followed by 48 hours
% NextPokeIndex starts at 0
compute_sleep_timer(State, waiting) ->
    NextPokeIndex = la_machine_state:get_poke_index(State),
    io:format("compute_sleep_timer waiting NextPokeIndex=~p\n", [NextPokeIndex]),
    HoursToWait =
        if
            NextPokeIndex > 0 ->
                PokeDelay = fib(NextPokeIndex) * 24,
                io:format("Next poke in ~ph\n", [PokeDelay]),
                PokeDelay;
            true ->
                % first time
                PlayHoursCount = la_machine_state:get_play_hours_count(State),
                case PlayHoursCount of
                    0 ->
                        24;
                    N ->
                        % pick a play hour at random
                        % if more than 3 hours after now : take it, otherwise add 24h
                        <<RandHour:56>> = crypto:strong_rand_bytes(7),
                        ChosenHour = la_machine_state:get_play_hour(RandHour rem N, State),
                        NowS = la_machine_state:get_seconds_since_boot(State),
                        NowHour = ((NowS div 3600) rem 24),
                        % at least 3 hours after now
                        Delay0 = (ChosenHour - NowHour + 24) rem 24,
                        io:format("First poke, NowHour=~ph, ChosenHour=~ph, Delay0=~ph\n", [
                            NowHour, ChosenHour, Delay0
                        ]),
                        if
                            Delay0 >= 3 -> Delay0;
                            true -> Delay0 + 24
                        end
                end
        end,
    io:format(" poke in ~ph\n", [HoursToWait]),
    <<RandSec:56>> = crypto:strong_rand_bytes(7),
    HoursToWait * 3600 + (RandSec rem 3600) - 1800;
% calling => compute delay from GestureCount
compute_sleep_timer(State, calling) ->
    % delay = random(CALLING_MIN_DELAY_S, CALLING_MAX_DELAY_S)
    GestureCount = la_machine_state:get_gestures_count(State),
    <<RandS:56>> = crypto:strong_rand_bytes(7),
    Delay = (?CALLING_MIN_DELAY_S + (RandS rem (?CALLING_MAX_DELAY_S - ?CALLING_MIN_DELAY_S))),
    io:format("compute_sleep_timer calling : sleep ~ps (GestureCount=~p)\n", [Delay, GestureCount]),
    Delay;
% other cases : delay first calling
compute_sleep_timer(_State, Mood) ->
    Delay = ?CALLING_START_DELAY_S,
    io:format("compute_sleep_timer ~s : sleep default ~ps\n", [Mood, Delay]),
    Delay.

%% fib
-spec fib(pos_integer()) -> pos_integer().
fib(X) -> fib(X, 0, 1).

-spec fib(pos_integer(), non_neg_integer(), pos_integer()) -> pos_integer().
fib(1, _A, B) -> B;
fib(I, A, B) -> fib(I - 1, B, A + B).

% recursive implementation of random number
% return a random number 1 <= N <= MaxNumber, but not equel to NotNumber (if defined)
-spec random_num_upto_butnot(pos_integer(), pos_integer() | undefined) -> pos_integer().
random_num_upto_butnot(1, _NotNumber) ->
    1;
random_num_upto_butnot(MaxNumber, NotNumber) when NotNumber =:= undefined ->
    <<RndNum:56>> = crypto:strong_rand_bytes(7),
    1 + (RndNum rem MaxNumber);
random_num_upto_butnot(MaxNumber, NotNumber) ->
    Trial = random_num_upto_butnot(MaxNumber, undefined),
    if
        % recurse, try again if we got the excluded number
        NotNumber == Trial -> random_num_upto_butnot(MaxNumber, NotNumber);
        true -> Trial
    end.

% fib(X) -> fib(X, 0, 1).
% fib(1, _A, B) -> B;
% fib(I, A, B) -> fib(I - 1, B, A + B).

-ifdef(TEST).
action_test_() ->
    [
        ?_assertEqual(play_poke, action(0, sleep_wakeup_timer, off, ok, la_machine_state:new())),
        ?_assertEqual(reset, action(0, undefined, off, ok, la_machine_state:new())),
        ?_assertMatch(
            {play, player, waiting, _ElapsedSecs, 0, 0},
            action(0, sleep_wakeup_timer, on, ok, la_machine_state:new())
        ),
        ?_assertMatch(
            {play, player, waiting, _ElapsedSecs, 0, 0},
            action(0, sleep_wakeup_gpio, on, ok, la_machine_state:new())
        ),
        ?_assertMatch(
            {play, player, waiting, _ElapsedSecs, 0, 0},
            action(0, undefined, on, ok, la_machine_state:new())
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
            "La Machine pokes between 23.5 and 24.5 hours after reboot (waiting)",
            [
                ?_assert(compute_sleep_timer(la_machine_state:new(), waiting) >= 23 * 3600 + 1800),
                ?_assert(compute_sleep_timer(la_machine_state:new(), waiting) =< 24 * 3600 + 1800)
            ]
        },
        {
            "La Machine pokes between 23.5 and 24.5 hours after first poke (fib(1)=1)",
            [
                ?_assert(
                    compute_sleep_timer(
                        la_machine_state:set_poke_index(1, la_machine_state:new()), waiting
                    ) >= 23 * 3600 + 1800
                ),
                ?_assert(
                    compute_sleep_timer(
                        la_machine_state:set_poke_index(1, la_machine_state:new()), waiting
                    ) =< 24 * 3600 + 1800
                )
            ]
        },
        {
            "La Machine pokes between 47.5 and 48.5 hours after third poke (fib(3)=2)",
            [
                ?_assert(
                    compute_sleep_timer(
                        la_machine_state:set_poke_index(3, la_machine_state:new()), waiting
                    ) >= 47 * 3600 + 1800
                ),
                ?_assert(
                    compute_sleep_timer(
                        la_machine_state:set_poke_index(3, la_machine_state:new()), waiting
                    ) =< 48 * 3600 + 1800
                )
            ]
        }
    ].
-endif.
