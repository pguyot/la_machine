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
    | play_battery_low
    | enter_travel
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

%% @doc Configure GPIOs
%% @end
-spec configure_gpios() -> ok.
configure_gpios() ->
    configure_gpio(?BUTTON_GPIO, ?BUTTON_GPIO_PULL),
    configure_gpio(?ACC_IRQ_GPIO, ?ACC_IRQ_GPIO_PULL).

%% @doc Enable GPIOs as deep sleep wakeup sources.
%% `accelerometer` configures only the accelerometer GPIO
%% `button' configures only the button GPIO (self test, provisioning)
%% `both' configures button and accelerometer GPIO (used after calibration).
%% @end
-spec enable_gpio_wakeup(accelerometer | button | both) -> ok.
enable_gpio_wakeup(Pins) ->
    PinMask =
        case Pins of
            accelerometer -> (1 bsl ?ACC_IRQ_GPIO);
            button -> (1 bsl ?BUTTON_GPIO);
            both -> (1 bsl ?BUTTON_GPIO) bor (1 bsl ?ACC_IRQ_GPIO)
        end,
    ok = esp:deep_sleep_enable_gpio_wakeup(PinMask, ?BUTTON_GPIO_WAKEUP_LEVEL),
    gpio:deep_sleep_hold_en().

configure_gpio(GPIO, Pull) ->
    ok = gpio:init(GPIO),
    ok = gpio:set_pin_mode(GPIO, input),
    ok = gpio:set_pin_pull(GPIO, Pull).

read_button() ->
    case gpio:digital_read(?BUTTON_GPIO) of
        ?BUTTON_GPIO_OFF -> off;
        ?BUTTON_GPIO_ON -> on
    end.

-spec run() -> no_return().
run() ->
    WatchdogUser = configure_watchdog(),
    ok = configure_gpios(),

    la_machine_battery:init(),
    WakeupCause = esp:sleep_get_wakeup_cause(),
    case WakeupCause of
        undefined ->
            ok = la_machine_sounds:verify_partition();
        _ ->
            ok
    end,

    Config = la_machine_configuration:load(),
    SelfTestState = la_machine_configuration:self_test_state(Config),

    {SleepTimer, WakeupGPIO} =
        case SelfTestState of
            uncalibrated ->
                ButtonState = read_button(),
                Timer = la_machine_selftest:run(Config, WakeupCause, ButtonState),
                {Timer, button};
            unreported ->
                la_machine_selftest:report(Config),
                Config1 = la_machine_configuration:set_self_test_reported(Config),
                la_machine_configuration:save(Config1),
                RTCState0 = la_machine_state:load_state(),
                RTCState1 = la_machine_state:set_wakeup_state(provisioning, RTCState0),
                la_machine_state:save_state(RTCState1),
                {infinity, button};
            calibrated ->
                ButtonState = read_button(),
                BatteryCharging = la_machine_battery:is_charging(),
                RTCState = la_machine_state:load_state(),
                WakeupState = la_machine_state:get_wakeup_state(RTCState),
                run_configured(
                    Config, WakeupCause, ButtonState, RTCState, WakeupState, BatteryCharging
                )
        end,
    enable_gpio_wakeup(WakeupGPIO),
    unconfigure_watchdog(WatchdogUser),
    case SleepTimer of
        infinity ->
            esp:deep_sleep();
        SleepSecs when is_integer(SleepSecs) ->
            esp:deep_sleep(SleepSecs * 1000)
    end.

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
%% action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec compute_action(
    esp:esp_wakeup_cause() | undefined,
    on | off,
    ok | {play, meuh} | not_resting | resting | replaced | upside_down,
    0..100,
    la_machine_state:state()
) -> action().

-if(?DEBUG_PLAY_ONLY_ONE_MOOD == 1).
% DEBUG
compute_action(_WakeupCause, on, ok, _BatteryLevel, _State) ->
    io:format("play one mood : ~p, Index =~p\n", [
        ?DEBUG_PLAY_ONLY_ONE_MOOD_MOOD, ?DEBUG_PLAY_ONLY_ONE_MOOD_INDEX
    ]),
    {play_scenario, ?DEBUG_PLAY_ONLY_ONE_MOOD_MOOD, ?DEBUG_PLAY_ONLY_ONE_MOOD_INDEX};
compute_action(WakeupCause, ButtonState, AccelerometerState, BatteryLevel, State) ->
    action(WakeupCause, ButtonState, AccelerometerState, BatteryLevel, State).

%% erlfmt:ignore-begin
-if(?DEBUG_PLAY_ONLY_ONE_MOOD_INDEX < 0).
-define(DEBUG_PLAY_SCENARIO_CASE(Config, State),
    {play_scenario, DebugMood, _DebugIndex} ->
        play_random_scenario_with_hit(DebugMood, undefined, Config),
        State;
).
-else.
-define(DEBUG_PLAY_SCENARIO_CASE(Config, State),
    {play_scenario, DebugMood, DebugIndex} ->
        play_scenario_with_hit(DebugMood, DebugIndex, Config),
        State;
).
-endif.
%% erlfmt:ignore-end

-else.

compute_action(WakeupCause, ButtonState, AccelerometerState, BatteryLevel, State) ->
    action(WakeupCause, ButtonState, AccelerometerState, BatteryLevel, State).

%% erlfmt:ignore-begin
-define(DEBUG_PLAY_SCENARIO_CASE(Config, State),).
%% erlfmt:ignore-end

-endif.

-spec run_configured(
    la_machine_configuration:config(),
    esp:esp_wakeup_cause() | undefined,
    on | off,
    la_machine_state:state(),
    la_machine_state:wakeup_state(),
    boolean()
) ->
    {non_neg_integer() | infinity, accelerometer | button | both}.
% provisioning state: wait for the button, even if La Machine is being charged
run_configured(_Config, _WakeupCause, off, _State0, provisioning, false) ->
    {infinity, button};
run_configured(_Config, _WakeupCause, off, _State0, provisioning, true) ->
    wait_while_charging(40000),
    {?SERVO_CHARGING_TIMEOUT, button};
run_configured(Config, sleep_wakeup_gpio, on, State0, provisioning, _Charging) ->
    % First run.
    play_welcome(Config),
    State1 = la_machine_state:set_wakeup_state(normal, State0),
    la_machine_state:save_state(State1),
    Timer = do_compute_sleep_timer(State1),
    {Timer, both};
% charging : open the lid while charging and closing it once it's done
% It also exits travel mode
run_configured(Config, _WakeupCause, ButtonState, State0, WakeupState, true) when ButtonState =:= off; WakeupState =:= travel ->
    case WakeupState of
        charging -> ok;
        _ ->
            ServoState0 = la_machine_servo:power_on(Config),
            {TargetMS, _ServoState1} = la_machine_servo:set_target(?SERVO_CHARGING_POSITION, ServoState0),
            timer:sleep(TargetMS),
            la_machine_servo:power_off(),
            State1 = la_machine_state:set_wakeup_state(charging, State0),
            la_machine_state:save_state(State1)
    end,
    wait_while_charging(40000),
    {?SERVO_CHARGING_TIMEOUT, button};
run_configured(Config, _WakeupCause, on, State0, charging, true) ->
    % Close La Machine and then transition to normal to play
    ServoState0 = la_machine_servo:power_on(Config, ?SERVO_CHARGING_POSITION),
    {TargetMS, _ServoState1} = la_machine_servo:set_target(0, ServoState0),
    timer:sleep(TargetMS),
    la_machine_servo:power_off(),
    State1 = la_machine_state:set_wakeup_state(normal, State0),
    la_machine_state:save_state(State1),
    {0, both};
run_configured(Config, _WakeupCause, ButtonState, State0, charging, false) ->
    ServoState0 = la_machine_servo:power_on(Config, ?SERVO_CHARGING_POSITION),
    {TargetMS, _ServoState1} = la_machine_servo:set_target(0, ServoState0),
    timer:sleep(TargetMS),
    la_machine_servo:power_off(),
    State1 = la_machine_state:set_wakeup_state(normal, State0),
    la_machine_state:save_state(State1),
    Timer = case ButtonState of
        off ->
            do_compute_sleep_timer(State1);
        on ->
            0
    end,
    {Timer, both};
run_configured(Config, _WakeupCause, ButtonState, State0, travel, false) ->
    AccelerometerState = la_machine_lis3dh:setup(false),
    case AccelerometerState of
        resting when ButtonState =:= on ->
            {infinity, accelerometer};
        resting when ButtonState =:= off ->
            {infinity, both};
        not_resting when ButtonState =:= on ->
            {infinity, accelerometer};
        not_resting ->
            {infinity, both};
        upside_down ->
            case detect_double_click(ButtonState) of
                true ->
                    play_system_sound(<<"_system/travel_leave.mp3">>, Config),
                    State1 = la_machine_state:set_wakeup_state(normal, State0),
                    la_machine_state:save_state(State1),
                    Timer = do_compute_sleep_timer(State1),
                    {Timer, both};
                false ->
                    case read_button() of
                        on ->
                            % If button is on, user has to shake us as we
                            % are not going to deplete battery waiting for them
                            % to switch the button off.
                            {infinity, accelerometer};
                        off ->
                            {infinity, both}
                    end
            end                    
    end;
run_configured(Config, WakeupCause, ButtonState, State0, normal, Charging) ->
    AccelerometerState = la_machine_lis3dh:setup(ButtonState =:= off),
    {ok, BatteryLevel} = la_machine_battery:get_level(),

    State1 =
        case compute_action(WakeupCause, ButtonState, AccelerometerState, BatteryLevel, State0) of
            ?DEBUG_PLAY_SCENARIO_CASE(Config, State0)
            play_meuh ->
                play_meuh(Config),
                State0;
            play_battery_low ->
                play_battery_low(Config),
                State0;
            play_poke ->
                % the poke
                PokeIndex = la_machine_state:get_poke_index(State0),
                play_poke(Config),
                la_machine_state:set_poke_index(PokeIndex + 1, State0);
            enter_travel ->
                play_system_sound(<<"_system/travel_enter.mp3">>, Config),
                la_machine_state:set_wakeup_state(travel, State0);
            % normal play
            {play, Reason, Mood, SecondsElapsed, LastPlaySeq, GestureCount} ->
                io:format(
                    "Play Reason=~s Mood=~s GestureCount=~p LastPlaySeq=~p SecondsElapsed=~p\n", [
                        Reason, Mood, GestureCount, LastPlaySeq, SecondsElapsed
                    ]
                ),

                % change mood ?
                Total_Gesture_Count = la_machine_state:get_total_gestures_count(State0),
                {Mood1, GestureCount1, LastPlaySeq1} = change_moodp(
                    Mood, Reason, GestureCount, Total_Gesture_Count, SecondsElapsed, LastPlaySeq
                ),

                if
                    Mood1 == waiting ->
                        % waiting : don't play anything
                        % Only change the mood
                        la_machine_state:set_mood_waiting(State0);
                    true ->
                        % else : play and remember
                        PlayedSeq = play_mood(Mood1, SecondsElapsed, LastPlaySeq1, Config),
                        la_machine_state:append_play(Mood1, GestureCount1 + 1, PlayedSeq, State0)
                end;
            reset ->
                la_machine_audio:reset(),
                la_machine_servo:reset(Config),
                State0
        end,
    la_machine_state:save_state(State1),
    Timer = case Charging of
        true ->
            0;
        false ->
            case la_machine_state:get_wakeup_state(State1) of
                travel ->
                    infinity;
                _ ->
                    do_compute_sleep_timer(State1)
            end
    end,
    % Even in travel mode we are waken up by either the button or the accelerometer:
    % we may be already upside down
    {Timer, both};
run_configured(_Config, WakeupCause, ButtonState, _State0, WakeupState, false) ->
    io:format("Unexpected state WakeupCause = ~p, ButtonState = ~p, WakeupState = ~p\n", [WakeupCause, ButtonState, WakeupState]),
    {infinity, both}.

% Busy loop to wait for button or for the end of charging or for 40 secs
% This simplifies firmware loading as the serial port remains open
-spec wait_while_charging(non_neg_integer()) -> ok.
wait_while_charging(TimeoutMS) ->
    Deadline = erlang:system_time(millisecond) + TimeoutMS,
    wait_while_charging_loop(Deadline).

-spec wait_while_charging_loop(integer()) -> ok.
wait_while_charging_loop(Deadline) ->
    case la_machine_battery:is_charging() of
        false ->
            ok;
        true ->
            case read_button() of
                on ->
                    ok;
                off ->
                    case erlang:system_time(millisecond) < Deadline of
                        true ->
                            timer:sleep(100),
                            wait_while_charging_loop(Deadline);
                        false ->
                            ok
                    end
            end
    end.

% action
-spec action(
    esp:esp_wakeup_cause() | undefined,
    on | off,
    ok | {play, meuh} | not_resting | resting | replaced | upside_down,
    0..100,
    la_machine_state:state()
) -> action().

% meuh
action(_WakeupCause, _ButtonState, {play, meuh}, _BatteryLevel, _State) ->
    play_meuh;
% not resting
action(_WakeupCause, _ButtonState, not_resting, _BatteryLevel, _State) ->
    reset;
% replaced => play_poke
action(_WakeupCause, _ButtonState, replaced, _BatteryLevel, _State) ->
    play_poke;
action(WakeupCause, ButtonState, upside_down, BatteryLevel, State) ->
    % Detect double click
    case detect_double_click(ButtonState) of
        true ->
            enter_travel;
        false ->
            action(WakeupCause, ButtonState, ok, BatteryLevel, State)
    end;
% resting -> ok
action(WakeupCause, ButtonState, resting, BatteryLevel, State) ->
    action(WakeupCause, ButtonState, ok, BatteryLevel, State);
% button on
action(_WakeupCause, on, ok, BatteryLevel, _State) when BatteryLevel =< 20 ->
    play_battery_low;
action(_WakeupCause, on, ok, _BatteryLevel, State) ->
    {Mood, LastPlaySeq, GestureCount, LastPlayTime} = la_machine_state:get_play_info(State),
    SecondsElapsed = erlang:system_time(second) - LastPlayTime,
    {play, player, Mood, SecondsElapsed, LastPlaySeq, GestureCount};
% button off
action(sleep_wakeup_gpio, off, ok, _BatteryLevel, _State) ->
    reset;
% timer
action(sleep_wakeup_timer, _ButtonState, ok, BatteryLevel, _State) when BatteryLevel =< 20 ->
    reset;
action(sleep_wakeup_timer, _ButtonState, ok, _BatteryLevel, State) ->
    {Mood, LastPlaySeq, GestureCount, LastPlayTime} = la_machine_state:get_play_info(State),
    if
        Mood == waiting ->
            play_poke;
        true ->
            SecondsElapsed = erlang:system_time(second) - LastPlayTime,
            {play, timer, Mood, SecondsElapsed, LastPlaySeq, GestureCount}
    end;
% catch all
action(_WakeupCause, _ButtonState, _AccelerometerState, _BatteryLevel, _State) ->
    reset.

%% Detect a double click by staying awake and polling the button.
%% A double click is: from the current state, wait for the opposite transition,
%% then wait for it to return to `on`.
-define(DOUBLE_CLICK_TIMEOUT_MS, 700).
-define(DOUBLE_CLICK_POLL_MS, 5).

detect_double_click(on) ->
    Deadline = erlang:system_time(millisecond) + ?DOUBLE_CLICK_TIMEOUT_MS,
    detect_double_click_wait_for(1, Deadline);
detect_double_click(off) ->
    Deadline = erlang:system_time(millisecond) + ?DOUBLE_CLICK_TIMEOUT_MS,
    detect_double_click_wait_for(0, Deadline).

detect_double_click_wait_for(Seq, Deadline) ->
    case erlang:system_time(millisecond) > Deadline of
        true ->
            false;
        false ->
            case {Seq, read_button()} of
                {0, on} ->
                    % Pressed, now wait for first release
                    detect_double_click_wait_for(1, Deadline);
                {1, off} ->
                    % Released, now wait for second press
                    detect_double_click_wait_for(2, Deadline);
                {2, on} ->
                    % Second press detected
                    true;
                _ ->
                    timer:sleep(?DOUBLE_CLICK_POLL_MS),
                    detect_double_click_wait_for(Seq, Deadline)
            end
    end.

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
% player plays while calling -> calm
change_moodp(calling, player, _GestureCount, _Total_Gesture_Count, _SecondsElapsed, _LastPlaySeq) ->
    Mood = calm,
    io:format("Button while calling => ~p\n", [Mood]),
    {Mood, 0, undefined};
% player plays while waiting -> joy
change_moodp(waiting, player, _GestureCount, _Total_Gesture_Count, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Button while waiting : Long time no see => joy\n"),
    {joy, 0, undefined};
% joy : mood change ?
change_moodp(joy, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    Mood = joy,
    % if more than MOOD_MIN_GESTURES gestures, one chance out of ?JOY_CALM_CHANCE to go to calm
    if
        GestureCount > ?MOOD_MIN_GESTURES ->
            <<RandChange:56>> = crypto:strong_rand_bytes(7),
            if
                (RandChange rem ?JOY_CALM_CHANCE) == 0 ->
                    NewMood = calm,
                    io:format("Change mood ~s => ~s\n", [Mood, NewMood]),
                    {NewMood, 0, undefined};
                true ->
                    {Mood, GestureCount, LastPlaySeq}
            end;
        true ->
            {Mood, GestureCount, LastPlaySeq}
    end;
% calm : mood change ?
change_moodp(calm, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    Mood = calm,
    if
        GestureCount =< ?MOOD_MIN_GESTURES ->
            {Mood, GestureCount, LastPlaySeq};
        true ->
            PossibleMoods =
                add_moods_to_list(
                    excited,
                    ?CALM_EXCITED_PROBA,
                    add_moods_to_list(
                        tired,
                        ?CALM_TIRED_PROBA,
                        add_moods_to_list(
                            upset,
                            ?CALM_UPSET_PROBA,
                            add_moods_to_list(
                                dialectic,
                                ?CALM_DIAL_PROBA,
                                add_moods_to_list(
                                    imitation,
                                    ?CALM_IMIT_PROBA,
                                    add_moods_to_list(Mood, ?CALM_CALM_PROBA, [])
                                )
                            )
                        )
                    )
                ),
            <<RandChange:56>> = crypto:strong_rand_bytes(7),
            NewMood = lists:nth(1 + RandChange rem length(PossibleMoods), PossibleMoods),
            if
                NewMood =:= Mood ->
                    {Mood, GestureCount, LastPlaySeq};
                true ->
                    io:format("Change mood ~s => ~s\n", [Mood, NewMood]),
                    {NewMood, 0, undefined}
            end
    end;
% all other moods (than calling, wait, joy, calm)
change_moodp(Mood, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    if
        GestureCount > ?MOOD_MIN_GESTURES ->
            <<RandChange:56>> = crypto:strong_rand_bytes(7),
            if
                (RandChange rem ?MOODY_CALM_CHANCE) == 0 ->
                    NewMood = calm,
                    io:format("Change mood ~s => ~s\n", [Mood, NewMood]),
                    {NewMood, 0, undefined};
                true ->
                    {Mood, GestureCount, LastPlaySeq}
            end;
        true ->
            {Mood, GestureCount, LastPlaySeq}
    end.

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
% all others : joy, calm, tired, upset, excited
play_mood(Mood, _ElapsedSeconds, LastPlaySeq, Config) ->
    io:format("playing mood : ~s\n", [Mood]),
    MoodScenar = Mood,
    play_random_scenario_with_hit(MoodScenar, LastPlaySeq, Config).

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
%% play_battery_low
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_battery_low(la_machine_configuration:config()) -> pos_integer().
play_battery_low(Config) ->
    play_scenario_with_hit(system, ?BATTERY_LOW_SYSTEM_SCENARIO, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_system_sound
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_system_sound(binary(), la_machine_configuration:config()) -> ok.
play_system_sound(SoundFile, Config) ->
    {ok, Pid} = la_machine_player:start_link(Config),
    ok = la_machine_player:play(Pid, [{mp3, SoundFile}, {wait, sound}]),
    ok = la_machine_player:stop(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_welcome
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_welcome(la_machine_configuration:config()) -> pos_integer().
play_welcome(Config) ->
    play_scenario_with_hit(system, ?WELCOME_SYSTEM_SCENARIO, Config).

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
    % play random scenario
    ScenarioIx = random_num_upto_butnot(ScenarioCount, LastPlaySeq),
    io:format("play_random_scenario MoodScenar=~p ScenarioCount=~p ScenarioIx=~p\n", [
        MoodScenar, ScenarioCount, ScenarioIx
    ]),
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
    % play random scenario
    ScenarioIx = random_num_upto_butnot(ScenarioCount, LastPlaySeq),
    io:format("play_random_scenario_with_hit MoodScenar=~p ScenarioCount=~p ScenarioIx=~p\n", [
        MoodScenar, ScenarioCount, ScenarioIx
    ]),
    play_scenario_with_hit(MoodScenar, ScenarioIx, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_scenario : play scenario ScenarioIx of MoodScenar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_scenario(atom(), pos_integer(), la_machine_configuration:config()) -> pos_integer().
play_scenario(MoodScenar, ScenarioIx, Config) ->
    Scenario0 = la_machine_scenarios:get(MoodScenar, ScenarioIx),
    ScenarioWithClose =
        case Scenario0 of
            [] ->
                [{servo, 0}];
            _ ->
                case lists:last(Scenario0) of
                    {servo, 0} -> Scenario0;
                    {servo, 0, _Timeout} -> Scenario0;
                    _ -> Scenario0 ++ [{servo, 0}]
                end
        end,
    {ok, Pid} = la_machine_player:start_link(Config),
    ok = la_machine_player:play(Pid, ScenarioWithClose),
    ok = la_machine_player:stop(Pid),
    ScenarioIx.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_scenario_with_hit : play scenario ScenarioIx of MoodScenar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec play_scenario_with_hit(atom(), pos_integer(), la_machine_configuration:config()) ->
    pos_integer().
play_scenario_with_hit(MoodScenar, ScenarioIx, Config) ->
    Scenario = la_machine_scenarios:get(MoodScenar, ScenarioIx),
    {ok, Pid} = la_machine_player:start_link(Config),
    ScenarioStart = erlang:system_time(millisecond),
    ok = la_machine_player:play(Pid, Scenario),
    ScenarioEnd = erlang:system_time(millisecond),
    % play hit if needed
    ButtonState = read_button(),
    io:format("   after play ButtonState=~s\n", [ButtonState]),
    HitIx =
        if
            ButtonState == on ->
                play_random_hit(Pid);
            true ->
                ok = la_machine_player:play(Pid, [{servo, 0}]),
                none
        end,
    HitEnd = erlang:system_time(millisecond),
    ok = la_machine_player:stop(Pid),
    io:format("Played scenario=~p index=~p duration=~pms hit=~p hit_duration=~pms time=~p\n", [
        MoodScenar, ScenarioIx, ScenarioEnd - ScenarioStart,
        HitIx, HitEnd - ScenarioEnd, ScenarioEnd
    ]),
    ScenarioIx.

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

%% list util
-spec add_moods_to_list(atom(), non_neg_integer(), list()) -> list().
add_moods_to_list(_Mood, 0, AList) -> AList;
add_moods_to_list(Mood, N, AList) -> add_moods_to_list(Mood, N - 1, [Mood | AList]).

-ifdef(TEST).
action_test_() ->
    [
        ?_assertEqual(play_poke, action(sleep_wakeup_timer, off, ok, 100, la_machine_state:new())),
        ?_assertEqual(reset, action(sleep_wakeup_timer, off, ok, 20, la_machine_state:new())),
        ?_assertEqual(reset, action(undefined, off, ok, 100, la_machine_state:new())),
        ?_assertMatch(
            {play, player, waiting, _ElapsedSecs, 0, 0},
            action(sleep_wakeup_timer, on, ok, 100, la_machine_state:new())
        ),
        ?_assertMatch(
            play_battery_low,
            action(sleep_wakeup_timer, on, ok, 10, la_machine_state:new())
        ),
        ?_assertMatch(
            {play, player, waiting, _ElapsedSecs, 0, 0},
            action(sleep_wakeup_gpio, on, ok, 100, la_machine_state:new())
        ),
        ?_assertMatch(
            {play, player, waiting, _ElapsedSecs, 0, 0},
            action(undefined, on, ok, 100, la_machine_state:new())
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
