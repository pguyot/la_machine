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
    | play_wait_5000
    | {
        play_scenario,
        Mood :: atom(),
        Index :: non_neg_integer()
    }    
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

    io:format("Waking up : WakeupCause=~s ButtonState=~s AccelerometerState=~s\n", [WakeupCause, ButtonState, AccelerometerState]),

    %% process click
    StateX = do_process_click(WakeupCause, ButtonState, State0),
    IsPausedNow = la_machine_state:get_is_paused(StateX),

    State1 =
        case compute_action(IsPausedNow, WakeupCause, ButtonState, AccelerometerState, StateX) of
            {play, meuh} ->
                play_meuh(),
                StateX;

            % only used when DEBUG_PLAY_ONLY_ONE_MOOD
            {play_scenario, Mood, Index} ->
                if
                    Index < 0 ->
                        play_random_scenario_with_hit(Mood, undefined);
                    true ->
                        play_scenario(Mood, Index),
                        play_hit_if_needed()
                end,
                StateX;

            % normal play
            {play, Reason, Mood, SecondsElapsed, LastPlaySeq, GestureCount} ->
                io:format("Play Reason=~s Mood=~s GestureCount=~p LastPlaySeq=~p SecondsElapsed=~p\n", [Reason, Mood, GestureCount, LastPlaySeq, SecondsElapsed]),

                % change mood ?
                Total_Gesture_Count = la_machine_state:get_total_gestures_count(StateX),
                {Mood1, GestureCount1, LastPlaySeq1} = change_moodp(Mood, Reason, GestureCount, Total_Gesture_Count, SecondsElapsed, LastPlaySeq),

                if
                    Mood1 == waiting ->
                        % waiting : don't play anything
                        % Only change the mood
                        la_machine_state:set_mood_waiting(StateX);
                    true ->
                        % else : play and remember
                        PlayedSeq = play_mood(Mood1, SecondsElapsed, LastPlaySeq1),
                        la_machine_state:append_play(Mood1, GestureCount1 + 1, PlayedSeq, StateX)
                end;

            reset ->
                io:format("resetting\n"),
                la_machine_audio:reset(),
                la_machine_servo:reset(),
                StateX;

            play_wait_5000 ->
                play_wait_5000(),
                la_machine_audio:reset(),
                la_machine_servo:reset(),
                StateX
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
%% do_process_click(WakeupCause, ButtonState, State)
%% process_click(WakeupCause, ButtonState, DurMs, ClickCnt, IsPaused, State)
%% BROKEN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-if(?TRIPLECLICK == 1).
do_process_click(WakeupCause, ButtonState, State0) ->
    DurMS = la_machine_state:get_ms_since_last_on(State0),
    ClickCnt = la_machine_state:get_click_count(State0),
    IsPaused = la_machine_state:get_is_paused(State0),
    process_click(WakeupCause, ButtonState, DurMS, ClickCnt, IsPaused, State0).

process_click(sleep_wakeup_gpio, on, _DurMs, _ClickCnt, _IsPaused, State) ->
    % on : remember time
    la_machine_state:set_last_on_now(State);

process_click(sleep_wakeup_gpio, off, DurMs, ClickCnt, IsPaused, State) 
    when DurMs =< 1000 andalso ClickCnt == 2 ->
    % off : triple click -> inverse paused
    NewPaused = 1 - IsPaused,
    io:format("NewPaused=~p\n", [NewPaused]),
    la_machine_state:set_is_paused(NewPaused, State);

process_click(sleep_wakeup_gpio, off, DurMs, ClickCnt, _IsPaused, State) 
    when DurMs =< 1000 -> % ClickCnt =< 2
    % off : click, and < three => increase count
    la_machine_state:set_click_count(ClickCnt + 1, State);

process_click(sleep_wakeup_gpio, off, _DurMs, _ClickCnt, _IsPaused, State) ->
    % DurMs > 1000
    % off : no click => set count to 0
    la_machine_state:set_click_count(0, State);

% all other cases
process_click(_, _, _, _, _, State) -> State.

-else.

do_process_click(_, _, State) -> State.

-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec compute_action(
    non_neg_integer(),
    esp:esp_wakeup_cause(),
    on | off,
    ok | {play, meuh} | not_resting | replaced,
    la_machine_state:state()
) -> action().

-if(?DEBUG_PLAY_ONLY_ONE_MOOD == 1).
    % DEBUG 
compute_action(0, _WakeupCause, on, ok, _State) ->
    io:format("play one mood : ~p, Index =~p\n", [?DEBUG_PLAY_ONLY_ONE_MOOD_MOOD, ?DEBUG_PLAY_ONLY_ONE_MOOD_INDEX]),
    {play_scenario, ?DEBUG_PLAY_ONLY_ONE_MOOD_MOOD, ?DEBUG_PLAY_ONLY_ONE_MOOD_INDEX};

compute_action(IsPausedNow, WakeupCause, ButtonState, AccelerometerState, State) ->
    action(IsPausedNow, WakeupCause, ButtonState, AccelerometerState, State).

-else.

compute_action(IsPausedNow, WakeupCause, ButtonState, AccelerometerState, State) ->
    action(IsPausedNow, WakeupCause, ButtonState, AccelerometerState, State).
-endif.

% action
-spec action(
    non_neg_integer(),
    esp:esp_wakeup_cause(),
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
    {play, meuh};

% not resting
action(_IsPausedNow, _WakeupCause, _ButtonState, not_resting, _State) ->
    reset;

% replaced
action(_IsPausedNow, _WakeupCause, _ButtonState, replaced, State) ->
    {Mood, LastPlaySeq, GestureCount, LastPlayTime} = la_machine_state:get_play_info(State),
    SecondsElapsed = erlang:system_time(second) - LastPlayTime,
    {play, replaced, Mood, SecondsElapsed, LastPlaySeq, GestureCount};

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
    SecondsElapsed = erlang:system_time(second) - LastPlayTime,
    {play, timer, Mood, SecondsElapsed, LastPlaySeq, GestureCount};

% catch all
action(_IsPausedNow, undefined, _ButtonState, _AccelerometerState, _State) ->
    reset.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% check mood change
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec change_moodp(Mood :: atom(), Reason :: atom(), GestureCount :: non_neg_integer(), Total_Gesture_Count :: non_neg_integer(), SecondsElapsed :: non_neg_integer(), LastPlaySeq :: non_neg_integer() | undefined) -> {
        NewMood :: atom(),
        NewGestureCount :: non_neg_integer(),
        NewLastPlaySeq :: non_neg_integer() | undefined
    }.

% timer while waiting -> start poke
change_moodp(waiting, timer, _GestureCount, _Total_Gesture_Count, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Wakeup timer while waiting : poke\n"),
    {poke, 0, undefined};
    
% replaced while waiting -> start poke
change_moodp(waiting, replaced, _GestureCount, _Total_Gesture_Count, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Wakeup replaced while waiting : poke\n"),
    {poke, 0, undefined};

% timer while poke -> continue poke or start waiting
change_moodp(poke, timer, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    io:format("Wakeup timer while poke\n"),
    if
        GestureCount >= ?MAX_CALLING_SOUNDS ->
            io:format("   max times => waiting\n"),
            {waiting, 0, undefined};
        true -> 
            io:format("   continue calling\n"),
            {poke, GestureCount, LastPlaySeq}
    end;

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
            io:format("   max times (TotalGesture=~p => MaxTimes=~p)=> waiting\n", [Total_Gesture_Count, Max_Calling_Times]),
            {waiting, 0, undefined};
        true -> 
            io:format("   continue calling\n"),
            {calling, GestureCount, LastPlaySeq}
    end;

% timer while all other moods -> start calling
change_moodp(Mood, timer, _GestureCount, _Total_Gesture_Count, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Wakeup while in ~s => calling\n", [Mood]),
    {calling, 0, undefined};

% player plays while calling -> continue game imitation
change_moodp(calling, player, _GestureCount, _Total_Gesture_Count, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Long time no see : joy\n"),
    {imitation, 0, undefined};

% player plays while waiting -> joy
change_moodp(waiting, player, _GestureCount, _Total_Gesture_Count, _SecondsElapsed, _LastPlaySeq) ->
    io:format("Long time no see : joy\n"),
    {joy, 0, undefined};

% joy : mood change ?
change_moodp(joy, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
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

% imitation : mood change ?
change_moodp(imitation, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    Mood = imitation,
    <<RandChangeDial>> = crypto:strong_rand_bytes(1),
    <<RandChangeUpset>> = crypto:strong_rand_bytes(1),
    <<RandChangeTired>> = crypto:strong_rand_bytes(1),
    <<RandChangeExcited>> = crypto:strong_rand_bytes(1),
    if
        GestureCount > ?IMIT_MIN_GESTURES ->
            if
                (RandChangeDial rem ?IMIT_DIAL_CHANCE) == 0 -> 
                    TmpMood = dialectics,
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
                true -> {Mood, GestureCount, LastPlaySeq}
            end;
        true -> {Mood, GestureCount, LastPlaySeq}
    end;

% dialectics : mood change ?
change_moodp(dialectics, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    Mood = dialectics,
    <<RandChangeImit>> = crypto:strong_rand_bytes(1),
    <<RandChangeUpset>> = crypto:strong_rand_bytes(1),
    <<RandChangeTired>> = crypto:strong_rand_bytes(1),
    <<RandChangeExcited>> = crypto:strong_rand_bytes(1),
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
                true -> {Mood, GestureCount, LastPlaySeq}
            end;
        true -> {Mood, GestureCount, LastPlaySeq}
    end;

% upset, tired, excited : mood change ?
change_moodp(Mood, player, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) 
    when Mood == upset orelse Mood == tired orelse Mood == excited
    ->
    <<RandChange>> = crypto:strong_rand_bytes(1),
    if
        GestureCount > ?DIAL_MIN_GESTURES ->
            if
                (RandChange rem ?MOODY_IMIT_CHANCE) == 0 -> 
                    TmpMood = imitation,
                    io:format("Change mood : ~s\n", [TmpMood]),
                    {TmpMood, 0, undefined};
                true -> {Mood, GestureCount, LastPlaySeq}
            end;
        true -> {Mood, GestureCount, LastPlaySeq}
    end;

% catch all : no change
change_moodp(Mood, _Reason, GestureCount, _Total_Gesture_Count, _SecondsElapsed, LastPlaySeq) ->
    {Mood, GestureCount, LastPlaySeq}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play_mood(imitation, ElapsedSeconds, LastPlaySeq) ->
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
    NewPlayedSeq = play_random_scenario_with_hit(MoodScenar, LastPlaySeq),
    NewPlayedSeq;

play_mood(dialectics, ElapsedSeconds, LastPlaySeq) ->
    Mood = dialectics,
    io:format("playing mood : ~s ElapsedSeconds=~p\n", [Mood, ElapsedSeconds]),
    % play inverse
    MoodScenar =
        if
            ElapsedSeconds =< ?GAME_MEDIUM_DUR_S ->
                game_long;
            true -> game_short
        end,
    NewPlayedSeq = play_random_scenario_with_hit(MoodScenar, LastPlaySeq),
    play_random_hit(),
    NewPlayedSeq;

% calling, poke (no hit)
play_mood(Mood, _ElapsedSeconds, LastPlaySeq)
    when Mood == calling orelse Mood == poke
    ->
    io:format("playing mood : ~s\n", [Mood]),
    MoodScenar = Mood,
    play_random_scenario(MoodScenar, LastPlaySeq);

% all others : joy, tired, upset, excited
play_mood(Mood, _ElapsedSeconds, LastPlaySeq) ->
    io:format("playing mood : ~s\n", [Mood]),
    MoodScenar = Mood,
    NewPlayedSeq = play_random_scenario_with_hit(MoodScenar, LastPlaySeq),
    play_random_hit(),
    NewPlayedSeq.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_wait_5000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play_wait_5000() ->
    io:format("play_wait_5000\n"),
    ScenarioPart = [{wait, 5000}],
    {ok, Pid} = la_machine_player:start_link(),
    ok = la_machine_player:play(Pid, ScenarioPart),
    ok = la_machine_player:stop(Pid),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_meuh
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play_meuh() ->
    play_random_scenario(meuh, undefined).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_random_hit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play_random_hit() ->
    MoodScenar = hit,
    ScenarioCount = la_machine_scenarios:count(MoodScenar),
    ScenarioIx = random_num_upto_butnot(ScenarioCount, undefined),
    io:format("play_random_hit ScenarioIx=~p\n", [ScenarioIx]),
    Scenario = la_machine_scenarios:get(MoodScenar, ScenarioIx),
    % could be adapted to lenth of hit
    Scenario_Full = Scenario ++ [{servo, 100}, {wait, 100}, {servo, 0}],
    {ok, Pid} = la_machine_player:start_link(),
    ok = la_machine_player:play(Pid, Scenario_Full),
    ok = la_machine_player:stop(Pid),
    ScenarioIx.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_hit_if_needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play_hit_if_needed() ->
    ButtonState = read_button(),
    io:format("   after play ButtonState=~s\n", [ButtonState]),
    if
        ButtonState == on -> play_random_hit();
        true -> 1
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_random_scenario : play random scenario of MoodScenar, but not LastPlaySeq if possible
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play_random_scenario(MoodScenar, LastPlaySeq) ->
    ScenarioCount = la_machine_scenarios:count(MoodScenar),
    io:format("play_random_scenario MoodScenar=~p ScenarioCount=~p\n", [MoodScenar, ScenarioCount]),

    % play random scenario
    ScenarioIx = random_num_upto_butnot(ScenarioCount, LastPlaySeq),
    io:format("ScenarioIx=~p\n", [ScenarioIx]),
    play_scenario(MoodScenar, ScenarioIx).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_random_scenario_with_hit : play_random_scenario + play hit if needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play_random_scenario_with_hit(MoodScenar, LastPlaySeq) ->
    ScenarioIx = play_random_scenario(MoodScenar, LastPlaySeq),
    play_hit_if_needed(),
    ScenarioIx.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% play_scenario : play scenario ScenarioIx of MoodScenar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play_scenario(MoodScenar, ScenarioIx) ->
    Scenario = la_machine_scenarios:get(MoodScenar, ScenarioIx),
    {ok, Pid} = la_machine_player:start_link(),
    ok = la_machine_player:play(Pid, Scenario),
    ok = la_machine_player:stop(Pid),
    ScenarioIx.

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
        % if waiting => 24h
        waiting ->
            io:format("waiting : sleep 24h\n"),
            3600 * 24;

        % if calling => compute delay from GestureCount
        calling -> 
            % delay = random(CALLING_MIN_DELAY_S, CALLING_MAX_DELAY_S)
            GestureCount = la_machine_state:get_gestures_count(State),
            <<RandS:56>> = crypto:strong_rand_bytes(7),
            Delay = (?CALLING_MIN_DELAY_S + (RandS rem (?CALLING_MAX_DELAY_S - ?CALLING_MIN_DELAY_S))),
            io:format("calling : sleep ~ps (GestureCount=~p)\n", [Delay, GestureCount]),
            Delay;

        % if poke => compute delay from GestureCount
        poke -> 
            % delay = random(CALLING_MIN_DELAY_S, CALLING_MAX_DELAY_S)
            GestureCount = la_machine_state:get_gestures_count(State),
            <<RandS:56>> = crypto:strong_rand_bytes(7),
            Delay = (?CALLING_MIN_DELAY_S + (RandS rem (?CALLING_MAX_DELAY_S - ?CALLING_MIN_DELAY_S))),
            io:format("poke : sleep ~ps (GestureCount=~p)\n", [Delay, GestureCount]),
            Delay;

        % else : delay first calling
        _ ->
            Delay = ?CALLING_START_DELAY_S,
            io:format("in ~s : sleep default ~ps\n", [Mood, Delay]),
            Delay
    end.

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
