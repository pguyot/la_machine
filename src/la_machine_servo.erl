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
%% @doc La Machine servo interface
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine_servo).

-include("la_machine_definitions.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    power_on/1,
    power_on/2,
    power_off/0,
    reset/1,
    set_duty/2,
    set_target/2,
    set_target/3,
    timeout/1,
    stop_fade/1
]).

-export_type([
    state/0
]).

-type duty() :: non_neg_integer().

-record(state, {
    pre_min :: duty(),
    pre_max :: duty(),
    target = undefined :: undefined | duty(),
    config :: la_machine_configuration:config(),
    current_fade = undefined :: undefined | hardware | pid()
}).

-opaque state() :: #state{}.

-spec power_on(la_machine_configuration:config()) -> state().
power_on(Config) ->
    power_on(Config, 0).

-spec power_on(la_machine_configuration:config(), 0..100) -> state().
power_on(Config, InitialTarget) ->
    Duty = target_to_duty(InitialTarget, Config),
    LEDCTimer = [
        {duty_resolution, ?LEDC_DUTY_RESOLUTION},
        {freq_hz, ?SERVO_FREQ_HZ},
        {speed_mode, ?LEDC_MODE},
        {timer_num, ?LEDC_TIMER}
    ],
    ok = ledc:timer_config(LEDCTimer),
    LEDCChannel = [
        {channel, ?LEDC_CHANNEL},
        {duty, Duty},
        {gpio_num, ?LEDC_CH_GPIO},
        {speed_mode, ?LEDC_MODE},
        {hpoint, 0},
        {timer_sel, ?LEDC_TIMER}
    ],
    ok = ledc:channel_config(LEDCChannel),

    power_on_boost(),

    ok = ledc:fade_func_install(0),

    #state{
        pre_min = Duty,
        pre_max = Duty,
        config = Config
    }.

-spec power_off() -> ok.
power_off() ->
    ok = ledc:stop(?LEDC_MODE, ?LEDC_CHANNEL, 0),
    ok = ledc:fade_func_uninstall(),
    % Turn down servo
    power_off_boost(),
    ok.

-spec power_on_boost() -> ok.
power_on_boost() ->
    ok = gpio:init(?SERVO_EN_BOOST_GPIO),
    ok = gpio:set_pin_mode(?SERVO_EN_BOOST_GPIO, output),
    ok = gpio:digital_write(?SERVO_EN_BOOST_GPIO, 1).

-spec power_off_boost() -> ok.
power_off_boost() ->
    ok = gpio:digital_write(?SERVO_EN_BOOST_GPIO, 0).

-spec reset(la_machine_configuration:config()) -> ok.
reset(Config) ->
    State0 = power_on(Config),
    {Sleep, _State1} = set_target(0, State0),
    timer:sleep(Sleep),
    power_off().

%% -----------------------------------------------------------------------------
%% @param Duty duty value for the servo
%% @param State current state
%% @return a tuple with the time until the end of the move and the new state
%% @doc Set the duty and move the servo as fast as possible. Used for
%% calibration purposes.
%% Fails with function_clause if a fade is in progress.
%% @end
%% -----------------------------------------------------------------------------
-spec set_duty(Duty :: non_neg_integer(), State :: state()) -> {non_neg_integer(), state()}.
set_duty(Duty, #state{current_fade = undefined} = State) ->
    ok = ledc:set_duty(?LEDC_MODE, ?LEDC_CHANNEL, Duty),
    ok = ledc:update_duty(?LEDC_MODE, ?LEDC_CHANNEL),
    target_duty_timeout(Duty, 0, State).

%% -----------------------------------------------------------------------------
%% @param Target target for the servo (from 0 to 100)
%% @param State current state
%% @return a tuple with the time until the end of the move and the new state
%% @doc Set the target and move the servo as fast as possible.
%% Fails with function_clause if a fade is in progress.
%% @end
%% -----------------------------------------------------------------------------
-spec set_target(Target :: number(), State :: state()) -> {non_neg_integer(), state()}.
set_target(Target, #state{config = Config, current_fade = undefined} = State) ->
    Duty = target_to_duty(Target, Config),
    ok = ledc:set_duty(?LEDC_MODE, ?LEDC_CHANNEL, Duty),
    ok = ledc:update_duty(?LEDC_MODE, ?LEDC_CHANNEL),
    target_duty_timeout(Duty, 0, State).

%% -----------------------------------------------------------------------------
%% @param Target target for the servo (from 0 to 100)
%% @param TimeMS time to reach the target, must be greater than the time
%% actually required to move the servo
%% @param State current state
%% @return a tuple with the time until the end of the move and the new state
%% @doc Set the target but slowly fade towards duty.
%% Fails with function_clause if a fade is in progress.
%% @end
%% -----------------------------------------------------------------------------
-spec set_target(Target :: number(), TimeMS :: pos_integer(), State :: state()) ->
    {non_neg_integer(), state()}.
set_target(
    NewTarget,
    TimeMS,
    #state{config = Config, pre_min = PreMin, current_fade = undefined, target = OldTarget} = State
) ->
    % If previous movement was canceled, compute the fade from the original target even if
    % we didn't reach it.
    CurrentDuty =
        case OldTarget of
            undefined -> PreMin;
            _ -> OldTarget
        end,
    Duty = target_to_duty(NewTarget, Config),
    {FadePid, ActualTimeMS} = start_fade(CurrentDuty, Duty, TimeMS),
    target_duty_timeout(Duty, ActualTimeMS, State#state{current_fade = FadePid, target = Duty}).

-spec target_duty_timeout(non_neg_integer(), non_neg_integer(), state()) ->
    {non_neg_integer(), state()}.
target_duty_timeout(Duty, MinTimeMS, #state{pre_min = PreMin, pre_max = PreMax} = State) ->
    MaxTime = ceil(
        max(abs(Duty - PreMin), abs(Duty - PreMax)) * ?SERVO_FULL_RANGE_TIME_MS / ?SERVO_MAX_DUTY *
            ?SERVO_FREQ_PERIOD_US / (?SERVO_MAX_WIDTH_US - ?SERVO_MIN_WIDTH_US)
    ),
    {max(MaxTime, MinTimeMS), State#state{
        pre_min = min(Duty, PreMin), pre_max = max(Duty, PreMax), target = Duty
    }}.

-spec target_to_duty(number(), la_machine_configuration:config()) -> non_neg_integer().
target_to_duty(Target, Config) ->
    InterruptDuty = la_machine_configuration:interrupt_duty(Config),
    ClosedDuty = la_machine_configuration:closed_duty(Config),
    ClosedDuty + Target * (InterruptDuty - ClosedDuty) div 100.

-spec compute_fade_params(non_neg_integer(), non_neg_integer()) ->
    {hardware, pos_integer(), pos_integer()} | software.
compute_fade_params(0, _TotalCycles) ->
    {hardware, 1, 1};
compute_fade_params(DutyDelta, 0) ->
    {hardware, max(1, DutyDelta), 1};
compute_fade_params(DutyDelta, TotalCycles) when DutyDelta >= TotalCycles ->
    ScaleFloor = DutyDelta div TotalCycles,
    ScaleCeil = ScaleFloor + 1,
    StepsFloor = (DutyDelta + ScaleFloor - 1) div ScaleFloor,
    StepsCeil = (DutyDelta + ScaleCeil - 1) div ScaleCeil,
    ErrFloor = abs(StepsFloor - TotalCycles),
    ErrCeil = abs(StepsCeil - TotalCycles),
    {BestScale, BestSteps} =
        case ErrFloor =< ErrCeil of
            true -> {ScaleFloor, StepsFloor};
            false -> {ScaleCeil, StepsCeil}
        end,
    case BestSteps * 10 > TotalCycles * 11 orelse BestSteps * 10 < TotalCycles * 9 of
        true -> software;
        false -> {hardware, BestScale, 1}
    end;
compute_fade_params(DutyDelta, TotalCycles) ->
    CycleFloor = TotalCycles div DutyDelta,
    CycleCeil = CycleFloor + 1,
    TotalFloor = DutyDelta * CycleFloor,
    TotalCeil = DutyDelta * CycleCeil,
    ErrFloor = abs(TotalFloor - TotalCycles),
    ErrCeil = abs(TotalCeil - TotalCycles),
    {BestCycle, BestTotal} =
        case ErrFloor =< ErrCeil of
            true -> {CycleFloor, TotalFloor};
            false -> {CycleCeil, TotalCeil}
        end,
    case BestTotal * 10 > TotalCycles * 11 orelse BestTotal * 10 < TotalCycles * 9 of
        true -> software;
        false -> {hardware, 1, BestCycle}
    end.

-spec start_fade(duty(), duty(), pos_integer()) ->
    {hardware | pid(), non_neg_integer()}.
start_fade(CurrentDuty, TargetDuty, TimeMS) ->
    DutyDelta = abs(TargetDuty - CurrentDuty),
    TotalCycles = TimeMS * ?SERVO_FREQ_HZ div 1000,
    case compute_fade_params(DutyDelta, TotalCycles) of
        {hardware, Scale, CycleNum} ->
            ok = ledc:set_fade_step_and_start(
                ?LEDC_MODE, ?LEDC_CHANNEL, TargetDuty, Scale, CycleNum, ?LEDC_FADE_NO_WAIT
            ),
            Steps = (DutyDelta + Scale - 1) div Scale,
            ActualTimeMS = Steps * CycleNum * 1000 div ?SERVO_FREQ_HZ,
            {hardware, ActualTimeMS};
        software ->
            PeriodMS = 1000 div ?SERVO_FREQ_HZ,
            Direction =
                case TargetDuty >= CurrentDuty of
                    true -> 1;
                    false -> -1
                end,
            Pid = spawn_link(
                fun() ->
                    software_fade_loop(1, TotalCycles, CurrentDuty, DutyDelta, Direction, PeriodMS)
                end
            ),
            {Pid, TotalCycles * PeriodMS}
    end.

-spec software_fade_loop(
    pos_integer(), pos_integer(), non_neg_integer(), non_neg_integer(), -1 | 1, pos_integer()
) -> ok.
software_fade_loop(Step, TotalCycles, _CurrentDuty, _DutyDelta, _Direction, _PeriodMS) when
    Step > TotalCycles
->
    ok;
software_fade_loop(Step, TotalCycles, CurrentDuty, DutyDelta, Direction, PeriodMS) ->
    timer:sleep(PeriodMS),
    Duty = CurrentDuty + Direction * (Step * DutyDelta div TotalCycles),
    ok = ledc:set_duty_and_update(?LEDC_MODE, ?LEDC_CHANNEL, Duty, 0),
    software_fade_loop(Step + 1, TotalCycles, CurrentDuty, DutyDelta, Direction, PeriodMS).

-spec timeout(State :: state()) -> state().
timeout(State0) ->
    State1 = stop_fade(State0),
    reset_target(State1).

-spec stop_fade(State :: state()) -> state().
stop_fade(#state{current_fade = undefined} = State) ->
    State;
stop_fade(#state{current_fade = hardware} = State) ->
    ok = ledc:fade_stop(?LEDC_MODE, ?LEDC_CHANNEL),
    State#state{current_fade = undefined};
stop_fade(#state{current_fade = Pid} = State) when is_pid(Pid) ->
    MonRef = erlang:monitor(process, Pid),
    unlink(Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MonRef, process, Pid, _} -> ok
    end,
    State#state{current_fade = undefined}.

-spec reset_target(state()) -> state().
reset_target(#state{target = Target} = State) ->
    State#state{pre_min = Target, pre_max = Target, target = undefined}.

-ifdef(TEST).
% Values depend on ?DEFAULT_SERVO_INTERRUPT_DUTY and ?DEFAULT_SERVO_CLOSED_DUTY
-if(?HARDWARE_REVISION =:= proto_20241023).
target_to_duty_test_() ->
    [
        ?_assertEqual(864, target_to_duty(0, la_machine_configuration:default())),
        ?_assertEqual(1911, target_to_duty(100, la_machine_configuration:default()))
    ].
-elif(?HARDWARE_REVISION =:= proto_20260106).
target_to_duty_test_() ->
    [
        ?_assertEqual(1547, target_to_duty(0, la_machine_configuration:default())),
        ?_assertEqual(682, target_to_duty(100, la_machine_configuration:default()))
    ].
-else.
-error({unsupported_hardware_revision, ?HARDWARE_REVISION}).
-endif.

target_duty_timeout_test_() ->
    {
        setup,
        fun() ->
            la_machine_configuration:default()
        end,
        fun(_) ->
            ok
        end,
        fun(Config) ->
            [
                ?_assertMatch(
                    {0, #state{
                        pre_min = 318,
                        pre_max = 318,
                        target = 318,
                        config = Config
                    }},
                    target_duty_timeout(318, 0, #state{
                        pre_min = 318, pre_max = 318, config = Config
                    })
                ),
                ?_assertMatch(
                    {469, #state{
                        pre_min = 318,
                        pre_max = 887,
                        target = 887,
                        config = Config
                    }},
                    target_duty_timeout(887, 0, #state{
                        pre_min = 318, pre_max = 318, config = Config
                    })
                ),
                ?_assertMatch(
                    {600, #state{
                        pre_min = 318,
                        pre_max = 887,
                        target = 887,
                        config = Config
                    }},
                    target_duty_timeout(887, 600, #state{
                        pre_min = 318, pre_max = 318, config = Config
                    })
                ),
                ?_assertMatch(
                    {469, #state{
                        pre_min = 318,
                        pre_max = 887,
                        target = 318,
                        config = Config
                    }},
                    target_duty_timeout(318, 0, #state{
                        pre_min = 887, pre_max = 887, config = Config
                    })
                )
            ]
        end
    }.

% A full servo range movement should take SERVO_FULL_RANGE_TIME_MS
target_duty_timeout_full_range_test_() ->
    {
        setup,
        fun() ->
            la_machine_configuration:default()
        end,
        fun(_) ->
            ok
        end,
        fun(Config) ->
            %% Duty values corresponding to the servo's physical range limits
            %% (500µs and 2500µs pulse widths)
            Duty0 = 409,
            Duty180 = 2047,
            [
                ?_assertMatch(
                    {?SERVO_FULL_RANGE_TIME_MS, _},
                    target_duty_timeout(Duty180, 0, #state{
                        pre_min = Duty0, pre_max = Duty0, config = Config
                    })
                ),
                ?_assertMatch(
                    {?SERVO_FULL_RANGE_TIME_MS, _},
                    target_duty_timeout(Duty0, 0, #state{
                        pre_min = Duty180, pre_max = Duty180, config = Config
                    })
                )
            ]
        end
    }.

reset_target_test_() ->
    {
        setup,
        fun() ->
            la_machine_configuration:default()
        end,
        fun(_) ->
            ok
        end,
        fun(Config) ->
            [
                ?_assertMatch(
                    #state{
                        pre_min = 887,
                        pre_max = 887,
                        target = undefined,
                        config = Config
                    },
                    reset_target(#state{pre_min = 318, pre_max = 318, target = 887, config = Config})
                ),
                ?_assertMatch(
                    #state{
                        pre_min = 500,
                        pre_max = 500,
                        target = undefined,
                        config = Config
                    },
                    reset_target(#state{pre_min = 400, pre_max = 600, target = 500, config = Config})
                )
            ]
        end
    }.

compute_fade_params_test_() ->
    [
        % Analysis example: ratio between 1 and 2, both options exceed 10%
        ?_assertEqual(software, compute_fade_params(367, 218)),
        % Exact ratio: 218/218 = 1, Scale=1 gives exactly 218 steps
        ?_assertEqual({hardware, 1, 1}, compute_fade_params(218, 218)),
        % DutyDelta < TotalCycles: CycleFloor=2, total=200, error=8.3%
        ?_assertEqual({hardware, 1, 2}, compute_fade_params(100, 218)),
        % High scale: ScaleCeil=9, steps=48, error=4%
        ?_assertEqual({hardware, 9, 1}, compute_fade_params(432, 50)),
        % DutyDelta=0: no-op
        ?_assertEqual({hardware, 1, 1}, compute_fade_params(0, 218)),
        % TotalCycles=0: immediate
        ?_assertEqual({hardware, 432, 1}, compute_fade_params(432, 0)),
        % Both zero
        ?_assertEqual({hardware, 1, 1}, compute_fade_params(0, 0))
    ].
-endif.
