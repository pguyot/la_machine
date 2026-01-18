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
    power_off/0,
    reset/1,
    set_angle/2,
    set_target/2,
    set_target/3,
    timeout/1
]).

-export_type([
    state/0
]).

-record(state, {
    pre_min :: number(),
    pre_max :: number(),
    target = undefined :: undefined | number(),
    config :: la_machine_configuration:config()
}).

-opaque state() :: #state{}.

-spec power_on(la_machine_configuration:config()) -> state().
power_on(Config) ->
    LEDCTimer = [
        {duty_resolution, ?LEDC_DUTY_RESOLUTION},
        {freq_hz, ?SERVO_FREQ_HZ},
        {speed_mode, ?LEDC_MODE},
        {timer_num, ?LEDC_TIMER}
    ],
    ok = ledc:timer_config(LEDCTimer),
    LEDCChannel = [
        {channel, ?LEDC_CHANNEL},
        {duty, 0},
        {gpio_num, ?LEDC_CH_GPIO},
        {speed_mode, ?LEDC_MODE},
        {hpoint, 0},
        {timer_sel, ?LEDC_TIMER}
    ],
    ok = ledc:channel_config(LEDCChannel),

    power_on_boost(),

    ok = ledc:fade_func_install(0),

    Duty0 = target_to_duty(0, Config),
    Duty100 = target_to_duty(100, Config),

    #state{
        pre_min = min(Duty0, Duty100),
        pre_max = max(Duty0, Duty100),
        config = Config
    }.

-spec power_off() -> ok.
power_off() ->
    ok = ledc:fade_func_uninstall(),
    % Turn down servo
    power_off_boost(),
    ok.

power_on_boost() ->
    ok = gpio:init(?SERVO_EN_BOOST_GPIO),
    ok = gpio:set_pin_mode(?SERVO_EN_BOOST_GPIO, output),
    ok = gpio:digital_write(?SERVO_EN_BOOST_GPIO, 1).

power_off_boost() ->
    ok = gpio:digital_write(?SERVO_EN_BOOST_GPIO, 0).

-spec reset(la_machine_configuration:config()) -> ok.
reset(Config) ->
    State0 = power_on(Config),
    {Sleep, _State1} = set_target(0, State0),
    timer:sleep(Sleep),
    power_off().

%% -----------------------------------------------------------------------------
%% @param Angle angle for the servo (from 0 to 180)
%% @param State current state
%% @return a tuple with the time until the end of the move and the new state
%% @doc Set the angle and move the servo as fast as possible. Used for
%% calibration purposes.
%% @end
%% -----------------------------------------------------------------------------
-spec set_angle(Angle :: 0..180, State :: state()) -> {non_neg_integer(), state()}.
set_angle(Angle, State) ->
    Duty = angle_to_duty(Angle),
    ok = ledc:set_duty(?LEDC_MODE, ?LEDC_CHANNEL, Duty),
    ok = ledc:update_duty(?LEDC_MODE, ?LEDC_CHANNEL),
    target_duty_timeout(Duty, 0, State).

%% -----------------------------------------------------------------------------
%% @param Target target for the servo (from 0 to 100)
%% @param State current state
%% @return a tuple with the time until the end of the move and the new state
%% @doc Set the target and move the servo as fast as possible
%% @end
%% -----------------------------------------------------------------------------
-spec set_target(Target :: number(), State :: state()) -> {non_neg_integer(), state()}.
set_target(Target, #state{config = Config} = State) ->
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
%% @doc Set the target but slowly fade towards duty
%% @end
%% -----------------------------------------------------------------------------
-spec set_target(Target :: number(), TimeMS :: pos_integer(), State :: state()) ->
    {non_neg_integer(), state()}.
set_target(Target, TimeMS, #state{config = Config} = State) ->
    Duty = target_to_duty(Target, Config),
    ok = ledc:set_fade_with_time(?LEDC_MODE, ?LEDC_CHANNEL, Duty, TimeMS),
    ok = ledc:fade_start(?LEDC_MODE, ?LEDC_CHANNEL, ?LEDC_FADE_NO_WAIT),
    target_duty_timeout(Duty, TimeMS, State).

target_duty_timeout(Duty, MinTimeMS, #state{pre_min = PreMin, pre_max = PreMax} = State) ->
    MaxTime = ceil(
        max(abs(Duty - PreMin), abs(Duty - PreMax)) * ?SERVO_MAX_ANGLE_TIME_MS / ?SERVO_MAX_DUTY *
            ?SERVO_FREQ_PERIOD_US / ?SERVO_MAX_WIDTH_US
    ),
    {max(MaxTime, MinTimeMS), State#state{
        pre_min = min(Duty, PreMin), pre_max = max(Duty, PreMax), target = Duty
    }}.

target_to_duty(Target, Config) ->
    InterruptAngle = la_machine_configuration:interrupt_angle(Config),
    ClosedAngle = la_machine_configuration:closed_angle(Config),
    Angle = Target * (InterruptAngle - ClosedAngle) / 100 + ClosedAngle,
    angle_to_duty(Angle).

angle_to_duty(Angle) ->
    AngleUS =
        (Angle / ?SERVO_MAX_ANGLE) * (?SERVO_MAX_WIDTH_US - ?SERVO_MIN_WIDTH_US) +
            ?SERVO_MIN_WIDTH_US,
    Duty = AngleUS * ?SERVO_MAX_DUTY / ?SERVO_FREQ_PERIOD_US,
    floor(Duty).

-spec timeout(State :: state()) -> state().
timeout(State) ->
    ledc:stop(?LEDC_MODE, ?LEDC_CHANNEL, 0),
    reset_target(State).

reset_target(#state{target = Target} = State) ->
    State#state{pre_min = Target, pre_max = Target, target = undefined}.

-ifdef(TEST).
% Values depend on ?DEFAULT_SERVO_INTERRUPT_ANGLE and ?DEFAULT_SERVO_CLOSED_ANGLE
-if(?HARDWARE_REVISION =:= proto_20241023).
target_to_duty_test_() ->
    [
        ?_assertEqual(432, target_to_duty(0, la_machine_configuration:default())),
        ?_assertEqual(955, target_to_duty(100, la_machine_configuration:default()))
    ].
-elif(?HARDWARE_REVISION =:= proto_20260106).
target_to_duty_test_() ->
    [
        ?_assertEqual(773, target_to_duty(0, la_machine_configuration:default())),
        ?_assertEqual(341, target_to_duty(100, la_machine_configuration:default()))
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
                    {501, #state{
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
                    {501, #state{
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
-endif.
