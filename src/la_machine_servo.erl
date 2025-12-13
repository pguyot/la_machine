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
    power_on/0,
    power_off/0,
    reset/0,
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
    target_ms = undefined :: undefined | pos_integer()
}).

-opaque state() :: #state{}.

-spec power_on() -> state().
power_on() ->
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

    #state{pre_min = target_to_duty(0), pre_max = target_to_duty(100)}.

-spec power_off() -> ok.
power_off() ->
    ok = ledc:fade_func_uninstall(),
    % Turn down servo
    power_off_boost(),
    ok.

-ifdef(SERVO_EN_BOOST_GPIO).
power_on_boost() ->
    ok = gpio:init(?SERVO_EN_BOOST_GPIO),
    ok = gpio:set_pin_mode(?SERVO_EN_BOOST_GPIO, output),
    ok = gpio:digital_write(?SERVO_EN_BOOST_GPIO, 1).

power_off_boost() ->
    ok = gpio:digital_write(?SERVO_EN_BOOST_GPIO, 0).
-else.
power_on_boost() -> ok.
power_off_boost() -> ok.
-endif.

-spec reset() -> ok.
reset() ->
    State0 = power_on(),
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
set_target(Target, State) ->
    Duty = target_to_duty(Target),
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
set_target(Target, TimeMS, State) ->
    Duty = target_to_duty(Target),
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

target_to_duty(Target) ->
    Angle = Target * (?SERVO_INTERRUPT_ANGLE - ?SERVO_CLOSED_ANGLE) / 100 + ?SERVO_CLOSED_ANGLE,
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
% Values depend on ?SERVO_INTERRUPT_ANGLE and ?SERVO_CLOSED_ANGLE
-if(?HARDWARE_REVISION =:= proto_20241023).
target_to_duty_test_() ->
    [
        ?_assertEqual(432, target_to_duty(0)),
        ?_assertEqual(955, target_to_duty(100))
    ].
-elif(?HARDWARE_REVISION =:= proto_20260106).
target_to_duty_test_() ->
    [
        ?_assertEqual(841, target_to_duty(0)),
        ?_assertEqual(341, target_to_duty(100))
    ].
-else.
-error({unsupported_hardware_revision, ?HARDWARE_REVISION}).
-endif.

target_duty_timeout_test_() ->
    [
        ?_assertMatch(
            {0, #state{pre_min = 318, pre_max = 318, target = 318}},
            target_duty_timeout(318, 0, #state{pre_min = 318, pre_max = 318})
        ),
        ?_assertMatch(
            {501, #state{pre_min = 318, pre_max = 887, target = 887}},
            target_duty_timeout(887, 0, #state{pre_min = 318, pre_max = 318})
        ),
        ?_assertMatch(
            {600, #state{pre_min = 318, pre_max = 887, target = 887}},
            target_duty_timeout(887, 600, #state{pre_min = 318, pre_max = 318})
        ),
        ?_assertMatch(
            {501, #state{pre_min = 318, pre_max = 887, target = 318}},
            target_duty_timeout(318, 0, #state{pre_min = 887, pre_max = 887})
        )
    ].

reset_target_test_() ->
    [
        ?_assertMatch(
            #state{pre_min = 887, pre_max = 887, target = undefined},
            reset_target(#state{pre_min = 318, pre_max = 318, target = 887})
        ),
        ?_assertMatch(
            #state{pre_min = 500, pre_max = 500, target = undefined},
            reset_target(#state{pre_min = 400, pre_max = 600, target = 500})
        )
    ].
-endif.
