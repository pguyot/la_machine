%
% This file is part of La Machine
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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
%% @doc La Machine self-test and calibration module (after NVS is erased,
%% typically in factory).
%%
%% When NVS is erased, La Machine is put to deep sleep and waits for button
%% (and only for button).
%% Button should be pressed with the USB cable unplugged, and not touched.
%% La Machine should be in resting position. Then this function is called. It will
%% compute the proper closed angle and interrupt angle, and eventually
%% update configuration. It will also play a sound to signify self test
%% is correct. If there is any failure, another sound will be played.
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine_selftest).

-include("la_machine_definitions.hrl").

-export([
    report/2,
    run/3
]).

-define(SELF_TEST_EXIT_WAKEUP_TIMER, 10).
-define(SELF_TEST_INIT_WAKEUP_TIMER, 60).
-define(SERVO_SELF_TEST_ANGLE,
    ((?DEFAULT_SERVO_INTERRUPT_ANGLE + ?DEFAULT_SERVO_CLOSED_ANGLE) div 2)
).

% When the arm pushes the button, it will switch off the interrupt before the
% actual end position. This margin is there to allow the arm to move further.
% Unfortunately we cannot calibrate it better.
-define(INTERRUPT_ANGLE_MARGIN, 5).

report(Config0, Force) ->
    {SelfTestResult, _SelfTestBattery, _SelfTestTime} =
        la_machine_configuration:self_test_result(Config0),
    if
        SelfTestResult =/= <<"OK">> -> report0(Config0);
        Force -> report0(Config0);
        true -> ok
    end.

report0(Config0) ->
    boot_countdown(5),
    case la_machine_configuration:configured(Config0) of
        true ->
            {SelfTestResult, SelfTestBattery, SelfTestTime} =
                la_machine_configuration:self_test_result(Config0),
            ClosedAngle = la_machine_configuration:closed_angle(Config0),
            InterruptAngle = la_machine_configuration:interrupt_angle(Config0),
            io:format(
                "Self-test result: ~s (time: ~B, ~B secs ago), closed = ~B, interrupt = ~B, battery = ~B\n",
                [
                    SelfTestResult,
                    SelfTestTime,
                    (erlang:system_time(millisecond) - SelfTestTime) div 1000,
                    ClosedAngle,
                    InterruptAngle,
                    SelfTestBattery
                ]
            );
        false ->
            io:format(
                "Unplug La Machine and switch the interrupt for self test within the next 60 secs\n"
            )
    end.

boot_countdown(0) ->
    ok;
boot_countdown(N) ->
    io:format("Booting in ~B secs\n", [N]),
    timer:sleep(1000),
    boot_countdown(N - 1).

run(Config0, sleep_wakeup_timer, _ButtonState) ->
    Config1 = la_machine_configuration:set_self_test_result(
        Config0, <<"GPIO wakeup failed or interrupt not switched">>, 0
    ),
    la_machine_configuration:save(Config1),
    ?SELF_TEST_EXIT_WAKEUP_TIMER;
run(Config0, undefined, _ButtonState) ->
    Servo0 = la_machine_servo:power_on(Config0),
    {WaitTimeMS1, _Servo1} = la_machine_servo:set_angle(?SERVO_SELF_TEST_ANGLE, Servo0),
    timer:sleep(WaitTimeMS1),
    la_machine_servo:power_off(),
    ?SELF_TEST_INIT_WAKEUP_TIMER;
run(Config0, sleep_wakeup_gpio, off) ->
    Config1 = la_machine_configuration:set_self_test_result(
        Config0, <<"Button OFF on initial wakeup">>, 0
    ),
    la_machine_configuration:save(Config1),
    ?SELF_TEST_EXIT_WAKEUP_TIMER;
run(Config0, sleep_wakeup_gpio, on) ->
    % Measure battery before calibration
    case la_machine_battery:get_voltage() of
        {ok, BatteryVoltage} ->
            case la_machine_battery:is_charging() of
                true ->
                    Config1 = la_machine_configuration:set_self_test_result(
                        Config0, <<"USB cable not unplugged, battery is charging">>, BatteryVoltage
                    ),
                    la_machine_configuration:save(Config1),
                    ?SELF_TEST_EXIT_WAKEUP_TIMER;
                false ->
                    Servo0 = la_machine_servo:power_on(Config0),
                    case test_servo(Config0, Servo0) of
                        {ok, Config1} ->
                            la_machine_servo:power_off(),
                            Config2 = la_machine_configuration:set_self_test_result(
                                Config1, <<"OK">>, BatteryVoltage
                            ),
                            la_machine_configuration:save(Config2),
                            ?SELF_TEST_EXIT_WAKEUP_TIMER;
                        {error, Reason} ->
                            {WaitTimeMS1, _Servo1} = la_machine_servo:set_angle(
                                ?SERVO_SELF_TEST_ANGLE, Servo0
                            ),
                            timer:sleep(WaitTimeMS1),
                            la_machine_servo:power_off(),
                            Config1 = la_machine_configuration:set_self_test_result(
                                Config0, Reason, BatteryVoltage
                            ),
                            la_machine_configuration:save(Config1),
                            ?SELF_TEST_EXIT_WAKEUP_TIMER
                    end
            end;
        {error, _} ->
            Config1 = la_machine_configuration:set_self_test_result(
                Config0, <<"Failed to read battery voltage">>, 0
            ),
            la_machine_configuration:save(Config1),
            ?SELF_TEST_EXIT_WAKEUP_TIMER
    end;
run(Config0, Other, _ButtonState) ->
    WakeupCauseBin = atom_to_binary(Other),
    Config1 = la_machine_configuration:set_self_test_result(
        Config0, <<"Unknown wakeup cause: ", WakeupCauseBin/binary>>, 0
    ),
    la_machine_configuration:save(Config1),
    ?SELF_TEST_EXIT_WAKEUP_TIMER.

-if(?HARDWARE_REVISION =:= proto_20241023).
-spec test_servo(la_machine_configuration:config(), la_machine_servo:state()) ->
    {ok, la_machine_configuration:config()} | {error, binary()}.
test_servo(Config0, _Servo0) ->
    {ok, Config0}.
-elif(?HARDWARE_REVISION =:= proto_20260106).
-spec test_servo(la_machine_configuration:config(), la_machine_servo:state()) ->
    {ok, la_machine_configuration:config()} | {error, binary()}.
test_servo(Config0, Servo0) ->
    test_servo_close(Config0, Servo0).

test_servo_close(Config0, Servo0) ->
    StartAngle = ?SERVO_SELF_TEST_ANGLE,
    {WaitTimeMS1, Servo1} = la_machine_servo:set_angle(StartAngle, Servo0),
    timer:sleep(WaitTimeMS1),
    case gpio:digital_read(?CALIBRATION_BUTTON_GPIO) of
        low ->
            case test_servo_close_loop(Servo1, StartAngle) of
                {ok, {Servo2, ClosedAngle}} ->
                    Config1 = la_machine_configuration:set_closed_angle(Config0, ClosedAngle),
                    test_servo_interrupt(Config1, Servo2);
                {error, _Reason} = ErrorT ->
                    ErrorT
            end;
        high ->
            {error, <<"Calibration button failure -- button is initially high">>}
    end.

test_servo_interrupt(Config0, Servo0) ->
    ClosedAngle = la_machine_configuration:closed_angle(Config0),
    StartAngle = (?DEFAULT_SERVO_INTERRUPT_ANGLE + ClosedAngle) div 2,
    {WaitTimeMS1, Servo1} = la_machine_servo:set_angle(StartAngle, Servo0),
    timer:sleep(WaitTimeMS1),
    case gpio:digital_read(?BUTTON_GPIO) of
        ?BUTTON_GPIO_ON ->
            case test_servo_interrupt_loop(Servo1, StartAngle) of
                {ok, {Servo2, InterruptAngle}} ->
                    {WaitTimeMS3, Servo3} = la_machine_servo:set_angle(ClosedAngle, Servo2),
                    timer:sleep(WaitTimeMS3),
                    {WaitTimeMS4, Servo4} = la_machine_servo:set_angle(InterruptAngle, Servo3),
                    timer:sleep(WaitTimeMS4),
                    {WaitTimeMS5, _Servo5} = la_machine_servo:set_angle(ClosedAngle, Servo4),
                    timer:sleep(WaitTimeMS5),
                    Config1 = la_machine_configuration:set_interrupt_angle(Config0, InterruptAngle),
                    {ok, Config1};
                {error, _Reason} = ErrorT ->
                    ErrorT
            end;
        ?BUTTON_GPIO_OFF ->
            {error, <<"Interrupt button failure -- button is initially off">>}
    end.

test_servo_close_loop(Servo0, Angle) when Angle > 0 andalso Angle < 180 ->
    case gpio:digital_read(?CALIBRATION_BUTTON_GPIO) of
        high ->
            ProperAngle = Angle - 1,
            {WaitTimeMS1, Servo1} = la_machine_servo:set_angle(ProperAngle, Servo0),
            timer:sleep(WaitTimeMS1),
            case gpio:digital_read(?CALIBRATION_BUTTON_GPIO) of
                low ->
                    {ok, {Servo1, ProperAngle}};
                high ->
                    test_servo_close_loop(Servo1, ProperAngle)
            end;
        low ->
            NewAngle = Angle + 1,
            {WaitTimeMS1, Servo1} = la_machine_servo:set_angle(NewAngle, Servo0),
            timer:sleep(WaitTimeMS1),
            test_servo_close_loop(Servo1, NewAngle)
    end;
test_servo_close_loop(_Servo0, 180) ->
    {error, <<"Calibration button failure -- button is always low">>};
test_servo_close_loop(_Servo0, 0) ->
    {error, <<"Calibration button failure -- button always remained high">>}.

test_servo_interrupt_loop(Servo0, Angle) when Angle > 0 ->
    {WaitTimeMS1, Servo1} = la_machine_servo:set_angle(Angle, Servo0),
    timer:sleep(WaitTimeMS1),
    {WaitTimeMS2, Servo2} = la_machine_servo:set_angle(min(Angle + 20, 180), Servo1),
    timer:sleep(WaitTimeMS2),
    ButtonState = gpio:digital_read(?BUTTON_GPIO),
    case ButtonState of
        ?BUTTON_GPIO_OFF ->
            {ok, {Servo2, max(Angle - ?INTERRUPT_ANGLE_MARGIN, 0)}};
        ?BUTTON_GPIO_ON ->
            NewAngle = Angle - 1,
            test_servo_interrupt_loop(Servo2, NewAngle)
    end;
test_servo_interrupt_loop(_Servo0, _Angle) ->
    {error, <<"Interrupt button failure -- button is always off">>}.

-else.
-error({unsupported_hardware_revision, ?HARDWARE_REVISION}).
-endif.
