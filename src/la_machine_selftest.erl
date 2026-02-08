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
%% compute the proper closed duty and interrupt duty, and eventually
%% update configuration. It will also play a sound to signify self test
%% is correct. If there is any failure, another sound will be played.
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine_selftest).

-include("la_machine_definitions.hrl").

-export([
    report/1,
    run/3
]).

-define(SELF_TEST_EXIT_WAKEUP_TIMER, 5).
-define(SELF_TEST_INIT_WAKEUP_TIMER, 60).
-define(SERVO_SELF_TEST_DUTY,
    ((4 * ?DEFAULT_SERVO_INTERRUPT_DUTY + ?DEFAULT_SERVO_CLOSED_DUTY) div 5)
).

% Total arm course in duty units.
-define(DUTY_COURSE, 865).
% Expected range for the 85% target duty (where the arm just touches the
% button but doesn't push it off).
-define(MIN_85_DUTY, 700).
-define(MAX_85_DUTY, 864).

% Duty units are on 14 bits (16384)
-define(DUTY_CALIBRATION_STEP, 16).

report(Config0) ->
    {SelfTestResult, SelfTestBattery, SelfTestTime} =
        la_machine_configuration:self_test_result(Config0),
    ClosedDuty = la_machine_configuration:closed_duty(Config0),
    InterruptDuty = la_machine_configuration:interrupt_duty(Config0),
    {ok, Player} = la_machine_player:start_link(Config0),
    Scenario =
        case SelfTestResult of
            <<"OK">> ->
                [
                    {aac, <<"_selftest/success_44kHz.mp3">>},
                    {wait, sound},
                    {aac, <<"_selftest/success_48kHz.mp3">>},
                    {wait, sound}
                ];
            _ ->
                [{aac, <<"_selftest/failure.mp3">>}, {wait, sound}]
        end,
    ok = la_machine_player:play(Player, Scenario),
    la_machine_player:stop(Player),
    case SelfTestResult of
        <<"OK">> -> ok;
        _ -> report_countdown(SelfTestResult, 10, 10)
    end,
    io:format(
        "Self-test result: ~s (time: ~B, ~B secs ago), closed_duty = ~B, interrupt_duty = ~B, battery = ~B\n",
        [
            SelfTestResult,
            SelfTestTime,
            (erlang:system_time(millisecond) - SelfTestTime) div 1000,
            ClosedDuty,
            InterruptDuty,
            SelfTestBattery
        ]
    ),
    ?SELF_TEST_EXIT_WAKEUP_TIMER.

report_countdown(_Cause, 0, _Total) ->
    ok;
report_countdown(Cause, N, Total) ->
    io:format("Self-test failure: ~s (~B/~B)\n", [Cause, N, Total]),
    timer:sleep(1000),
    report_countdown(Cause, N - 1, Total).

run(Config0, sleep_wakeup_timer, _ButtonState) ->
    Config1 = la_machine_configuration:set_self_test_result(
        Config0, <<"GPIO wakeup failed or interrupt not switched">>, 0
    ),
    la_machine_configuration:save(Config1),
    ?SELF_TEST_EXIT_WAKEUP_TIMER;
run(Config0, undefined, _ButtonState) ->
    % First part of test is to make sure the battery is charging or 100%
    % (cable should be plugged)
    BatteryTest =
        case la_machine_battery:get_voltage() of
            {ok, 100} ->
                ok;
            {ok, _} ->
                case la_machine_battery:is_charging() of
                    true -> ok;
                    false -> {error, <<"Battery not charging (unplugged ?)">>}
                end;
            {error, _} ->
                {error, <<"Failed to read battery voltage">>}
        end,
    case BatteryTest of
        ok ->
            {ok, Player} = la_machine_player:start_link(Config0),
            ok = la_machine_player:play(Player, [{aac, <<"_selftest/start.mp3">>}, {wait, sound}]),
            la_machine_player:stop(Player),
            io:format(
                "Unplug La Machine, put it in normal resting position and switch the interrupt for self test within the next 60 secs\n"
            ),
            ?SELF_TEST_INIT_WAKEUP_TIMER;
        {error, Reason} ->
            Config1 = la_machine_configuration:set_self_test_result(
                Config0, Reason, 0
            ),
            la_machine_configuration:save(Config1),
            ?SELF_TEST_EXIT_WAKEUP_TIMER
    end;
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
            case test_lis3dh() of
                ok ->
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
                            {WaitTimeMS1, _Servo1} = la_machine_servo:set_duty(
                                ?SERVO_SELF_TEST_DUTY, Servo0
                            ),
                            timer:sleep(WaitTimeMS1),
                            la_machine_servo:power_off(),
                            Config1 = la_machine_configuration:set_self_test_result(
                                Config0, Reason, BatteryVoltage
                            ),
                            la_machine_configuration:save(Config1),
                            ?SELF_TEST_EXIT_WAKEUP_TIMER
                    end;
                {error, AccReason} ->
                    Config1 = la_machine_configuration:set_self_test_result(
                        Config0, AccReason, BatteryVoltage
                    ),
                    la_machine_configuration:save(Config1),
                    ?SELF_TEST_EXIT_WAKEUP_TIMER
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

test_lis3dh() ->
    try la_machine_lis3dh:setup() of
        ok -> ok;
        not_resting -> {error, <<"Accelerometer: not in resting position">>};
        {play, meuh} -> {error, <<"Accelerometer: unexpected meuh detection">>};
        replaced -> {error, <<"Accelerometer: unexpected replaced detection">>}
    catch
        Class:Reason ->
            ReasonBin = atom_to_binary(Class),
            DetailBin0 = iolist_to_binary(io_lib:format("~p", [Reason])),
            DetailBin =
                case DetailBin0 of
                    <<Short:128/binary, _/binary>> -> <<Short/binary, "...">>;
                    _ -> DetailBin0
                end,
            {error, <<"Accelerometer error: ", ReasonBin/binary, " ", DetailBin/binary>>}
    end.

-if(?HARDWARE_REVISION =:= proto_20241023).
-spec test_servo(la_machine_configuration:config(), la_machine_servo:state()) ->
    {ok, la_machine_configuration:config()} | {error, binary()}.
test_servo(Config0, _Servo0) ->
    {ok, Config0}.
-elif(?HARDWARE_REVISION =:= proto_20260106).
-spec test_servo(la_machine_configuration:config(), la_machine_servo:state()) ->
    {ok, la_machine_configuration:config()} | {error, binary()}.
test_servo(Config0, Servo0) ->
    StartDuty = ?SERVO_SELF_TEST_DUTY,
    {WaitTimeMS1, Servo1} = la_machine_servo:set_duty(StartDuty, Servo0),
    timer:sleep(WaitTimeMS1),
    case gpio:digital_read(?BUTTON_GPIO) of
        ?BUTTON_GPIO_ON ->
            test_servo_calibrate_loop(Config0, Servo1, StartDuty - ?DUTY_CALIBRATION_STEP);
        ?BUTTON_GPIO_OFF ->
            {error, <<"Button is initially off">>}
    end.

test_servo_calibrate_loop(_Config0, _Servo0, Duty) when Duty =< 0 ->
    {error, <<"Button never went off">>};
test_servo_calibrate_loop(Config0, Servo0, Duty) ->
    {WaitTimeMS, Servo1} = la_machine_servo:set_duty(Duty, Servo0),
    timer:sleep(WaitTimeMS),
    case gpio:digital_read(?BUTTON_GPIO) of
        ?BUTTON_GPIO_ON ->
            test_servo_calibrate_loop(Config0, Servo1, Duty - ?DUTY_CALIBRATION_STEP);
        ?BUTTON_GPIO_OFF ->
            Duty85 = Duty + ?DUTY_CALIBRATION_STEP,
            case Duty85 >= ?MIN_85_DUTY andalso Duty85 =< ?MAX_85_DUTY of
                true ->
                    ClosedDuty = Duty85 + (85 * ?DUTY_COURSE) div 100,
                    InterruptDuty = ClosedDuty - ?DUTY_COURSE,
                    {WaitTimeMS1, Servo2} = la_machine_servo:set_duty(InterruptDuty, Servo1),
                    timer:sleep(WaitTimeMS1),
                    {WaitTimeMS2, _Servo3} = la_machine_servo:set_duty(ClosedDuty, Servo2),
                    timer:sleep(WaitTimeMS2),
                    Config1 = la_machine_configuration:set_closed_duty(Config0, ClosedDuty),
                    Config2 = la_machine_configuration:set_interrupt_duty(Config1, InterruptDuty),
                    {ok, Config2};
                false ->
                    Duty85Bin = integer_to_binary(Duty85),
                    {error, <<"Unexpected 85% duty: ", Duty85Bin/binary>>}
            end
    end.

-else.
-error({unsupported_hardware_revision, ?HARDWARE_REVISION}).
-endif.
