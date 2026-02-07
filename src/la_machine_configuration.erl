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
%% @doc Configuration of La Machine as stored in NVS (non-volatile storage)
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine_configuration).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("la_machine_definitions.hrl").

% State persistence
-export([
    load/0,
    save/1
]).

% Accessors
-export([
    self_test_state/1,
    closed_duty/1,
    interrupt_duty/1,
    self_test_result/1,
    set_closed_duty/2,
    set_interrupt_duty/2,
    set_self_test_result/3,
    set_self_test_reported/1
]).

-ifdef(TEST).
-export([default/0]).
-endif.

-export_type([
    config/0
]).

-define(NVS_NAMESPACE, la_machine).
-define(NVS_KEY, configuration).

-record(config, {
    closed_duty :: non_neg_integer(),
    interrupt_duty :: non_neg_integer(),
    self_test_time :: non_neg_integer(),
    self_test_battery :: non_neg_integer(),
    self_test_reported :: boolean(),
    self_test_result :: binary()
}).

-type config() :: #config{}.

-define(DEFAULT_CONFIGURATION, #config{
    closed_duty = ?DEFAULT_SERVO_CLOSED_DUTY,
    interrupt_duty = ?DEFAULT_SERVO_INTERRUPT_DUTY,
    self_test_time = 0,
    self_test_battery = 0,
    self_test_reported = false,
    self_test_result = <<>>
}).

-ifdef(TEST).
default() ->
    ?DEFAULT_CONFIGURATION.
-endif.

load() ->
    case esp:nvs_get_binary(?NVS_NAMESPACE, ?NVS_KEY) of
        undefined ->
            ?DEFAULT_CONFIGURATION;
        Bin ->
            deserialize_configuration(Bin)
    end.

save(Config) ->
    Serialized = serialize_configuration(Config),
    esp:nvs_put_binary(?NVS_NAMESPACE, ?NVS_KEY, Serialized).

self_test_state(#config{self_test_result = <<>>}) ->
    uncalibrated;
self_test_state(#config{self_test_reported = false}) ->
    unreported;
self_test_state(#config{}) ->
    calibrated.

closed_duty(#config{closed_duty = ClosedDuty}) -> ClosedDuty.

interrupt_duty(#config{interrupt_duty = InterruptDuty}) -> InterruptDuty.

self_test_result(#config{
    self_test_time = SelfTestTime,
    self_test_battery = SelfTestBattery,
    self_test_result = SelfTestResult
}) ->
    {SelfTestResult, SelfTestBattery, SelfTestTime}.

set_closed_duty(#config{} = Config0, Duty) when
    is_integer(Duty) andalso Duty >= 0 andalso Duty =< ?SERVO_MAX_DUTY
->
    Config0#config{
        closed_duty = Duty
    }.

set_interrupt_duty(#config{} = Config0, Duty) when
    is_integer(Duty) andalso Duty >= 0 andalso Duty =< ?SERVO_MAX_DUTY
->
    Config0#config{
        interrupt_duty = Duty
    }.

set_self_test_result(#config{} = Config0, SelfTestResult, SelfTestBattery) ->
    Config0#config{
        self_test_time = erlang:system_time(millisecond),
        self_test_battery = SelfTestBattery,
        self_test_result = SelfTestResult
    }.

set_self_test_reported(#config{} = Config0) ->
    Config0#config{
        self_test_reported = true
    }.

-spec deserialize_configuration(binary()) -> config().
deserialize_configuration(
    <<ClosedDuty:16, InterruptDuty:16, SelfTestTime:64, SelfTestBattery:16, SelfTestReportedByte,
        SelfTestResult/binary>>
) when
    ClosedDuty =< ?SERVO_MAX_DUTY andalso InterruptDuty =< ?SERVO_MAX_DUTY
->
    #config{
        closed_duty = ClosedDuty,
        interrupt_duty = InterruptDuty,
        self_test_time = SelfTestTime,
        self_test_battery = SelfTestBattery,
        self_test_reported = SelfTestReportedByte =/= 0,
        self_test_result = SelfTestResult
    };
deserialize_configuration(_) ->
    ?DEFAULT_CONFIGURATION.

-spec serialize_configuration(config()) -> binary().
serialize_configuration(#config{
    closed_duty = ClosedDuty,
    interrupt_duty = InterruptDuty,
    self_test_time = SelfTestTime,
    self_test_battery = SelfTestBattery,
    self_test_reported = SelfTestReported,
    self_test_result = SelfTestResult
}) ->
    SelfTestReportedByte =
        case SelfTestReported of
            false -> 0;
            _ -> 1
        end,
    <<ClosedDuty:16, InterruptDuty:16, SelfTestTime:64, SelfTestBattery:16, SelfTestReportedByte,
        SelfTestResult/binary>>.

-ifdef(TEST).
deserialize_configuration_test_() ->
    [
        ?_assertEqual(
            ?DEFAULT_CONFIGURATION,
            deserialize_configuration(<<>>)
        ),
        ?_assertEqual(
            ?DEFAULT_CONFIGURATION,
            deserialize_configuration(<<255, 255>>)
        ),
        ?_assertEqual(
            #config{
                closed_duty = 0,
                interrupt_duty = 0,
                self_test_time = 0,
                self_test_battery = 0,
                self_test_reported = false,
                self_test_result = <<"">>
            },
            deserialize_configuration(<<0:16, 0:16, 0:64, 0:16, 0>>)
        ),
        ?_assertEqual(
            #config{
                closed_duty = 1547,
                interrupt_duty = 682,
                self_test_time = 42,
                self_test_battery = 2100,
                self_test_reported = false,
                self_test_result = <<"OK">>
            },
            deserialize_configuration(<<1547:16, 682:16, 42:64, 2100:16, 0, "OK">>)
        ),
        ?_assertEqual(
            #config{
                closed_duty = 1547,
                interrupt_duty = 682,
                self_test_time = 42,
                self_test_battery = 2100,
                self_test_reported = true,
                self_test_result = <<"OK">>
            },
            deserialize_configuration(<<1547:16, 682:16, 42:64, 2100:16, 1, "OK">>)
        )
    ].

serialize_configuration_test_() ->
    [
        ?_assertMatch(
            <<0:16, 0:16, 0:64, 0, 0:16>>,
            serialize_configuration(
                #config{
                    closed_duty = 0,
                    interrupt_duty = 0,
                    self_test_battery = 0,
                    self_test_time = 0,
                    self_test_reported = false,
                    self_test_result = <<"">>
                }
            )
        ),
        ?_assertMatch(
            <<1547:16, 682:16, 42:64, 2100:16, 0, "OK">>,
            serialize_configuration(
                #config{
                    closed_duty = 1547,
                    interrupt_duty = 682,
                    self_test_time = 42,
                    self_test_battery = 2100,
                    self_test_reported = false,
                    self_test_result = <<"OK">>
                }
            )
        ),
        ?_assertMatch(
            <<1547:16, 682:16, 42:64, 2100:16, 1, "OK">>,
            serialize_configuration(
                #config{
                    closed_duty = 1547,
                    interrupt_duty = 682,
                    self_test_time = 42,
                    self_test_battery = 2100,
                    self_test_reported = true,
                    self_test_result = <<"OK">>
                }
            )
        )
    ].
-endif.
