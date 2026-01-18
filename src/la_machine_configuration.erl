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
    configured/1,
    closed_angle/1,
    interrupt_angle/1,
    self_test_result/1,
    set_closed_angle/2,
    set_interrupt_angle/2,
    set_self_test_result/3
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
    configured :: boolean(),
    closed_angle :: 0..180,
    interrupt_angle :: 0..180,
    self_test_time :: non_neg_integer(),
    self_test_battery :: non_neg_integer(),
    self_test_result :: binary()
}).

-type config() :: #config{}.

-define(DEFAULT_CONFIGURATION, #config{
    configured = false,
    closed_angle = ?DEFAULT_SERVO_CLOSED_ANGLE,
    interrupt_angle = ?DEFAULT_SERVO_INTERRUPT_ANGLE,
    self_test_time = 0,
    self_test_battery = 0,
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

configured(#config{configured = Configured}) -> Configured.

closed_angle(#config{closed_angle = ClosedAngle}) -> ClosedAngle.

interrupt_angle(#config{interrupt_angle = InterruptAngle}) -> InterruptAngle.

self_test_result(#config{
    self_test_time = SelfTestTime,
    self_test_battery = SelfTestBattery,
    self_test_result = SelfTestResult
}) ->
    {SelfTestResult, SelfTestBattery, SelfTestTime}.

set_closed_angle(#config{} = Config0, Angle) when
    is_integer(Angle) andalso Angle >= 0 andalso Angle =< 180
->
    Config0#config{
        closed_angle = Angle
    }.

set_interrupt_angle(#config{} = Config0, Angle) when
    is_integer(Angle) andalso Angle >= 0 andalso Angle =< 180
->
    Config0#config{
        interrupt_angle = Angle
    }.

set_self_test_result(#config{} = Config0, SelfTestResult, SelfTestBattery) ->
    Config0#config{
        configured = true,
        self_test_time = erlang:system_time(millisecond),
        self_test_battery = SelfTestBattery,
        self_test_result = SelfTestResult
    }.

-spec deserialize_configuration(binary()) -> config().
deserialize_configuration(
    <<ClosedAngle, InterruptAngle, SelfTestTime:64, SelfTestBattery:16, SelfTestResult/binary>>
) when
    ClosedAngle =< 180 andalso InterruptAngle =< 180
->
    #config{
        configured = true,
        closed_angle = ClosedAngle,
        interrupt_angle = InterruptAngle,
        self_test_time = SelfTestTime,
        self_test_battery = SelfTestBattery,
        self_test_result = SelfTestResult
    };
deserialize_configuration(_) ->
    ?DEFAULT_CONFIGURATION.

-spec serialize_configuration(config()) -> binary().
serialize_configuration(#config{
    configured = true,
    closed_angle = ClosedAngle,
    interrupt_angle = InterruptAngle,
    self_test_time = SelfTestTime,
    self_test_battery = SelfTestBattery,
    self_test_result = SelfTestResult
}) ->
    <<ClosedAngle, InterruptAngle, SelfTestTime:64, SelfTestBattery:16, SelfTestResult/binary>>;
serialize_configuration(#config{}) ->
    <<>>.

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
                configured = true,
                closed_angle = 0,
                interrupt_angle = 0,
                self_test_time = 0,
                self_test_battery = 0,
                self_test_result = <<"">>
            },
            deserialize_configuration(<<0, 0, 0:64, 0:16>>)
        ),
        ?_assertEqual(
            #config{
                configured = true,
                closed_angle = 125,
                interrupt_angle = 30,
                self_test_time = 42,
                self_test_battery = 2100,
                self_test_result = <<"OK">>
            },
            deserialize_configuration(<<125, 30, 42:64, 2100:16, "OK">>)
        )
    ].

serialize_configuration_test_() ->
    [
        ?_assertMatch(
            <<>>,
            serialize_configuration(
                #config{configured = false}
            )
        ),
        ?_assertMatch(
            <<0, 0, 0:64, 0:16>>,
            serialize_configuration(
                #config{
                    configured = true,
                    closed_angle = 0,
                    interrupt_angle = 0,
                    self_test_battery = 0,
                    self_test_time = 0,
                    self_test_result = <<"">>
                }
            )
        ),
        ?_assertMatch(
            <<125, 30, 42:64, 2100:16, "OK">>,
            serialize_configuration(
                #config{
                    configured = true,
                    closed_angle = 125,
                    interrupt_angle = 30,
                    self_test_time = 42,
                    self_test_battery = 2100,
                    self_test_result = <<"OK">>
                }
            )
        )
    ].
-endif.
