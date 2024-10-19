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
%% @doc La Machine battery interface
%% @end
%%-----------------------------------------------------------------------------
-module(la_machine_battery).

-include("la_machine_definitions.hrl").

-ifdef(MODEL_PROTO_20240718).

-export([
    init/0,
    is_charging/0,
    get_level/0
]).

init() ->
    ok = gpio:init(?BATTERY_STAT1_GPIO),
    ok = gpio:set_pin_mode(?BATTERY_STAT1_GPIO, input),
    ok = gpio:init(?BATTERY_STAT2_GPIO),
    ok = gpio:set_pin_mode(?BATTERY_STAT2_GPIO, input),
    ok.

is_charging() ->
    gpio:digital_read(?BATTERY_STAT1_GPIO) =:= high andalso
        gpio:digital_read(?BATTERY_STAT2_GPIO) =:= low.

get_level() ->
    {ok, Unit} = adc:init(),
    {ok, Chan} = adc:acquire(?BATTERY_LEVEL_GPIO, bit_max, db_11, Unit),
    case adc:sample(Chan, Unit, [raw, voltage, {samples, 64}]) of
        {ok, {Raw, MilliVolts}} ->
            % Raw: 2970 Voltage: 2093mV -- 100% 4.11V
            io:format("Raw: ~p Voltage: ~pmV~n", [Raw, MilliVolts]);
        Error ->
            io:format("Error taking reading: ~p~n", [Error])
    end,
    ok = adc:release_channel(Chan),
    ok = adc:deinit(Unit),
    50.

-endif.
