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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("la_machine_definitions.hrl").

-export([
    init/0,
    is_charging/0,
    get_level/0,
    get_voltage/0
]).

%% @doc Initialize the battery by configuring the stat2 GPIO.
%% @end
-spec init() -> ok.
init() ->
    ok = gpio:init(?BATTERY_STAT2_GPIO),
    ok = gpio:set_pin_mode(?BATTERY_STAT2_GPIO, input),
    ok.

%% @doc Determine if the battery is currently charging.
%% @end
-spec is_charging() -> boolean().
is_charging() ->
    gpio:digital_read(?BATTERY_STAT2_GPIO) =:= low.

%% @doc Return the battery level from 0 to 100, with 20 being the level where
%% it is advised to recharge La Machine
%% @end
-spec get_level() -> {ok, 0..100} | {error, any()}.
get_level() ->
    case get_voltage() of
        {ok, Millivolts} ->
            {ok, compute_soc(Millivolts)};
        Error ->
            Error
    end.

%% @doc Return the battery voltage, using the ADC to measure it.
%% @end
-spec get_voltage() -> {ok, non_neg_integer()} | {error, any()}.
get_voltage() ->
    maybe
        {ok, Unit} ?= esp_adc:init(),
        Measured1 =
            maybe
                {ok, Chan} ?= esp_adc:acquire(?BATTERY_LEVEL_GPIO, Unit),
                Measured0 =
                    maybe
                        {ok, {_Raw, MilliVolts}} ?= esp_adc:sample(Chan, Unit),
                        {ok, MilliVolts * 2}
                    end,
                ok = esp_adc:release_channel(Chan),
                Measured0
            end,
        ok = esp_adc:deinit(Unit),
        Measured1
    end.

compute_soc(Voltage) ->
    SOC =
        if
            % Cap at 100%
            Voltage >= 4100 -> 100;
            % Segment 1: 4.1V to 3.9V (100% to 80%)
            Voltage >= 3900 -> 100 - (4100 - Voltage) * (20 / 200);
            % Segment 2: 3.9V to 3.65V (80% to 20%)
            Voltage >= 3650 -> 80 - (3900 - Voltage) * (60 / 250);
            % Segment 3: 3.65V to 3.3V (20% to 0%)
            Voltage >= 3300 -> 20 - (3650 - Voltage) * (20 / 350);
            % Below 3.3V: 0% SOC
            true -> 0
        end,
    round(SOC).

-ifdef(TEST).
compute_soc_test_() ->
    [
        ?_assertEqual(20, compute_soc(3650)),
        ?_assertEqual(100, compute_soc(4186)),
        ?_assertEqual(0, compute_soc(3300))
    ].
-endif.
