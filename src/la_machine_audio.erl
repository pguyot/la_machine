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
%% @doc La Machine audio interface
%% @end
%%-----------------------------------------------------------------------------
-module(la_machine_audio).

-include("la_machine_definitions.hrl").

-export([
    power_on/0,
    power_off/0,
    reset/0,
    play/1
]).

power_on() ->
    % Reset pin (turn on MAX98357A)
    %   gpio:hold_dis(?MAX_SD_MODE_PIN),
    %   gpio:set_pin_mode(?MAX_SD_MODE_PIN, input),
    %   gpio:set_pin_pull(?MAX_SD_MODE_PIN, floating),
    ok.

power_off() ->
    % Turn down MAX98357A
    %   gpio:set_pin_mode(?MAX_SD_MODE_PIN, output),
    %   gpio:digital_write(?MAX_SD_MODE_PIN, 0),
    %   gpio:hold_en(?MAX_SD_MODE_PIN),
    ok.

reset() ->
    power_off().

play(AACFilename) ->
    AudioPipeline = esp_adf_audio_pipeline:init([]),

    AACFile = atomvm:read_priv(la_machine, AACFilename),
    AACDecoder = esp_adf_aac_decoder:init([]),
    ok = esp_adf_audio_element:set_read_binary(AACDecoder, AACFile),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, AACDecoder, <<"aac">>),

    Filter = esp_adf_rsp_filter:init([
        {src_rate, 22050}, {src_ch, 1}, {dest_rate, 44100}, {dest_ch, 2}
    ]),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, Filter, <<"filter">>),

    I2SOutput = esp_adf_i2s_output:init([
        {rate, 44100},
        {bits, 16},
        {gpio_bclk, ?MAX_BCLK_GPIO},
        {gpio_lrclk, ?MAX_LRC_GPIO},
        {gpio_dout, ?MAX_DIN_GPIO}
    ]),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, I2SOutput, <<"i2s">>),

    ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"aac">>, <<"filter">>, <<"i2s">>]),

    ok = esp_adf_audio_pipeline:run(AudioPipeline),

    ok =
        receive
            {audio_element, I2SOutput, {status, state_finished}} -> ok
        end,

    ok = esp_adf_audio_pipeline:stop(AudioPipeline),

    ok =
        receive
            {audio_element, AACDecoder, {status, state_stopped}} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {audio_element, Filter, {status, state_stopped}} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {audio_element, I2SOutput, {status, state_stopped}} -> ok
        after 500 -> timeout
        end,

    ok = esp_adf_audio_pipeline:wait_for_stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:terminate(AudioPipeline),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, AACDecoder),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, Filter),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, I2SOutput),
    ok.
