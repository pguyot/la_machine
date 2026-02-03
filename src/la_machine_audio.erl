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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    power_on/0,
    power_off/0,
    reset/0,
    play/1
]).

-ifdef(MAX_SD_MODE_GPIO).
power_on() ->
    % Reset pin (turn on MAX98357A)
    ok = gpio:init(?MAX_SD_MODE_GPIO),
    ok = gpio:hold_dis(?MAX_SD_MODE_GPIO),
    ok = gpio:set_pin_mode(?MAX_SD_MODE_GPIO, input),
    ok = gpio:set_pin_pull(?MAX_SD_MODE_GPIO, floating),
    ok.

power_off() ->
    % Turn down MAX98357A
    ok = gpio:set_pin_mode(?MAX_SD_MODE_GPIO, output),
    ok = gpio:digital_write(?MAX_SD_MODE_GPIO, 0),
    ok = gpio:hold_en(?MAX_SD_MODE_GPIO),
    ok.
-else.
power_on() -> ok.
power_off() -> ok.
-endif.

reset() ->
    power_off().

play(MP3Filename) ->
    AudioPipeline = esp_adf_audio_pipeline:init([]),
    {ok, MP3File} = la_machine_sounds:get_sound(MP3Filename),

    SampleRate = get_mp3_sample_rate(MP3File),
    case SampleRate of
        44100 -> ok;
        48000 -> ok;
        Other -> error({unsupported_sample_rate, Other, MP3Filename})
    end,

    MP3Decoder = esp_adf_mp3_decoder:init([]),
    ok = esp_adf_audio_element:set_read_binary(MP3Decoder, MP3File),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, MP3Decoder, <<"mp3">>),

    Filter = esp_adf_rsp_filter:init([
        {src_rate, SampleRate}, {src_ch, 1}, {dest_rate, SampleRate}, {dest_ch, 2}
    ]),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, Filter, <<"filter">>),

    I2SOutput = esp_adf_i2s_output:init([
        {rate, SampleRate},
        {bits, 16},
        {gpio_bclk, ?MAX_BCLK_GPIO},
        {gpio_lrclk, ?MAX_LRC_GPIO},
        {gpio_dout, ?MAX_DIN_GPIO}
    ]),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, I2SOutput, <<"i2s">>),

    ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"mp3">>, <<"filter">>, <<"i2s">>]),

    ok = esp_adf_audio_pipeline:run(AudioPipeline),

    ok =
        receive
            {audio_element, I2SOutput, {status, state_finished}} -> ok
        end,

    ok = esp_adf_audio_pipeline:stop(AudioPipeline),

    ok =
        receive
            {audio_element, MP3Decoder, {status, state_stopped}} -> ok
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
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, MP3Decoder),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, Filter),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, I2SOutput),
    ok.

%% @doc Extract sample rate from MP3 binary data
%% @private
get_mp3_sample_rate(
    <<"ID3", _MajorVersion:8, _MinorVersion:8, _Flags:8, S1:8, S2:8, S3:8, S4:8, Rest/binary>>
) ->
    % ID3v2 tag present - size is stored as syncsafe integer (7 bits per byte)
    TagSize =
        ((S1 band 16#7F) bsl 21) bor ((S2 band 16#7F) bsl 14) bor
            ((S3 band 16#7F) bsl 7) bor (S4 band 16#7F),
    <<_TagData:TagSize/binary, FrameData/binary>> = Rest,
    parse_mp3_frame_header(FrameData);
get_mp3_sample_rate(Data) ->
    parse_mp3_frame_header(Data).

parse_mp3_frame_header(
    <<16#FF, 2#111:3, MPEGVersion:2, _Layer:2, _Protection:1, _Bitrate:4, SampleRateIndex:2,
        _Rest/bitstring>>
) ->
    sample_rate(MPEGVersion, SampleRateIndex);
parse_mp3_frame_header(<<_:8, Rest/binary>>) ->
    % Skip bytes until we find frame sync
    parse_mp3_frame_header(Rest);
parse_mp3_frame_header(<<>>) ->
    error(mp3_frame_not_found).

% MPEG Version 1
sample_rate(2#11, 2#00) -> 44100;
sample_rate(2#11, 2#01) -> 48000;
sample_rate(2#11, 2#10) -> 32000;
% MPEG Version 2
sample_rate(2#10, 2#00) -> 22050;
sample_rate(2#10, 2#01) -> 24000;
sample_rate(2#10, 2#10) -> 16000;
% MPEG Version 2.5
sample_rate(2#00, 2#00) -> 11025;
sample_rate(2#00, 2#01) -> 12000;
sample_rate(2#00, 2#10) -> 8000;
% Reserved or invalid
sample_rate(_, 2#11) -> error(invalid_sample_rate_index);
sample_rate(2#01, _) -> error(reserved_mpeg_version).

-ifdef(TEST).
get_mp3_sample_rate_test() ->
    try atomvm:platform() of
        esp32 ->
            {ok, Hit00082} = la_machine_sounds:get_sound("hits/00082.mp3"),
            44100 = get_mp3_sample_rate(Hit00082),
            {ok, Hit00117} = la_machine_sounds:get_sound("hits/00117.mp3"),
            48000 = get_mp3_sample_rate(Hit00117),
            ok;
        generic_unix ->
            ok
    catch
        error:undef ->
            ok
    end.
-endif.
