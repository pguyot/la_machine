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
%% @doc La Machine entry point
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine).

-include("la_machine_definitions.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start/0]).

-type action() ::
    setup
    | {
        play,
        LastPlayTime :: non_neg_integer(),
        LastPlaySeq :: non_neg_integer() | undefined,
        PlayIndex :: non_neg_integer()
    }
    | {
        poke,
        PokeIndex :: non_neg_integer()
    }.

init_servo() ->
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

    %   gpio:set_pin_mode(?MOSFET_GPIO, output),
    %   gpio:digital_write(?MOSFET_GPIO, 1),

    LEDCChannel.

play_aac(AACFilename) ->
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            AudioPipeline = esp_adf_audio_pipeline:init([]),

            AACFile = atomvm:read_priv(?MODULE, AACFilename),
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
            ok
        end,
        [monitor]
    ),
    normal =
        receive
            {'DOWN', MonitorRef, process, Pid, Reason} -> Reason
        end,
    ok.

start() ->
    % Configure button GPIO
    ok = gpio:set_pin_mode(?BUTTON_GPIO, input),
    ok = gpio:set_pin_pull(?BUTTON_GPIO, up),
    ok = esp:deep_sleep_enable_gpio_wakeup(1 bsl ?BUTTON_GPIO, 0),

    WakeupCause = esp:sleep_get_wakeup_cause(),
    ButtonState =
        case gpio:digital_read(?BUTTON_GPIO) of
            high -> off;
            low -> on
        end,

    State0 = la_machine_state:load_state(),

    State1 =
        case action(WakeupCause, ButtonState, State0) of
            {play, LastPlayTime, LastPlayIndex, PlayIndex} ->
                play(LastPlayTime, LastPlayIndex, PlayIndex),
                la_machine_state:append_play(1, 0, State0);
            {poke, PokeIndex} ->
                poke(),
                la_machine_state:set_poke_index(PokeIndex + 1, State0);
            setup ->
                LEDCChannel = init_servo(),

                % Ensure servo is at closed angle
                write_angle(?SERVO_CLOSED_ANGLE, LEDCChannel),
                timer:sleep(300),
                ledc:stop(?LEDC_MODE, ?LEDC_CHANNEL, 0),
                State0
        end,
    SleepTimer = compute_sleep_timer(State1),
    la_machine_state:save_state(State1),
    go_to_sleep(SleepTimer).

% Until https://github.com/atomvm/AtomVM/pull/1197 is merged
-spec action(atom(), on | off, la_machine_state:state()) -> action().
action(_WakeupCause, on, State) ->
    LastPlayTime = la_machine_state:get_last_play_time(State),
    LastPlaySeq = la_machine_state:get_last_play_seq(State),
    PlayIndex = la_machine_state:get_play_index(State),
    {play, LastPlayTime, LastPlaySeq, PlayIndex};
action(sleep_wakeup_gpio, off, _State) ->
    setup;
action(sleep_wakeup_timer, _ButtonState, State) ->
    {poke, la_machine_state:get_poke_index(State)};
action(undefined, _ButtonState, _State) ->
    setup.

poke() ->
    LEDCChannel = init_servo(),

    % Ensure servo is at slightly open angle
    write_angle(?SERVO_SLIGHTLY_OPEN_ANGLE, LEDCChannel),
    timer:sleep(150),
    ledc:stop(?LEDC_MODE, ?LEDC_CHANNEL, 0),

    % Ensure servo is at closed angle
    write_angle(?SERVO_CLOSED_ANGLE, LEDCChannel),
    timer:sleep(150),
    ledc:stop(?LEDC_MODE, ?LEDC_CHANNEL, 0),

    ok.

play(_LastPlayTime, _LastPlayIndex, _PlayIndex) ->
    % Reset pin (turn on MAX98357A)
    %   gpio:hold_dis(?MAX_SD_MODE_PIN),
    %   gpio:set_pin_mode(?MAX_SD_MODE_PIN, input),
    %   gpio:set_pin_pull(?MAX_SD_MODE_PIN, floating),

    LEDCChannel = init_servo(),

    % Ensure servo is at closed angle
    %   write_angle(?SERVO_CLOSED_ANGLE, LEDCChannel),
    %   timer:sleep(300),
    %   ledc:stop(?LEDC_MODE, ?LEDC_CHANNEL, 0),

    % Ensure servo is at slightly open angle
    write_angle(?SERVO_SLIGHTLY_OPEN_ANGLE, LEDCChannel),
    timer:sleep(150),
    ledc:stop(?LEDC_MODE, ?LEDC_CHANNEL, 0),

    play_aac("gears/17243.aac"),

    play_aac("hits/party-blower-fail-soundroll-1-1-00-01.aac"),
    write_angle(?SERVO_INTERRUPT_ANGLE, LEDCChannel),
    timer:sleep(300),
    ledc:stop(?LEDC_MODE, ?LEDC_CHANNEL, 0),

    write_angle(?SERVO_CLOSED_ANGLE, LEDCChannel),
    timer:sleep(300),
    ledc:stop(?LEDC_MODE, ?LEDC_CHANNEL, 0).

go_to_sleep(SleepSecs) ->
    % Turn down MAX98357A
    %   gpio:set_pin_mode(?MAX_SD_MODE_PIN, output),
    %   gpio:digital_write(?MAX_SD_MODE_PIN, 0),
    %   gpio:hold_en(?MAX_SD_MODE_PIN),

    % Turn down servo
    %   gpio:digital_write(?MOSFET_GPIO, 0),

    gpio:deep_sleep_hold_en(),
    SleepMs = SleepSecs * 1000,
    esp:deep_sleep(SleepMs).

write_angle(Angle, ChannelConfig) ->
    Duty = angle_to_duty(Angle),
    SpeedMode = proplists:get_value(speed_mode, ChannelConfig),
    Channel = proplists:get_value(channel, ChannelConfig),
    ok = ledc:set_duty(SpeedMode, Channel, Duty),
    ok = ledc:update_duty(SpeedMode, Channel).

angle_to_duty(Angle) ->
    AngleUS =
        (Angle / ?SERVO_MAX_ANGLE) * (?SERVO_MAX_WIDTH_US - ?SERVO_MIN_WIDTH_US) +
            ?SERVO_MIN_WIDTH_US,
    Duty = AngleUS * ?SERVO_MAX_DUTY / ?SERVO_FREQ_PERIOD_US,
    floor(Duty).

compute_sleep_timer(State) ->
    <<RandHour:64>> = crypto:strong_rand_bytes(8),
    <<RandSec:64>> = crypto:strong_rand_bytes(8),
    compute_sleep_timer(RandHour, RandSec, State).

compute_sleep_timer(RandHour, RandSec, State) ->
    NextPokeIndex = la_machine_state:get_poke_index(State),
    PlayHoursCount = la_machine_state:get_play_hours_count(State),
    PokeHour =
        case PlayHoursCount of
            0 ->
                0;
            N ->
                la_machine_state:get_play_hour(RandHour rem N, State)
        end,
    % starts with 24 hours followed by 48 hours
    PokeBase = fib(2 + NextPokeIndex) * 24,
    PokeTime = PokeHour + PokeBase,
    PokeTime * 3600 + (RandSec rem 3600) - 1800.

fib(X) -> fib(X, 0, 1).

fib(1, _A, B) -> B;
fib(I, A, B) -> fib(I - 1, B, A + B).

-ifdef(TEST).
action_test_() ->
    [
        ?_assertEqual({poke, 0}, action(sleep_wakeup_timer, off, la_machine_state:new())),
        ?_assertEqual(setup, action(undefined, off, la_machine_state:new())),
        ?_assertEqual(
            {play, 0, undefined, 0}, action(sleep_wakeup_timer, on, la_machine_state:new())
        ),
        ?_assertEqual(
            {play, 0, undefined, 0}, action(sleep_wakeup_gpio, on, la_machine_state:new())
        ),
        ?_assertEqual({play, 0, undefined, 0}, action(undefined, on, la_machine_state:new()))
    ].

fib_test_() ->
    [
        ?_assertEqual(1, fib(1)),
        ?_assertEqual(1, fib(2)),
        % 2 days
        ?_assertEqual(2, fib(3)),
        % 3 days
        ?_assertEqual(3, fib(4)),
        % 5 days
        ?_assertEqual(5, fib(5)),
        % 8 days, etc.
        ?_assertEqual(8, fib(6))
    ].

compute_sleep_timer_test_() ->
    [
        {
            "La Machine pokes 24 hours after reboot",
            ?_assertEqual(24 * 3600, compute_sleep_timer(0, 1800, la_machine_state:new()))
        },
        {
            "La Machine pokes between 23.5 and 24.5 hours after reboot",
            [
                ?_assert(compute_sleep_timer(la_machine_state:new()) >= 23 * 3600 + 1800),
                ?_assert(compute_sleep_timer(la_machine_state:new()) =< 24 * 3600 + 1800)
            ]
        },
        {
            "La Machine pokes 48 hours after first poke",
            ?_assertEqual(
                48 * 3600,
                compute_sleep_timer(
                    0, 1800, la_machine_state:set_poke_index(1, la_machine_state:new())
                )
            )
        },
        {
            "La Machine pokes 24 hours after first play",
            ?_assertEqual(
                24 * 3600,
                compute_sleep_timer(
                    0, 1800, la_machine_state:append_play(1, 0, la_machine_state:new())
                )
            )
        }
    ].
-endif.
