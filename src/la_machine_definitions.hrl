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

% Hardware definitions
% Prototypes:
% - 20241023 : boîte inutile 1.2-1.3
% - 20260106 : boîte inutile 1.6+

-ifndef(HARDWARE_REVISION).
-define(HARDWARE_REVISION, proto_20260106).
-endif.

%% GPIOs

% La machine uses several GPIO pins that are labelled differently in schema as
% GPIO is not their first function. We define them here.
-define(XTAL_32K_P_GPIO, 0).
-define(XTAL_32K_N_GPIO, 1).
-define(MTMS_GPIO, 4).
-define(MTDO_GPIO, 7).
-define(MTCK_GPIO, 6).
-define(MTDI_GPIO, 5).
-define(U0RXD_GPIO, 20).
-define(U0TXD_GPIO, 21).

-define(SERVO_PWM_GPIO, 10).
-define(SERVO_EN_BOOST_GPIO, ?MTMS_GPIO).

-define(MAX_LRC_GPIO, ?MTDO_GPIO).
-define(MAX_BCLK_GPIO, 8).
-define(MAX_DIN_GPIO, ?MTCK_GPIO).
-define(MAX_SD_MODE_GPIO, ?XTAL_32K_N_GPIO).

-define(BUTTON_GPIO, 3).
-define(BUTTON_GPIO_PULL, down).
-define(BUTTON_GPIO_WAKEUP_LEVEL, 1).
-define(BUTTON_GPIO_OFF, low).
-define(BUTTON_GPIO_ON, high).

-define(BATTERY_LEVEL_GPIO, ?XTAL_32K_P_GPIO).
-define(BATTERY_STAT2_GPIO, ?U0TXD_GPIO).
-define(BATTERY_MV_LOW, 3650).
-define(BATTERY_MV_HIGH, 4110).

-define(ACC_IRQ_GPIO, ?MTDI_GPIO).
-define(I2C_SDA_GPIO, 2).
-define(I2C_SCL_GPIO, ?U0RXD_GPIO).

-if(?HARDWARE_REVISION =:= proto_20260106).
-define(ACC_IRQ_GPIO_PULL, floating).
-else.
-define(ACC_IRQ_GPIO_PULL, down).
-endif.

%% Accelerometer
%% SDA0 is connected to GND
-define(LIS3DH_ADDR, 2#0011000).

-if(?HARDWARE_REVISION =:= proto_20241023 orelse ?HARDWARE_REVISION =:= proto_20260106).
% La Boîte Inutile 1.2-1.4 and 1.6
%
% _____
% |   | U5
% |o__|
%
% X : low, Y : high Z : low
-define(LIS3DH_RESTING(XH, YH, ZH),
    XH =< (300 div 16) andalso XH >= -(300 div 16) andalso
        YH >= (700 div 16) andalso
        ZH =< (300 div 16) andalso ZH >= -(300 div 16)
).
-define(LIS3DH_MEUH_ZONE(_XH, YH, _ZH),
    if
        YH =< -(700 div 16) -> neg;
        YH >= 700 div 16 -> pos;
        true -> low
    end
).
-else.
-error({unsupported_hardware_revision, ?HARDWARE_REVISION}).
-endif.

%% SERVO

-define(LEDC_FADE_NO_WAIT, 0).
-define(LEDC_FADE_WAIT_DONE, 1).
-define(LEDC_FADE_MAX, 2).

-define(LEDC_DUTY_RESOLUTION, 14).
-define(LEDC_TIMER, 0).
% only value supported by esp32c3
-define(LEDC_MODE, 0).
-define(LEDC_CH_GPIO, ?SERVO_PWM_GPIO).
-define(LEDC_CHANNEL, 0).

-if(?HARDWARE_REVISION =:= proto_20241023).
-define(DEFAULT_SERVO_CLOSED_DUTY, 864).
-define(DEFAULT_SERVO_INTERRUPT_DUTY, 1911).
-elif(?HARDWARE_REVISION =:= proto_20260106).
-define(DEFAULT_SERVO_CLOSED_DUTY, 1547).
-define(DEFAULT_SERVO_INTERRUPT_DUTY, 682).
-else.
-error({unsupported_hardware_revision, ?HARDWARE_REVISION}).
-endif.

-define(SERVO_MAX_WIDTH_US, 2500).
-define(SERVO_MIN_WIDTH_US, 500).
-define(SERVO_FREQ_HZ, 50).
-define(SERVO_FREQ_PERIOD_US, (1000000 / ?SERVO_FREQ_HZ)).
-define(SERVO_MAX_DUTY, ((1 bsl ?LEDC_DUTY_RESOLUTION) - 1)).
% Time for full servo range in ms. Servo is 0.3s/60°, full 180° = 900ms.
-define(SERVO_FULL_RANGE_TIME_MS, 900).

% Maximum run time. If La machine runs in more than this, watchdog is triggered,
% La machine panics and state stored in RTC Slow memory is ignored on next boot.
-define(WATCHDOG_TIMEOUT_MS, 60000).

%% Moods

% Travel Mode = always paused, no action
-define(TRAVELMODE, 0).

% Delay to start calling after interaction
-define(CALLING_START_DELAY_S, 15).
% Min delay to continue calling
-define(CALLING_MIN_DELAY_S, 10).
% Max delay to continue calling
-define(CALLING_MAX_DELAY_S, 25).
% MAX number of callings
-define(MAX_CALLING_SOUNDS, 3).


% MIN number of gestures in mood joy
-define(JOY_MIN_GESTURES, 1).
% Chances to change mood : joy => calm
-define(JOY_CALM_CHANCE, 2). % one chance out of 2
% MIN number of gestures in a mood (calm, imitation, dialectic, tired, excited, upset)
-define(MOOD_MIN_GESTURES, 3).
% Chances to change mood : upset/tired/excited/imitation/dialectic => calm (otherwise stay in same)
-define(MOODY_CALM_CHANCE, 2). % one chance out of 2
  
% Probabilities after calm
% all probabilities are added
% here 10 possibilities : 5 to stay calm, 1 to go to imitation, 1 to go to dialectic, 1 to go to upset, 1 to go to tired, 1 to go to excited
% Probability to change mood : calm => calm
-define(CALM_CALM_PROBA, 5).
% Probability to change mood : calm => imitation
-define(CALM_IMIT_PROBA, 1).
% Probability to change mood : calm => dialectic
-define(CALM_DIAL_PROBA, 1).
% Probability to change mood : calm => upset
-define(CALM_UPSET_PROBA, 1).
% Probability to change mood : calm => tired
-define(CALM_TIRED_PROBA, 1).
% Probability to change mood : calm => excited
-define(CALM_EXCITED_PROBA, 1).

% Max duration of short gestures in games mood
-define(GAME_SHORT_DUR_S, 2).
% Max duration of medium gestures in games mood
-define(GAME_MEDIUM_DUR_S, 4).

%% DEBUG
% play only one mood (to debug)
-define(DEBUG_PLAY_ONLY_ONE_MOOD, 0).
% which mood
-define(DEBUG_PLAY_ONLY_ONE_MOOD_MOOD, test).
% which scenario number (starting at 1, -1 means random)
-define(DEBUG_PLAY_ONLY_ONE_MOOD_INDEX, 1).
