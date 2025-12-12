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
% Prototype 20241023 : boîte inutile 1.2

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
-define(BUTTON_GPIO_HOLD(_), ok).
-define(BUTTON_GPIO_WAKEUP_LEVEL, 1).
-define(BUTTON_GPIO_OFF, low).
-define(BUTTON_GPIO_ON, high).

-define(BATTERY_LEVEL_GPIO, ?XTAL_32K_P_GPIO).
-define(BATTERY_STAT2_GPIO, ?U0TXD_GPIO).

-define(ACC_IRQ_GPIO, ?MTDI_GPIO).
-define(I2C_SDA_GPIO, 2).
-define(I2C_SCL_GPIO, ?U0RXD_GPIO).

%% Accelerometer
%% SDA0 is connected to GND
-define(LIS3DH_ADDR, 2#0011000).

%% SERVO

-define(LEDC_FADE_NO_WAIT, 0).
-define(LEDC_FADE_WAIT_DONE, 1).
-define(LEDC_FADE_MAX, 2).

-define(LEDC_DUTY_RESOLUTION, 13).
-define(LEDC_TIMER, 0).
% only value supported by esp32c3
-define(LEDC_MODE, 0).
-define(LEDC_CH_GPIO, ?SERVO_PWM_GPIO).
-define(LEDC_CHANNEL, 0).

-define(SERVO_CLOSED_ANGLE, 50.0).
%-define(SERVO_SLIGHTLY_OPEN_ANGLE, 50.0).
-define(SERVO_INTERRUPT_ANGLE, 165.0).

-define(SERVO_MAX_ANGLE, 180.0).
-define(SERVO_MAX_WIDTH_US, 2500).
-define(SERVO_MIN_WIDTH_US, 500).
-define(SERVO_FREQ_HZ, 50).
-define(SERVO_FREQ_PERIOD_US, (1000000 / ?SERVO_FREQ_HZ)).
-define(SERVO_MAX_DUTY, ((1 bsl ?LEDC_DUTY_RESOLUTION) - 1)).
% Time required to move for MAX_ANGLE in ms. Servo is 0.3s/60°
-define(SERVO_MAX_ANGLE_TIME_MS, 900).

% Maximum run time. If La machine runs in more than this, watchdog is triggered,
% La machine panics and state stored in RTC Slow memory is ignored on next boot.
-define(WATCHDOG_TIMEOUT_MS, 60000).

%% Moods

% Travel Mode = always paused, no action
-define(TRAVELMODE, 1).

% Should process tripleclick (broken)
-define(TRIPLECLICK, 0).

% Delay to start calling after interaction
- define(CALLING_START_DELAY_S, 15).
% Min delay to continue calling
- define(CALLING_MIN_DELAY_S, 10).
% Max delay to continue calling
- define(CALLING_MAX_DELAY_S, 25).

% MAX number of calling sounds
- define(MAX_CALLING_SOUNDS, 3).

% MIN number of interactions in joy
- define(JOY_MIN_GESTURES, 3).
% Chances to change mood : joy => imitation
- define(JOY_IMIT_CHANCE, 2).

% MIN number of interactions in imitation
- define(IMIT_MIN_GESTURES, 3).
% Chances to change mood : imitation => dialectics
- define(IMIT_DIAL_CHANCE, 3).
% Chances to change mood : imitation => upset
- define(IMIT_UPSET_CHANCE, 5).
% Chances to change mood : imitation => tired
- define(IMIT_TIRED_CHANCE, 5).
% Chances to change mood : imitation => excited
- define(IMIT_EXCITED_CHANCE, 5).

% MIN number of interactions in dialectics
- define(DIAL_MIN_GESTURES, 3).
% Chances to change mood : dialectics => imitation
- define(DIAL_IMIT_CHANCE, 3).
% Chances to change mood : dialectics => upset
- define(DIAL_UPSET_CHANCE, 5).
% Chances to change mood : dialectics => tired
- define(DIAL_TIRED_CHANCE, 5).
% Chances to change mood : dialectics => excited
- define(DIAL_EXCITED_CHANCE, 5).

% Chances to change mood : upset/tired/excited => imitation
- define(MOODY_IMIT_CHANCE, 7).

% Max duration of short gestures in games mood
- define(GAME_SHORT_DUR_S, 2).
% Max duration of medium gestures in games mood
- define(GAME_MEDIUM_DUR_S, 4).

% Sensitivity accelerometer
- define(ACCEL_REST_VALUE, (300 div 16)).

%% DEBUG
% play only one mood (to debug)
- define(DEBUG_PLAY_ONLY_ONE_MOOD, 1).
% which mood
- define(DEBUG_PLAY_ONLY_ONE_MOOD_MOOD, upset).
% which scenario number (staring at 1, -1 means random)
- define(DEBUG_PLAY_ONLY_ONE_MOOD_INDEX, -1).



