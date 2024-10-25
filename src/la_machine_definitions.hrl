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

%% GPIOs

-if(
    not defined(MODEL_PROTO_PAPER_TOY) andalso not (defined(MODEL_PROTO_LEGO)) andalso
        not (defined(MODEL_PROTO_20240718)) andalso not (defined(MODEL_PROTO_20241023))
).
-define(MODEL_PROTO_20241023, 1).
-endif.

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

% Paper toy prototype : ESP32-C3 Dev board
-ifdef(MODEL_PROTO_PAPER_TOY).
-define(SERVO_PWM_GPIO, 21).

-define(MAX_LRC_GPIO, 7).
-define(MAX_BCLK_GPIO, 8).
-define(MAX_DIN_GPIO, 6).

-define(BUTTON_GPIO, 5).
-define(BUTTON_GPIO_PULL, up).
-define(BUTTON_GPIO_HOLD(_), ok).
-define(BUTTON_GPIO_WAKEUP_LEVEL, 0).
-define(BUTTON_GPIO_OFF, high).
-define(BUTTON_GPIO_ON, low).
-endif.

% Lego prototype : ESP32-C3 Dev board, button is reverted.
-ifdef(MODEL_PROTO_LEGO).
-define(SERVO_PWM_GPIO, 21).

-define(MAX_LRC_GPIO, 7).
-define(MAX_BCLK_GPIO, 8).
-define(MAX_DIN_GPIO, 6).

-define(BUTTON_GPIO, 5).
-define(BUTTON_GPIO_PULL, up).
-define(BUTTON_GPIO_HOLD(F), ok = gpio:F(?BUTTON_GPIO)).
-define(BUTTON_GPIO_WAKEUP_LEVEL, 1).
-define(BUTTON_GPIO_OFF, low).
-define(BUTTON_GPIO_ON, high).
-endif.

% Prototype 20240718 : boîte inutile 1.1
-ifdef(MODEL_PROTO_20240718).
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
-define(BATTERY_STAT1_GPIO, ?U0RXD_GPIO).
-define(BATTERY_STAT2_GPIO, ?U0TXD_GPIO).
-endif.

% Prototype 20241023 : boîte inutile 1.2
-ifdef(MODEL_PROTO_20241023).
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
-endif.

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

-ifdef(MODEL_PROTO_LEGO).
-define(SERVO_CLOSED_ANGLE, 55.0).
-define(SERVO_SLIGHTLY_OPEN_ANGLE, 80.0).
-define(SERVO_INTERRUPT_ANGLE, 180.0).
-else.
-define(SERVO_CLOSED_ANGLE, 25.0).
-define(SERVO_SLIGHTLY_OPEN_ANGLE, 50.0).
-define(SERVO_INTERRUPT_ANGLE, 150.0).
-endif.

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

%% Accelerometer
%% SDA0 is connected to GND
-define(LIS3DH_ADDR, 2#0011000).
