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
%% @doc La Machine accelerometer interface
%% @end
%%-----------------------------------------------------------------------------
-module(la_machine_lis3dh).

-include("la_machine_definitions.hrl").

-ifdef(ACC_IRQ_GPIO).

-define(WHO_AM_I, 16#0F).
-define(CTRL_REG1, 16#20).
-define(CTRL_REG2, 16#21).
-define(CTRL_REG3, 16#22).
-define(CTRL_REG4, 16#23).
-define(CTRL_REG5, 16#24).
-define(CTRL_REG6, 16#25).
-define(REFEFENCE_REG, 16#26).
-define(STATUS_REG, 16#27).
-define(OUT_X_L, 16#28).
-define(OUT_X_H, 16#29).
-define(OUT_Y_L, 16#2A).
-define(OUT_Y_H, 16#2B).
-define(OUT_Z_L, 16#2C).
-define(OUT_Z_H, 16#2D).
-define(FIFO_CTRL_REG, 16#2E).
-define(FIFO_SRC_REG, 16#2F).
-define(INT1_CFG, 16#30).
-define(INT1_SRC, 16#31).
-define(INT1_THS, 16#32).
-define(INT1_DURATION, 16#33).
-define(INT2_CFG, 16#34).
-define(INT2_SRC, 16#35).
-define(INT2_THS, 16#36).
-define(INT2_DURATION, 16#37).

-define(AUTO_INCREMENT, 16#80).

% Accelerometer interruptions
% We configure either inertial interruption, and for simplicity we use
% either INT1 or INT2.
% If La Machine is resting, we enable INT1 interruption to find out if we
% were moved. This interruption is triggered any of the two of the non-Earth
% gravity directions go beyond a given threshold (400 mg).
% If La Machine is not resting, we enable INT2 interruption to find out if we
% are resting. This interruption is configured to match one of the 6 direction
% postion recognition corresponding to the resting position.
-define(CTRL_REG1_CONFIG_10HZ_LOW_POWER, 2#00101111).
-define(CTRL_REG3_CONFIG_ENABLE_IA1, 2#01000000).
-define(CTRL_REG3_CONFIG_ENABLE_IA2, 2#00100000).
-define(CTRL_REG5_CONFIG_LATCH_INTERRUPTS, 2#00001010).

-define(INT1_CFG_OR_ZH_YL_XH, 2#00100110).
-define(INT1_THS_400_MG, (400 div 16)).
-define(INT1_DURATION_1, 1).
-define(INT2_CFG_6D_POS_RECOGNITION_RESTING, 2#11011001).
-define(INT2_THS_300_MG, (300 div 16)).

-define(INT2_DURATION_1, 1).

-export([
    setup/0
]).

% resting position :
% X : low, Y : high, positive, Z : low

-spec setup() -> ok | {play, meuh} | not_resting | replaced.
setup() ->
    I2C = i2c:open([{scl, ?I2C_SCL_GPIO}, {sda, ?I2C_SDA_GPIO}, {clock_speed_hz, 400000}]),
    % Read configuration to figure out the state.
    {ok, ConfigFirstPart} = i2c:read_bytes(
        I2C, ?LIS3DH_ADDR, ?CTRL_REG1 bor ?AUTO_INCREMENT, ?INT1_CFG - ?CTRL_REG1
    ),
    {ok, ConfigSecondPart} = i2c:read_bytes(
        I2C, ?LIS3DH_ADDR, ?INT1_CFG bor ?AUTO_INCREMENT, 1 + ?INT2_DURATION - ?INT1_CFG
    ),
    case <<ConfigFirstPart/binary, ConfigSecondPart/binary>> of
        <<
            ?CTRL_REG1_CONFIG_10HZ_LOW_POWER,
            0,
            ?CTRL_REG3_CONFIG_ENABLE_IA1,
            0,
            ?CTRL_REG5_CONFIG_LATCH_INTERRUPTS,
            0,
            0,
            _StatusReg,
            _XL,
            XH/signed,
            _YL,
            YH/signed,
            _ZL,
            ZH/signed,
            0,
            _FifoSrc,
            ?INT1_CFG_OR_ZH_YL_XH,
            Int1Src,
            ?INT1_THS_400_MG,
            ?INT1_DURATION_1,
            ?INT2_CFG_6D_POS_RECOGNITION_RESTING,
            _Int2Src,
            ?INT2_THS_300_MG,
            ?INT2_DURATION_1
        >> ->
            setup_int1_enabled(I2C, XH, YH, ZH, Int1Src);
        <<
            ?CTRL_REG1_CONFIG_10HZ_LOW_POWER,
            0,
            ?CTRL_REG3_CONFIG_ENABLE_IA2,
            0,
            ?CTRL_REG5_CONFIG_LATCH_INTERRUPTS,
            0,
            0,
            _StatusReg,
            _XL,
            XH/signed,
            _YL,
            YH/signed,
            _ZL,
            ZH/signed,
            0,
            _FifoSrc,
            ?INT1_CFG_OR_ZH_YL_XH,
            _Int1Src,
            ?INT1_THS_400_MG,
            ?INT1_DURATION_1,
            ?INT2_CFG_6D_POS_RECOGNITION_RESTING,
            Int2Src,
            ?INT2_THS_300_MG,
            ?INT2_DURATION_1
        >> ->
            setup_int2_enabled(I2C, XH, YH, ZH, Int2Src);
        _OtherConfig ->
            setup_configure(I2C)
    end.

setup_configure(I2C) ->
    % Ensure boot is complete
    timer:sleep(5),
    i2c:write_bytes(
        I2C, ?LIS3DH_ADDR, ?CTRL_REG1 bor ?AUTO_INCREMENT, <<?CTRL_REG1_CONFIG_10HZ_LOW_POWER, 0>>
    ),
    {ok, <<?CTRL_REG1_CONFIG_10HZ_LOW_POWER, 0>>} = i2c:read_bytes(
        I2C, ?LIS3DH_ADDR, ?CTRL_REG1 bor ?AUTO_INCREMENT, 2
    ),

    i2c:write_bytes(
        I2C,
        ?LIS3DH_ADDR,
        ?CTRL_REG4 bor ?AUTO_INCREMENT,
        <<0, ?CTRL_REG5_CONFIG_LATCH_INTERRUPTS, 0, 0>>
    ),
    {ok, <<0, ?CTRL_REG5_CONFIG_LATCH_INTERRUPTS, 0, 0>>} = i2c:read_bytes(
        I2C, ?LIS3DH_ADDR, ?CTRL_REG4 bor ?AUTO_INCREMENT, 4
    ),

    i2c:write_bytes(I2C, ?LIS3DH_ADDR, ?FIFO_CTRL_REG, <<0>>),
    i2c:write_bytes(I2C, ?LIS3DH_ADDR, ?INT1_CFG, <<?INT1_CFG_OR_ZH_YL_XH>>),

    {ok, <<?INT1_CFG_OR_ZH_YL_XH>>} = i2c:read_bytes(I2C, ?LIS3DH_ADDR, ?INT1_CFG, 1),

    i2c:write_bytes(
        I2C,
        ?LIS3DH_ADDR,
        ?INT1_THS bor ?AUTO_INCREMENT,
        <<?INT1_THS_400_MG, ?INT1_DURATION_1, ?INT2_CFG_6D_POS_RECOGNITION_RESTING>>
    ),
    i2c:write_bytes(
        I2C, ?LIS3DH_ADDR, ?INT2_THS bor ?AUTO_INCREMENT, <<?INT2_THS_300_MG, ?INT2_DURATION_1>>
    ),

    % read orientation
    {ok, <<XH/signed, _YL, YH/signed, _ZL, ZH/signed>>} = i2c:read_bytes(
        I2C, ?LIS3DH_ADDR, ?OUT_X_H bor ?AUTO_INCREMENT, 1 + ?OUT_Z_H - ?OUT_X_H
    ),
    if
        ?LIS3DH_RESTING(XH, YH, ZH) ->
            i2c:write_bytes(I2C, ?LIS3DH_ADDR, ?CTRL_REG3, <<?CTRL_REG3_CONFIG_ENABLE_IA1>>),
            ok;
        true ->
            i2c:write_bytes(I2C, ?LIS3DH_ADDR, ?CTRL_REG3, <<?CTRL_REG3_CONFIG_ENABLE_IA2>>),
            not_resting
    end.

setup_int1_enabled(I2C, _X, _Y, _Z, Int1Src) when Int1Src band 16#40 =/= 0 ->
    % INT1 was generated.
    % Detect meuh movement.
    case detect_meuh(I2C, start, 30) of
        meuh ->
            {play, meuh};
        replaced ->
            replaced;
        timeout ->
            % Use interrupt to detect resting positiong
            i2c:write_bytes(I2C, ?LIS3DH_ADDR, ?CTRL_REG3, <<?CTRL_REG3_CONFIG_ENABLE_IA2>>),
            not_resting;
        not_meuh ->
            ok
    end;
setup_int1_enabled(_I2C, _X, _Y, _Z, _Int1Src) ->
    ok.

setup_int2_enabled(I2C, X, Y, Z, Int2Src) when Int2Src band 16#40 =/= 0 ->
    % INT2 was generated.
    % Detect if we are indeed resting.
    if
        ?LIS3DH_RESTING(X, Y, Z) ->
            % Use interrupt to detect movement
            i2c:write_bytes(I2C, ?LIS3DH_ADDR, ?CTRL_REG3, <<?CTRL_REG3_CONFIG_ENABLE_IA1>>),
            io:format("Replaced after long move\n"),
            replaced;
        true ->
            not_resting
    end;
setup_int2_enabled(_I2C, _X, _Y, _Z, _Int2Src) ->
    ok.

detect_meuh(_I2C, _State, 0) ->
    timeout;
detect_meuh(I2C, State, Steps) ->
    {ok, <<XH/signed, _YL, YH/signed, _ZL, ZH/signed>>} = i2c:read_bytes(
        I2C, ?LIS3DH_ADDR, ?OUT_X_H bor ?AUTO_INCREMENT, 1 + ?OUT_Z_H - ?OUT_X_H
    ),
    MeuhZone = ?LIS3DH_MEUH_ZONE(_XH, YH, _ZH),
    NewState =
        case {State, MeuhZone} of
            {start, neg} -> neg_detected;
            {start, _} -> start;
            {neg_detected, pos} -> pos_detected;
            {neg_detected, _} -> neg_detected;
            {pos_detected, neg} -> not_meuh;
            {pos_detected, _} -> pos_detected;
            {not_meuh, _} -> not_meuh
        end,
    if
        ?LIS3DH_RESTING(XH, YH, ZH) andalso MeuhZone =:= pos ->
            case NewState of
                pos_detected ->
                    meuh;
                start ->
                    if
                        % replaced vertically after 10*90ms without having been upside down
                        Steps < 20 -> replaced;
                        true -> not_meuh
                    end;
                _ ->
                    not_meuh
            end;
        true ->
            timer:sleep(90),
            detect_meuh(I2C, NewState, Steps - 1)
    end.

-endif.
