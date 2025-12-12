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

-define(CTRL_REG1_CONFIG_10HZ_LOW_POWER, 2#00101111).
-define(CTRL_REG3_CONFIG_ENABLE_IA1, 2#01000000).
-define(CTRL_REG3_CONFIG_ENABLE_IA2, 2#00100000).
-define(CTRL_REG5_CONFIG_LATCH_INTERRUPTS, 2#00001010).
-define(INT1_CFG_OR_ZH_XH, 2#00100010).
-define(INT1_THS_300_MG, ?ACCEL_REST_VALUE).
-define(INT1_DURATION_1, 1).
-define(INT2_CFG_AND_ZL_YH_XL, 2#10011001).
-define(INT2_THS_300_MG, ?ACCEL_REST_VALUE).
-define(INT2_DURATION_1, 1).

-export([
    setup/0
]).

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
            ?INT1_CFG_OR_ZH_XH,
            Int1Src,
            ?INT1_THS_300_MG,
            ?INT1_DURATION_1,
            ?INT2_CFG_AND_ZL_YH_XL,
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
            ?INT1_CFG_OR_ZH_XH,
            _Int1Src,
            ?INT1_THS_300_MG,
            ?INT1_DURATION_1,
            ?INT2_CFG_AND_ZL_YH_XL,
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
    i2c:write_bytes(I2C, ?LIS3DH_ADDR, ?INT1_CFG, <<?INT1_CFG_OR_ZH_XH>>),

    {ok, <<?INT1_CFG_OR_ZH_XH>>} = i2c:read_bytes(I2C, ?LIS3DH_ADDR, ?INT1_CFG, 1),

    i2c:write_bytes(
        I2C,
        ?LIS3DH_ADDR,
        ?INT1_THS bor ?AUTO_INCREMENT,
        <<?INT1_THS_300_MG, ?INT1_DURATION_1, ?INT2_CFG_AND_ZL_YH_XL>>
    ),
    i2c:write_bytes(
        I2C, ?LIS3DH_ADDR, ?INT2_THS bor ?AUTO_INCREMENT, <<?INT2_THS_300_MG, ?INT2_DURATION_1>>
    ),

    % read orientation
    {ok, <<XH/signed, _YL, YH/signed, _ZL, ZH/signed>>} = i2c:read_bytes(
        I2C, ?LIS3DH_ADDR, ?OUT_X_H bor ?AUTO_INCREMENT, 1 + ?OUT_Z_H - ?OUT_X_H
    ),
    Resting = restingp(XH, YH, ZH),
    if
        Resting == yes ->
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
    Resting = restingp(X, Y, Z),
    if
        Resting == yes ->
            % Use interrupt to detect movement
            i2c:write_bytes(I2C, ?LIS3DH_ADDR, ?CTRL_REG3, <<?CTRL_REG3_CONFIG_ENABLE_IA1>>),
            io:format("Replaced after long move\n"),
            replaced;
        true ->
            not_resting
    end;
setup_int2_enabled(_I2C, _X, _Y, _Z, _Int2Src) ->
    ok.

% resting position :
% X : low, Y : high, Z : low
restingp(XH, YH, ZH) ->
    if
        XH =< ?ACCEL_REST_VALUE andalso XH >= -?ACCEL_REST_VALUE andalso YH >= ?ACCEL_REST_VALUE andalso
            ZH =< ?ACCEL_REST_VALUE andalso ZH >= -?ACCEL_REST_VALUE -> yes;
        true -> no
    end.

detect_meuh(_I2C, _State, 0) ->
    timeout;
detect_meuh(I2C, State, Steps) ->
    {ok, <<XH/signed, _YL, YH/signed, _ZL, ZH/signed>>} = i2c:read_bytes(
        I2C, ?LIS3DH_ADDR, ?OUT_X_H bor ?AUTO_INCREMENT, 1 + ?OUT_Z_H - ?OUT_X_H
    ),
    io:format("Accelerometer X : ~B ; Y : ~B ; Z : ~B\n", [
        XH, YH, ZH
    ]),
    YHZone =
        if
            YH =< -(700 div 16) -> neg;
            YH >= 700 div 16 -> pos;
            true -> low
        end,
    NewState =
        case {State, YHZone} of
            {start, neg} -> neg_detected;
            {start, _} -> start;
            {neg_detected, pos} -> pos_detected;
            {neg_detected, _} -> neg_detected;
            {pos_detected, neg} -> not_meuh;
            {pos_detected, _} -> pos_detected;
            {not_meuh, _} -> not_meuh
        end,
    Resting = restingp(XH, YH, ZH),
    if
        Resting == yes ->
        io:format("Resting\n"),
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
