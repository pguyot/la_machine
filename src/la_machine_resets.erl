%
% This file is part of La Machine
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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
%% @doc La Machine reset log.
%% Writes one 6-byte record per boot into a dedicated 256 KB flash partition.
%% Records are written sequentially; when the partition is full it is erased
%% once and writing restarts, incrementing a 4-byte erase counter in the
%% partition header.
%%
%% Record layout (6 bytes):
%%   bytes 0-3  Unix timestamp (uint32 big-endian, seconds)
%%   byte  4    [button | battery_7bits]
%%                bit  7    button state (0=off, 1=on)
%%                bits 6-0  battery level 0-100, 126 when unknown, 127 when charging
%%   byte  5    [reset_reason | wakeup_cause | wakeup_state]
%%                bits 7-4  reset reason  (see reset_reason_to_int/1)
%%                bit  3    wakeup cause  (0=not-gpio, 1=gpio)
%%                bits 2-0  wakeup state  (0=normal, 1=provisioning, 2=charging, 3=travel, 4=factory)
%% @end
%%-----------------------------------------------------------------------------
-module(la_machine_resets).

-export([log_reset/5]).

-define(PARTITION, <<"resets">>).
-define(PARTITION_SIZE, 16#40000).
-define(HEADER_SIZE, 4).
-define(RECORD_SIZE, 6).
-define(MAX_RECORDS, ((?PARTITION_SIZE - ?HEADER_SIZE) div ?RECORD_SIZE)).
-define(BATTERY_LEVEL_UNKNOWN, 126).
-define(BATTERY_LEVEL_CHARGING, 127).

%%-----------------------------------------------------------------------------
%% @doc Log the current reset into the resets partition.
%% For deep-sleep wakeups, call at the end of the run (before sleeping).
%% For all other reset reasons, call at the beginning of the run.
%% @end
%%-----------------------------------------------------------------------------
-spec log_reset(
    WakeupCause :: undefined | sleep_wakeup_gpio | sleep_wakeup_timer,
    ButtonState :: on | off,
    BatteryLevel :: 0..100,
    IsCharging :: boolean(),
    WakeupState :: la_machine_state:wakeup_state()
) -> ok.
log_reset(WakeupCause, ButtonState, BatteryLevel, IsCharging, WakeupState) ->
    Timestamp = erlang:system_time(second),
    ResetReason = esp:reset_reason(),
    BatteryByte = encode_battery(BatteryLevel, IsCharging, ButtonState),
    StatusByte = encode_status(ResetReason, WakeupCause, WakeupState),
    Record = <<Timestamp:32, BatteryByte:8, StatusByte:8>>,
    {EraseCount, WriteIndex} = find_write_position(),
    case WriteIndex >= ?MAX_RECORDS of
        true ->
            NewEraseCount = EraseCount + 1,
            ok = esp:partition_erase_range(?PARTITION, 0, ?PARTITION_SIZE),
            ok = esp:partition_write(?PARTITION, 0, <<NewEraseCount:32>>),
            ok = esp:partition_write(?PARTITION, ?HEADER_SIZE, Record);
        false ->
            ok = esp:partition_write(?PARTITION, ?HEADER_SIZE + WriteIndex * ?RECORD_SIZE, Record)
    end.

-spec find_write_position() -> {EraseCount :: non_neg_integer(), WriteIndex :: non_neg_integer()}.
find_write_position() ->
    EraseCount =
        case esp:partition_read(?PARTITION, 0, ?HEADER_SIZE) of
            {ok, <<16#FF, 16#FF, 16#FF, 16#FF>>} -> 0;
            {ok, <<Count:32>>} -> Count;
            error -> 0
        end,
    WriteIndex = find_first_empty(0, ?MAX_RECORDS),
    {EraseCount, WriteIndex}.

% Binary search for the first slot whose timestamp bytes are all 0xFF (erased).
-spec find_first_empty(Lo :: non_neg_integer(), Hi :: non_neg_integer()) -> non_neg_integer().
find_first_empty(Lo, Hi) when Lo >= Hi ->
    Lo;
find_first_empty(Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    Offset = ?HEADER_SIZE + Mid * ?RECORD_SIZE,
    case esp:partition_read(?PARTITION, Offset, 4) of
        {ok, <<16#FF, 16#FF, 16#FF, 16#FF>>} -> find_first_empty(Lo, Mid);
        {ok, _} -> find_first_empty(Mid + 1, Hi);
        error -> Hi
    end.

-spec encode_battery(
    BatteryLevel :: 0..100 | undefined, IsCharging :: boolean(), ButtonState :: on | off
) -> 0..255.
encode_battery(BatteryLevel, IsCharging, ButtonState) ->
    BatteryBits =
        case IsCharging of
            true -> ?BATTERY_LEVEL_CHARGING;
            false when BatteryLevel =:= undefined -> ?BATTERY_LEVEL_UNKNOWN;
            false -> BatteryLevel
        end,
    ButtonBit =
        case ButtonState of
            on -> 1;
            off -> 0
        end,
    (ButtonBit bsl 7) bor BatteryBits.

-spec encode_status(
    ResetReason :: atom(),
    WakeupCause :: undefined | sleep_wakeup_gpio | sleep_wakeup_timer,
    WakeupState :: la_machine_state:wakeup_state()
) -> 0..255.
encode_status(ResetReason, WakeupCause, WakeupState) ->
    (reset_reason_to_int(ResetReason) bsl 4) bor
        (wakeup_cause_to_int(WakeupCause) bsl 3) bor
        wakeup_state_to_int(WakeupState).

-spec reset_reason_to_int(atom()) -> 0..10.
reset_reason_to_int(esp_rst_poweron) -> 1;
reset_reason_to_int(esp_rst_ext) -> 2;
reset_reason_to_int(esp_rst_sw) -> 3;
reset_reason_to_int(esp_rst_panic) -> 4;
reset_reason_to_int(esp_rst_int_wdt) -> 5;
reset_reason_to_int(esp_rst_task_wdt) -> 6;
reset_reason_to_int(esp_rst_wdt) -> 7;
reset_reason_to_int(esp_rst_deepsleep) -> 8;
reset_reason_to_int(esp_rst_brownout) -> 9;
reset_reason_to_int(esp_rst_sdio) -> 10;
reset_reason_to_int(_) -> 0.

-spec wakeup_cause_to_int(undefined | sleep_wakeup_gpio | sleep_wakeup_timer) -> 0..1.
wakeup_cause_to_int(sleep_wakeup_gpio) -> 1;
wakeup_cause_to_int(_) -> 0.

-spec wakeup_state_to_int(la_machine_state:wakeup_state()) -> 0..4.
wakeup_state_to_int(normal) -> 0;
wakeup_state_to_int(provisioning) -> 1;
wakeup_state_to_int(charging) -> 2;
wakeup_state_to_int(travel) -> 3;
wakeup_state_to_int(factory) -> 4.
