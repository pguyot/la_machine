#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% This file is part of La Machine
%%
%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% SPDX-License-Identifier: Apache-2.0

-define(PARTITION_TABLE_ADDR, 16#8000).
-define(PARTITION_TABLE_SIZE, 16#C00).
-define(PARTITION_ENTRY_SIZE, 32).
-define(PARTITION_MAGIC, 16#50AA).
-define(HEADER_SIZE, 4).
-define(RECORD_SIZE, 6).

main(Args) ->
    Port = parse_args(Args),
    Esptool = find_esptool(),
    {ResetAddr, ResetSize} = read_partition_table(Port, Esptool),
    Bin = read_flash(Port, Esptool, ResetAddr, ResetSize, "/tmp/la_machine_resets.bin"),
    <<EraseCountRaw:32, Records/binary>> = Bin,
    EraseCount =
        case EraseCountRaw of
            16#FFFFFFFF -> 0;
            N -> N
        end,
    io:format(standard_error, "Erase counter: ~w~n", [EraseCount]),
    MaxRecords = (ResetSize - ?HEADER_SIZE) div ?RECORD_SIZE,
    Rows = decode_records(Records, 0, MaxRecords, []),
    print_csv(Rows).

parse_args([Port]) ->
    Port;
parse_args(_) ->
    io:format(standard_error, "Usage: read_resets.escript <port>~n", []),
    halt(1).

read_partition_table(Port, Esptool) ->
    Bin = read_flash(
        Port,
        Esptool,
        ?PARTITION_TABLE_ADDR,
        ?PARTITION_TABLE_SIZE,
        "/tmp/la_machine_partitions.bin"
    ),
    case find_partition(Bin, <<"resets">>) of
        {ok, Addr, Size} ->
            io:format(standard_error, "Found resets partition at 0x~.16b, size 0x~.16b~n", [
                Addr, Size
            ]),
            {Addr, Size};
        not_found ->
            io:format(standard_error, "Error: no 'resets' partition found in partition table~n", []),
            halt(1)
    end.

find_partition(<<>>, _Name) ->
    not_found;
find_partition(
    <<?PARTITION_MAGIC:16/little, _Type:8, _SubType:8, Offset:32/little, Size:32/little,
        NameBin:16/binary, _Flags:32, Rest/binary>>,
    Name
) ->
    EntryName = binary:part(NameBin, 0, name_length(NameBin)),
    case EntryName =:= Name of
        true -> {ok, Offset, Size};
        false -> find_partition(Rest, Name)
    end;
find_partition(<<_:(?PARTITION_ENTRY_SIZE * 8), Rest/binary>>, Name) ->
    find_partition(Rest, Name);
find_partition(_, _Name) ->
    not_found.

name_length(Bin) ->
    name_length(Bin, 0).

name_length(<<0, _/binary>>, Len) -> Len;
name_length(<<>>, Len) -> Len;
name_length(<<_, Rest/binary>>, Len) -> name_length(Rest, Len + 1).

read_flash(Port, Esptool, Addr, Size, TmpFile) ->
    AddrHex = "0x" ++ integer_to_list(Addr, 16),
    SizeHex = "0x" ++ integer_to_list(Size, 16),
    Cmd = Esptool ++ " --port " ++ Port ++ " read_flash " ++ AddrHex ++ " " ++ SizeHex ++ " " ++ TmpFile,
    io:format(standard_error, "Running: ~s~n", [Cmd]),
    Output = os:cmd(Cmd ++ " 2>&1; echo $?"),
    Lines = string:split(string:trim(Output), "\n", all),
    ExitCodeStr = lists:last(Lines),
    case string:to_integer(ExitCodeStr) of
        {0, []} ->
            ok;
        _ ->
            io:format(standard_error, "esptool failed:~n~s~n", [Output]),
            halt(1)
    end,
    case file:read_file(TmpFile) of
        {ok, Bin} -> Bin;
        {error, Reason} ->
            io:format(standard_error, "Failed to read ~s: ~p~n", [TmpFile, Reason]),
            halt(1)
    end.

find_esptool() ->
    EnvOverride = os:getenv("ESPTOOL", ""),
    case EnvOverride =/= "" andalso filelib:is_regular(EnvOverride) of
        true ->
            EnvOverride;
        false ->
            PathCandidates = ["esptool", "esptool.py"],
            case find_in_path(PathCandidates) of
                {ok, Cmd} ->
                    Cmd;
                not_found ->
                    AbsoluteCandidates = [
                        "/usr/local/bin/esptool",
                        "/usr/local/bin/esptool.py",
                        "/usr/bin/esptool",
                        "/usr/bin/esptool.py",
                        os:getenv("HOME", "") ++
                            "/Library/Arduino15/packages/esp32/tools/esptool_py/5.1.0/esptool"
                    ],
                    find_first_existing(AbsoluteCandidates)
            end
    end.

find_in_path([]) ->
    not_found;
find_in_path([Cmd | Rest]) ->
    case os:cmd("command -v " ++ Cmd ++ " 2>/dev/null") of
        "" -> find_in_path(Rest);
        Path -> {ok, string:trim(Path)}
    end.

find_first_existing([]) ->
    io:format(standard_error, "esptool not found. Set ESPTOOL env var.~n", []),
    halt(1);
find_first_existing([H | T]) ->
    case H =/= "" andalso filelib:is_regular(H) of
        true -> H;
        false -> find_first_existing(T)
    end.

decode_records(_Bin, Index, MaxRecords, Acc) when Index >= MaxRecords ->
    lists:reverse(Acc);
decode_records(<<>>, _Index, _MaxRecords, Acc) ->
    lists:reverse(Acc);
decode_records(<<16#FF, 16#FF, 16#FF, 16#FF, _:16, _Rest/binary>>, _Index, _MaxRecords, Acc) ->
    lists:reverse(Acc);
decode_records(<<Timestamp:32, BatteryByte:8, StatusByte:8, Rest/binary>>, Index, MaxRecords, Acc) ->
    Row = decode_record(Index, Timestamp, BatteryByte, StatusByte),
    decode_records(Rest, Index + 1, MaxRecords, [Row | Acc]);
decode_records(_, _Index, _MaxRecords, Acc) ->
    lists:reverse(Acc).

decode_record(Index, Timestamp, BatteryByte, StatusByte) ->
    Button =
        case BatteryByte bsr 7 of
            1 -> "on";
            0 -> "off"
        end,
    BatteryBits = BatteryByte band 16#7F,
    Battery =
        case BatteryBits of
            127 -> "charging";
            N -> integer_to_list(N)
        end,
    ResetReasonInt = StatusByte bsr 4,
    WakeupCauseInt = (StatusByte bsr 3) band 1,
    WakeupStateInt = StatusByte band 7,
    ResetReason = int_to_reset_reason(ResetReasonInt),
    WakeupCause = int_to_wakeup_cause(WakeupCauseInt),
    WakeupState = int_to_wakeup_state(WakeupStateInt),
    {Index, Timestamp, Button, Battery, ResetReason, WakeupCause, WakeupState}.

print_csv(Rows) ->
    io:format("index,timestamp,button,battery,reset_reason,wakeup_cause,wakeup_state~n"),
    lists:foreach(
        fun({Index, Timestamp, Button, Battery, ResetReason, WakeupCause, WakeupState}) ->
            io:format("~w,~w,~s,~s,~s,~s,~s~n", [
                Index,
                Timestamp,
                Button,
                Battery,
                ResetReason,
                WakeupCause,
                WakeupState
            ])
        end,
        Rows
    ).

int_to_reset_reason(0) -> "esp_rst_unknown";
int_to_reset_reason(1) -> "esp_rst_poweron";
int_to_reset_reason(2) -> "esp_rst_ext";
int_to_reset_reason(3) -> "esp_rst_sw";
int_to_reset_reason(4) -> "esp_rst_panic";
int_to_reset_reason(5) -> "esp_rst_int_wdt";
int_to_reset_reason(6) -> "esp_rst_task_wdt";
int_to_reset_reason(7) -> "esp_rst_wdt";
int_to_reset_reason(8) -> "esp_rst_deepsleep";
int_to_reset_reason(9) -> "esp_rst_brownout";
int_to_reset_reason(10) -> "esp_rst_sdio";
int_to_reset_reason(N) -> "unknown_" ++ integer_to_list(N).

int_to_wakeup_cause(1) -> "gpio";
int_to_wakeup_cause(_) -> "other".

int_to_wakeup_state(0) -> "normal";
int_to_wakeup_state(1) -> "provisioning";
int_to_wakeup_state(2) -> "charging";
int_to_wakeup_state(3) -> "travel";
int_to_wakeup_state(4) -> "factory";
int_to_wakeup_state(N) -> "unknown_" ++ integer_to_list(N).
