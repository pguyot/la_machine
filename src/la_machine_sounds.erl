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
%% @doc La Machine sounds interface for reading sounds from a partition
%% using a pre-compiled index.
%% @end
%%-----------------------------------------------------------------------------
-module(la_machine_sounds).

-include("la_machine_sounds_index.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    get_sound/1
]).

-define(SOUNDS_PARTITION, <<"sounds">>).

%%-----------------------------------------------------------------------------
%% @doc Get a sound file as an mmap binary from the sounds partition.
%%
%% Looks up the file's offset and size from the pre-compiled index, then
%% memory-maps the data directly from the partition.
%% @end
%%-----------------------------------------------------------------------------
-spec get_sound(Filename :: binary()) -> {ok, binary()} | {error, term()}.
get_sound(Filename) ->
    case maps:get(Filename, ?SOUNDS_INDEX, undefined) of
        undefined ->
            {error, {file_not_found, Filename}};
        {DataOffset, DataSize} ->
            case esp:partition_mmap(?SOUNDS_PARTITION, DataOffset, DataSize) of
                {ok, MappedBinary} ->
                    {ok, MappedBinary};
                error ->
                    {error, partition_mmap_failed}
            end
    end.

-ifdef(TEST).
get_sound_test() ->
    try atomvm:platform() of
        esp32 ->
            {ok, Calling00099} = get_sound("calling/00099.mp3"),
            true = is_binary(Calling00099),
            {error, {file_not_found, _}} = get_sound("calling/not-found"),
            ok;
        generic_unix ->
            ok
    catch
        error:undef ->
            ok
    end.
-endif.
