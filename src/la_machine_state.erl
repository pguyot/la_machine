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
%% @doc State of La Machine as stored in RTC slow Memory
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine_state).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("la_machine_definitions.hrl").

% State persistence
-export([
    load_state/0,
    save_state/1
]).

% Accessor to state
-export([
    get_poke_index/1,
    get_last_play_time/1,
    get_last_play_seq/1,
    get_play_hours_count/1,
    get_play_index/1,
    get_play_hour/2
]).

% Modifiers
-export([
    append_play/3,
    set_poke_index/2
]).

-export_type([
    state/0
]).

% Constructor for tests
-ifdef(TEST).
-export([
    new/0
]).
-endif.

-record(state, {
    % 64 bit erlang:system_time(second)
    boot_time = 0 :: non_neg_integer(),
    % idem, used for sequence plays
    last_play_time = 0 :: non_neg_integer(),
    % index of last played scenario (32 bits), used to:
    % - play next part if we have several parts
    % - avoid playing one twice in a row
    % - or play scenarios sequentially (demo mode)
    last_play_seq = 0 :: non_neg_integer(),
    % index in play or poke sequence (16 bits)
    play_poke_index = 0 :: non_neg_integer(),
    % buffer with hours, appended to, and then rotated
    play_hours = <<>> :: binary()
}).

-type state() :: #state{}.

-define(MAX_PLAY_HOURS, 1000).

load_state() ->
    Now = erlang:system_time(second),
    deserialize_state(Now, get_state()).

save_state(State) ->
    Bin = serialize_state(State),
    ok = esp:rtc_slow_set_binary(Bin).

-spec get_state() -> binary() | undefined.
get_state() ->
    try
        case esp:reset_reason() of
            esp_rst_deepsleep -> esp:rtc_slow_get_binary();
            _ -> undefined
        end
    catch
        error:badarg ->
            undefined
    end.

-spec deserialize_state(non_neg_integer(), binary() | undefined) -> state().
deserialize_state(
    Now, <<BootTime:64, LastPlayTime:64, LastPlaySeq:32, PlayPokeIndex:16, PlayHours/binary>>
) when
    BootTime =< Now andalso byte_size(PlayHours) =< ?MAX_PLAY_HOURS
->
    #state{
        boot_time = BootTime,
        last_play_time = LastPlayTime,
        last_play_seq = LastPlaySeq,
        play_poke_index = PlayPokeIndex,
        play_hours = PlayHours
    };
deserialize_state(Now, _) ->
    #state{
        boot_time = Now,
        last_play_time = 0,
        last_play_seq = 0,
        play_poke_index = 0,
        play_hours = <<>>
    }.

-spec serialize_state(state()) -> binary().
serialize_state(#state{
    boot_time = BootTime,
    last_play_time = LastPlayTime,
    last_play_seq = LastPlaySeq,
    play_poke_index = PlayPokeIndex,
    play_hours = PlayHours
}) ->
    <<BootTime:64, LastPlayTime:64, LastPlaySeq:32, PlayPokeIndex:16, PlayHours/binary>>.

%% Function called after a sequence has been played
append_play(PlaySeqIndex, PlayIndex, #state{boot_time = BootTime, play_hours = PlayHours0} = State) ->
    % Add the play hour
    Now = erlang:system_time(second),
    NowHour = ((Now - BootTime) div 3600) rem 24,
    PlayHours1 = append_hour(NowHour, PlayHours0),
    State#state{
        last_play_time = Now,
        last_play_seq = PlaySeqIndex,
        play_poke_index = PlayIndex,
        play_hours = PlayHours1
    }.

append_hour(Hour, Buffer) when byte_size(Buffer) < ?MAX_PLAY_HOURS ->
    <<Hour, Buffer/binary>>;
append_hour(Hour, Buffer) ->
    SubBuffer = binary:part(Buffer, 0, ?MAX_PLAY_HOURS - 1),
    <<Hour, SubBuffer/binary>>.

set_poke_index(PokeIndex, State) ->
    State#state{play_poke_index = PokeIndex, last_play_time = 0}.

-spec get_last_play_time(state()) -> non_neg_integer().
get_last_play_time(#state{last_play_time = LastPlayTime}) -> LastPlayTime.

-spec get_last_play_seq(state()) -> undefined | non_neg_integer().
get_last_play_seq(#state{last_play_time = 0}) -> undefined;
get_last_play_seq(#state{last_play_seq = LastPlaySeq}) -> LastPlaySeq.

-spec get_poke_index(state()) -> non_neg_integer().
get_poke_index(#state{last_play_time = 0, play_poke_index = PlayPokeIndex}) -> PlayPokeIndex;
get_poke_index(#state{last_play_time = _}) -> 0.

-spec get_play_index(state()) -> non_neg_integer().
get_play_index(#state{last_play_time = 0}) -> 0;
get_play_index(#state{last_play_time = _, play_poke_index = PlayPokeIndex}) -> PlayPokeIndex.

-spec get_play_hours_count(state()) -> non_neg_integer().
get_play_hours_count(#state{play_hours = PlayHours}) -> byte_size(PlayHours).

-spec get_play_hour(non_neg_integer(), state()) -> non_neg_integer().
get_play_hour(Ix, #state{play_hours = PlayHours}) -> binary:at(PlayHours, Ix).

-ifdef(TEST).
-spec new() -> state().
new() -> #state{boot_time = erlang:system_time(second)}.

deserialize_state_test_() ->
    [
        ?_assertEqual(
            #state{boot_time = 42, last_play_time = 0, play_poke_index = 0, play_hours = <<>>},
            deserialize_state(42, undefined)
        ),
        ?_assertEqual(
            #state{boot_time = 42, last_play_time = 0, play_poke_index = 0, play_hours = <<>>},
            deserialize_state(42, <<>>)
        ),
        ?_assertEqual(
            #state{boot_time = 42, last_play_time = 0, play_poke_index = 0, play_hours = <<>>},
            deserialize_state(42, <<0:(2048 * 8)>>)
        ),
        ?_assertEqual(
            #state{
                boot_time = 1,
                last_play_time = 2,
                last_play_seq = 3,
                play_poke_index = 4,
                play_hours = <<>>
            },
            deserialize_state(42, <<1:64, 2:64, 3:32, 4:16>>)
        ),
        ?_assertEqual(
            #state{
                boot_time = 1,
                last_play_time = 2,
                last_play_seq = 3,
                play_poke_index = 4,
                play_hours = <<5>>
            },
            deserialize_state(42, <<1:64, 2:64, 3:32, 4:16, 5>>)
        ),
        ?_assertEqual(
            #state{
                boot_time = 1,
                last_play_time = 2,
                last_play_seq = 3,
                play_poke_index = 4,
                play_hours = <<5, 6>>
            },
            deserialize_state(42, <<1:64, 2:64, 3:32, 4:16, 5, 6>>)
        ),
        ?_assertEqual(
            #state{boot_time = 42, last_play_time = 0, play_poke_index = 0, play_hours = <<>>},
            deserialize_state(42, <<142:64, 2:64, 3:32, 4:16, 5>>)
        )
    ].

serialize_state_test_() ->
    [
        ?_assertMatch(
            <<_:64, 0:64, 0:32, 0:16>>,
            serialize_state(
                new()
            )
        ),
        ?_assertEqual(
            <<1:64, 2:64, 3:32, 4:16>>,
            serialize_state(
                #state{
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<>>
                }
            )
        ),
        ?_assertEqual(
            <<1:64, 2:64, 3:32, 4:16, 5>>,
            serialize_state(
                #state{
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<5>>
                }
            )
        ),
        ?_assertEqual(
            <<1:64, 2:64, 3:32, 4:16, 5, 6>>,
            serialize_state(
                #state{
                    boot_time = 1,
                    last_play_time = 2,
                    last_play_seq = 3,
                    play_poke_index = 4,
                    play_hours = <<5, 6>>
                }
            )
        )
    ].
-endif.
