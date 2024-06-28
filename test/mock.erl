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
%% @doc Common mock implementation for la_machine tests
%% @end
%%-----------------------------------------------------------------------------

-module(mock).

-include_lib("eunit/include/eunit.hrl").

% mock interface
-export([
    new/1,
    expect/3,
    assert_called/2,
    delete/1,
    call/3
]).

new(MockServerName) ->
    spawn_opt(
        fun() ->
            register(MockServerName, self()),
            mock_loop([], [])
        end,
        [monitor]
    ).

delete({Pid, MonitorRef} = MockServer) ->
    mock_server_call(MockServer, stop),
    receive
        {'DOWN', MonitorRef, process, Pid, Reason} -> ?assertEqual(normal, Reason)
    end.

expect(MockServer, Function, Callback) ->
    mock_server_call(MockServer, {expect, Function, Callback}).

assert_called(MockServer, Function) ->
    Count = mock_server_call(MockServer, {count_calls, Function}),
    ?assert(Count > 0).

mock_loop(Calls, Expectations) ->
    receive
        {Caller, Ref, stop} ->
            Caller ! {Ref, ok};
        {Caller, Ref, {expect, Function, Callback}} ->
            NewExpectations = lists:keystore(Function, 1, Expectations, {Function, Callback}),
            Caller ! {Ref, ok},
            mock_loop(Calls, NewExpectations);
        {Caller, Ref, {call, Function, Args}} ->
            case lists:keyfind(Function, 1, Expectations) of
                false ->
                    Caller ! {Ref, {error, {unexpected, {Function, Args}}}};
                {Function, Callback} when is_function(Callback) ->
                    Caller ! {Ref, {ok, apply(Callback, Args)}};
                {Function, Result} ->
                    Caller ! {Ref, {ok, Result}}
            end,
            mock_loop([Function | Calls], Expectations);
        {Caller, Ref, {count_calls, Function}} ->
            Caller ! {Ref, length([Call || Call <- Calls, Call =:= Function])},
            mock_loop(Calls, Expectations)
    end.

mock_server_call({MockPid, MockMonitorRef}, Message) ->
    Ref = make_ref(),
    MockPid ! {self(), Ref, Message},
    receive
        {Ref, Result} ->
            Result;
        {'DOWN', MockMonitorRef, process, MockPid, _Reason} = DownMessage ->
            ?assertEqual(ok, {unexpected, DownMessage})
    end.

call(MockServerName, Function, Args) ->
    Ref = make_ref(),
    MockServerName ! {self(), Ref, {call, Function, Args}},
    receive
        {Ref, {ok, Result}} -> Result;
        {Ref, {error, _Reason} = ErrorT} -> ?assertEqual(ok, ErrorT)
    end.
