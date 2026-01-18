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
%% @doc La Machine servo mock interface
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine_servo_mock).

-include_lib("eunit/include/eunit.hrl").

% mock interface
-export([
    new/0,
    expect/3,
    assert_called/2,
    delete/1
]).

-export([
    power_on/1,
    set_target/2,
    set_target/3,
    timeout/1,
    power_off/0
]).

-define(MOCK_SERVER_NAME, ?MODULE).

power_on(_Config) ->
    mock:call(?MOCK_SERVER_NAME, power_on, []).

set_target(Target, State) ->
    mock:call(?MOCK_SERVER_NAME, set_target, [Target, State]).

set_target(Target, TimeMS, State) ->
    mock:call(?MOCK_SERVER_NAME, set_target, [Target, TimeMS, State]).

timeout(State) ->
    mock:call(?MOCK_SERVER_NAME, timeout, [State]).

power_off() ->
    mock:call(?MOCK_SERVER_NAME, power_off, []).

new() ->
    mock:new(?MOCK_SERVER_NAME).

delete(MockServer) ->
    mock:delete(MockServer).

expect(MockServer, Function, Callback) ->
    mock:expect(MockServer, Function, Callback).

assert_called(MockServer, Function) ->
    mock:assert_called(MockServer, Function).
