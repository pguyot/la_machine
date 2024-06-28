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
%% @doc La Machine audio mock interface
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine_audio_mock).

-include_lib("eunit/include/eunit.hrl").

% mock interface
-export([
    new/0,
    expect/3,
    assert_called/2,
    delete/1
]).

% la_machine_audio interface
-export([
    power_on/0,
    power_off/0,
    play/1
]).

-define(MOCK_SERVER_NAME, ?MODULE).

power_on() ->
    mock:call(?MOCK_SERVER_NAME, power_on, []).

power_off() ->
    mock:call(?MOCK_SERVER_NAME, power_off, []).

play(AACFilename) ->
    mock:call(?MOCK_SERVER_NAME, play, [AACFilename]).

new() ->
    mock:new(?MOCK_SERVER_NAME).

delete(MockServer) ->
    mock:delete(MockServer).

expect(MockServer, Function, Callback) ->
    mock:expect(MockServer, Function, Callback).

assert_called(MockServer, Function) ->
    mock:assert_called(MockServer, Function).
