%%
%% This file is part of La Machine
%%
%% Copyright 2024-2026 Olivier Mével <olivier@multiplie.fr>
%%                     Paul Guyot <pguyot@kallisys.net>
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
%%

-module(la_machine_scenarios).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    count/1,
    get/2
]).

-export_type([
    scenario/0
]).

-type scenario() :: [scenario_element()].
-type scenario_element() ::
    {servo, non_neg_integer()}
    | {servo, non_neg_integer(), non_neg_integer()}
    | {aac, binary()}
    | {wait, non_neg_integer() | sound}.

-compile(inline).

-spec count(atom()) -> non_neg_integer().
-spec get(atom(), pos_integer()) -> scenario().

%% Generated scenario functions, count/1 and get/2
-include("la_machine_scenarios.hrl").

-ifdef(TEST).
count_test_() ->
    [
        ?_assert(count(excited) > 0),
        ?_assertEqual(0, count(unknown))
    ].

get_test_() ->
    [
        ?_assert(is_list(get(excited, 1)))
    ].
-endif.
