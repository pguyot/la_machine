%%
%% This file is part of La Machine
%%
%% Copyright 2024-2025 Olivier Mével <olivier@multiplie.fr>
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

-export([
	scenario_joy_0110/0,
	scenario_joy_0177/0,
	scenario_joy_0192/0,
	scenario_joy_0239/0,
	scenario_joy_0295/0,
	scenario_joy_0361/0,
	scenario_joy_0485/0,
	scenario_joy_0499/0,
	scenario_joy_0597/0,
	scenario_joy_0686/0,
	scenario_game_short_0105/0,
	scenario_game_short_0141/0,
	scenario_game_short_0144/0,
	scenario_game_short_0177/0,
	scenario_game_short_0185/0,
	scenario_game_short_0186/0,
	scenario_game_short_0201/0,
	scenario_game_medium_0249/0,
	scenario_game_medium_0269/0,
	scenario_game_medium_0372/0,
	scenario_game_long_05coucoudefil/0,
	scenario_game_long_03bidibidicencorqui/0,
	scenario_upset_silent/0,
	scenario_upset_silent2/0,
	scenario_upset_0101/0,
	scenario_upset_0117/0,
	scenario_upset_0180/0,
	scenario_upset_0189/0,
	scenario_upset_0192/0,
	scenario_upset_0194/0,
	scenario_upset_0247/0,
	scenario_upset_0261/0,
	scenario_upset_0384/0,
	scenario_upset_0585/0,
	scenario_missing_0134/0,
	scenario_missing_0165/0,
	scenario_missing_0187/0,
	scenario_missing_0199/0,
	scenario_missing_0206/0,
	scenario_missing_0245/0,
	scenario_missing_0281/0,
	scenario_missing_0302/0,
	scenario_missing_0381/0,
	scenario_missing_0402/0,
	scenario_meuh_V2/0
    ]).

-export_type([
    scenario/0
]).

-type scenario() :: [scenario_part()].
-type scenario_part() :: [scenario_element()].
-type scenario_element() ::
    {servo, non_neg_integer()}
    | {aac, binary()}
    | {wait, non_neg_integer() | sound}.

%% To save memory and leverage mmap, each scenario is a dedicated exported
%% function, named "scenario_<type>".
%% Suffixes are ignored but scenarios are sorted lexicographically to get
%% ith scenario with get/2.

-spec count(atom()) -> non_neg_integer().
count(Type) ->
    Prefix = "scenario_" ++ atom_to_list(Type),
    PrefixLen = length(Prefix),
    Exports = ?MODULE:module_info(exports),
    lists:foldl(
        fun(Export, AccCount) ->
            case Export of
                {FuncName, 0} ->
                    FuncPrefix = lists:sublist(atom_to_list(FuncName), PrefixLen),
                    if
                        FuncPrefix =:= Prefix -> AccCount + 1;
                        true -> AccCount
                    end;
                {_FuncName, _OtherArity} ->
                    AccCount
            end
        end,
        0,
        Exports
    ).

-spec get(atom(), pos_integer()) -> scenario().
get(Type, Ith) ->
    Prefix = "scenario_" ++ atom_to_list(Type),
    PrefixLen = length(Prefix),
    Exports = ?MODULE:module_info(exports),
    AllOfType = lists:foldl(
        fun(Export, AccL) ->
            case Export of
                {FuncName, 0} ->
                    FuncPrefix = lists:sublist(atom_to_list(FuncName), PrefixLen),
                    if
                        FuncPrefix =:= Prefix -> [FuncName | AccL];
                        true -> AccL
                    end;
                {_FuncName, _OtherArity} ->
                    AccL
            end
        end,
        [],
        Exports
    ),
    Sorted = lists:sort(AllOfType),
    Selected = lists:nth(Ith, Sorted),
    ?MODULE:Selected().

scenario_joy_0110() ->
	[
		[
			{aac, <<"joy/0110.aac">>}, {wait, sound}, {servo, 100}, {wait, 172}, {servo, 0}
		]
	].

scenario_joy_0177() ->
	[
		[
			{aac, <<"joy/0177.aac">>}, {wait, sound}, {servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_joy_0192() ->
	[
		[
			{aac, <<"joy/0192.aac">>}, {wait, sound}, {servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_joy_0239() ->
	[
		[
			{aac, <<"joy/0239.aac">>}, {wait, sound}, {servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_joy_0295() ->
	[
		[
			{aac, <<"joy/0295.aac">>}, {wait, sound}, {servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_joy_0361() ->
	[
		[
			{aac, <<"joy/0362.aac">>}, {wait, sound}, {servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_joy_0485() ->
	[
		[
			{aac, <<"joy/0485.aac">>}, {wait, sound}, {servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_joy_0499() ->
	[
		[
			{aac, <<"joy/0499.aac">>}, {wait, sound}, {servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_joy_0597() ->
	[
		[
			{aac, <<"joy/0597.aac">>}, {wait, sound}, {servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_joy_0686() ->
	[
		[
			{aac, <<"joy/0686.aac">>}, {wait, sound}, {servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_game_short_0105() ->
	[
		[
			{aac, <<"game/0105.aac">>}, {wait, 700}, {servo, 100}, {wait, 387}, {servo, 0}
		]
	].

scenario_game_short_0141() ->
	[
		[
			{aac, <<"game/0141.aac">>}, {wait, 500}, {servo, 100}, {wait, 875}, {servo, 0}
		]
	].

scenario_game_short_0144() ->
	[
		[
			{aac, <<"game/0144.aac">>}, {wait, 1125}, {servo, 100}, {wait, 375}, {servo, 0}
		]
	].

scenario_game_short_0177() ->
	[
		[
			{aac, <<"game/0177.aac">>}, {wait, 100}, {servo, 52}, {wait, 1337}, {servo, 100}, {wait, 388}, {servo, 0}
		]
	].

scenario_game_short_0185() ->
	[
		[
			{aac, <<"game/0185.aac">>}, {wait, 87}, {servo, 58}, {wait, 1013}, {servo, 100}, {wait, 775}, {servo, 0}
		]
	].

scenario_game_short_0186() ->
	[
		[
			{aac, <<"game/0186.aac">>}, {wait, 100}, {servo, 54}, {wait, 237}, {servo, 38}, {wait, 225}, {servo, 62}, {wait, 300}, {servo, 38}, {wait, 175}, {servo, 62}, {wait, 163}, {servo, 100}, {wait, 274}, {servo, 0}
		]
	].

scenario_game_short_0201() ->
	[
		[
			{aac, <<"game/0201.aac">>}, {wait, 50}, {servo, 49}, {wait, 187}, {servo, 49}, {wait, 163}, {servo, 51}, {wait, 150}, {servo, 54}, {wait, 300}, {servo, 54}, {wait, 250}, {servo, 56}, {wait, 125}, {servo, 57}, {wait, 262}, {servo, 35}, {wait, 25}, {servo, 100}, {wait, 488}, {servo, 0}
		]
	].

scenario_game_medium_0249() ->
	[
		[
			{aac, <<"game/0249.aac">>}, {wait, 187}, {servo, 56}, {wait, 1513}, {servo, 36}, {wait, 550}, {servo, 100}, {wait, 312}, {servo, 0}
		]
	].

scenario_game_medium_0269() ->
	[
		[
			{aac, <<"game/0269.aac">>}, {wait, 250}, {servo, 52}, {wait, 587}, {servo, 58}, {wait, 475}, {servo, 46}, {wait, 538}, {servo, 100}, {wait, 862}, {servo, 0}
		]
	].

scenario_game_medium_0372() ->
	[
		[
			{aac, <<"game/0372.aac">>}, {wait, 1100}, {servo, 56}, {wait, 975}, {servo, 38}, {wait, 362}, {servo, 53}, {wait, 400}, {servo, 63}, {wait, 288}, {servo, 100}, {wait, 475}, {servo, 0}
		]
	].

scenario_game_long_05coucoudefil() ->
	[
		[
			{aac, <<"game/05coucoudefil.aac">>}, {wait, 300}, {servo, 55}, {wait, 425}, {servo, 42}, {wait, 1462}, {servo, 57}, {wait, 513}, {servo, 29}, {wait, 1012}, {servo, 57}, {wait, 550}, {servo, 41}, {wait, 1388}, {servo, 100}, {wait, 1425}, {servo, 0}
		]
	].

scenario_game_long_03bidibidicencorqui() ->
	[
		[
			{aac, <<"game/03bidibidicencorqui.aac">>}, {wait, 375}, {servo, 61}, {wait, 425}, {servo, 33}, {wait, 350}, {servo, 64}, {wait, 525}, {servo, 34}, {wait, 274}, {servo, 64}, {wait, 651}, {servo, 35}, {wait, 2350}, {servo, 99}, {wait, 462}, {servo, 0}
		]
	].

scenario_upset_silent() ->
	[
		[
			{servo, 20}, {wait, 1000}, {servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_upset_silent2() ->
	[
		[
			{servo, 100}, {wait, 10}, {servo, 0}
		]
	].

scenario_upset_0101() ->
	[
		[
			{aac, <<"upset/0101.aac">>}, {wait, 2012}, {servo, 100}, {wait, 288}, {servo, 0}
		]
	].

scenario_upset_0117() ->
	[
		[
			{aac, <<"upset/0117.aac">>}, {wait, 1949}, {servo, 100}, {wait, 163}, {servo, 0}
		]
	].

scenario_upset_0180() ->
	[
		[
			{aac, <<"upset/0180.aac">>}, {wait, 1662}, {servo, 100}, {wait, 1100}, {servo, 0}
		]
	].

scenario_upset_0189() ->
	[
		[
			{aac, <<"upset/0189.aac">>}, {wait, 2925}, {servo, 100}, {wait, 349}, {servo, 0}
		]
	].

scenario_upset_0192() ->
	[
		[
			{aac, <<"upset/0192.aac">>}, {wait, 2487}, {servo, 100}, {wait, 138}, {servo, 0}
		]
	].

scenario_upset_0194() ->
	[
		[
			{aac, <<"upset/0194.aac">>}, {wait, 2575}, {servo, 100}, {wait, 212}, {servo, 0}
		]
	].

scenario_upset_0247() ->
	[
		[
			{aac, <<"upset/0247.aac">>}, {wait, 2850}, {servo, 99}, {wait, 71}, {servo, 0}
		]
	].

scenario_upset_0261() ->
	[
		[
			{aac, <<"upset/0261.aac">>}, {wait, sound}, {servo, 100}, {wait, 425}, {servo, 0}
		]
	].

scenario_upset_0384() ->
	[
		[
			{aac, <<"upset/0384.aac">>}, {wait, sound}, {servo, 100}, {wait, 425}, {servo, 0}
		]
	].

scenario_upset_0585() ->
	[
		[
			{aac, <<"upset/0585.aac">>}, {wait, sound}, {servo, 100}, {wait, 425}, {servo, 0}
		]
	].

scenario_missing_0134() ->
	[
		[
			{aac, <<"missing/0134.aac">>}
		]
	].

scenario_missing_0165() ->
	[
		[
			{aac, <<"missing/0165.aac">>}
		]
	].

scenario_missing_0187() ->
	[
		[
			{aac, <<"missing/0187.aac">>}
		]
	].

scenario_missing_0199() ->
	[
		[
			{aac, <<"missing/0199.aac">>}
		]
	].

scenario_missing_0206() ->
	[
		[
			{aac, <<"missing/0206.aac">>}
		]
	].

scenario_missing_0245() ->
	[
		[
			{aac, <<"missing/0245.aac">>}
		]
	].

scenario_missing_0281() ->
	[
		[
			{aac, <<"missing/0281.aac">>}
		]
	].

scenario_missing_0302() ->
	[
		[
			{aac, <<"missing/0302.aac">>}
		]
	].

scenario_missing_0381() ->
	[
		[
			{aac, <<"missing/0381.aac">>}
		]
	].

scenario_missing_0402() ->
	[
		[
			{aac, <<"missing/0402.aac">>}
		]
	].

scenario_meuh_V2() ->
	[
		[
			{servo, 60}, {wait, 1000}, {aac, <<"meuh/meuh-V2.aac">>}, {wait, 4754}, {servo, 0}
		]
	].

-ifdef(TEST).
count_test_() ->
    [
        ?_assert(count(play) > 0),
        ?_assertEqual(0, count(poke)),
        ?_assertEqual(0, count(unknown))
    ].

get_test_() ->
    [
        ?_assert(is_list(get(play, 1))),
        ?_assertEqual([], get(unknown, 1))
    ].
-endif.
