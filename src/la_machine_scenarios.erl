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
	scenario_calling_00315/0,
	scenario_calling_00353/0,
	scenario_excited_00425/0,
	scenario_excited_01021/0,
	scenario_excited_01041/0,
	scenario_excited_01340/0,
	scenario_excited_01572/0,
	scenario_excited_01652/0,
	scenario_excited_01789/0,
	scenario_excited_01795/0,
	scenario_excited_02044/0,
	scenario_excited_02137/0,
	scenario_game_long_03bidibidicencorqui/0,
	scenario_game_long_05coucoudefil/0,
	scenario_game_medium_02292/0,
	scenario_game_medium_02309/0,
	scenario_game_medium_02824/0,
	scenario_game_medium_02903/0,
	scenario_game_medium_03923/0,
	scenario_game_short_00992/0,
	scenario_game_short_01261/0,
	scenario_game_short_01512/0,
	scenario_game_short_01683/0,
	scenario_game_short_01800/0,
	scenario_hits_00412/0,
	scenario_hits_00441/0,
	scenario_hits_00496/0,
	scenario_hits_00534/0,
	scenario_hits_00717/0,
	scenario_hits_00831/0,
	scenario_hits_00888/0,
	scenario_hits_00995/0,
	scenario_hits_01056/0,
	scenario_hits_01338/0,
	scenario_hits_01857/0,
	scenario_joy_01367/0,
	scenario_joy_01498/0,
	scenario_joy_01510/0,
	scenario_joy_01878/0,
	scenario_joy_02069/0,
	scenario_joy_02220/0,
	scenario_joy_02308/0,
	scenario_joy_02445/0,
	scenario_joy_04086/0,
	scenario_joy_05201/0,
	scenario_meuh_meuh_V2/0,
	scenario_poke_tt_coucou_poke/0,
	scenario_tired_01536/0,
	scenario_tired_01582/0,
	scenario_tired_01645/0,
	scenario_tired_01859/0,
	scenario_tired_02146/0,
	scenario_tired_02234/0,
	scenario_tired_02308/0,
	scenario_tired_02314/0,
	scenario_tired_02330/0,
	scenario_tired_02550/0,
	scenario_tired_02985/0,
	scenario_upset_00570/0,
	scenario_upset_00800/0,
	scenario_upset_00806/0,
	scenario_upset_00812/0,
	scenario_upset_00911/0,
	scenario_upset_01201/0,
	scenario_upset_01221/0,
	scenario_upset_01238/0,
	scenario_upset_01280/0,
	scenario_upset_01327/0,
	scenario_upset_03087/0,
	scenario_upset_silent/0,
	scenario_upset_silent2/0
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

scenario_calling_00315() ->
	[
		[
			{aac, <<"calling/00315.aac">>}
		]
	].

scenario_calling_00353() ->
	[
		[
			{aac, <<"calling/00353.aac">>}
		]
	].

scenario_excited_00425() ->
	[
		[
			{wait, 10}, {servo, 100}, {wait, 484}, {aac, <<"excited/00425.aac">>}, {wait, 401}, {servo, 0}
		]
	].

scenario_excited_01021() ->
	[
		[
			{aac, <<"excited/01021.aac">>}, {wait, 244}, {servo, 100}, {wait, 631}, {servo, 0}
		]
	].

scenario_excited_01041() ->
	[
		[
			{servo, 100}, {wait, 463}, {aac, <<"excited/01041.aac">>}, {wait, 1094}, {servo, 0}
		]
	].

scenario_excited_01340() ->
	[
		[
			{servo, 100}, {wait, 426}, {aac, <<"excited/01340.aac">>}, {wait, 1313}, {servo, 0}
		]
	].

scenario_excited_01572() ->
	[
		[
			{servo, 50, 885}, {wait, 703}, {aac, <<"excited/01572.aac">>}, {wait, 453}, {servo, 100}, {wait, 557}, {servo, 0}
		]
	].

scenario_excited_01652() ->
	[
		[
			{servo, 54}, {wait, 494}, {servo, 100}, {wait, 73}, {aac, <<"excited/01652.aac">>}, {wait, 1599}, {servo, 0}
		]
	].

scenario_excited_01789() ->
	[
		[
			{servo, 51}, {wait, 275}, {aac, <<"excited/01789.aac">>}, {wait, 198}, {servo, 64}, {wait, 511}, {servo, 71}, {wait, 453}, {servo, 83}, {wait, 365}, {servo, 100}, {wait, 156}, {servo, 0}
		]
	].

scenario_excited_01795() ->
	[
		[
			{wait, 10}, {servo, 72}, {wait, 358}, {aac, <<"excited/01795.aac">>}, {wait, 1230}, {servo, 100}, {wait, 370}, {servo, 0}
		]
	].

scenario_excited_02044() ->
	[
		[
			{servo, 80, 2276}, {wait, 557}, {aac, <<"excited/02044.aac">>}, {wait, 1802}, {servo, 100}, {wait, 187}, {servo, 0}
		]
	].

scenario_excited_02137() ->
	[
		[
			{servo, 100}, {wait, 452}, {aac, <<"excited/02137.aac">>}, {wait, 735}, {servo, 89}, {wait, 297}, {servo, 98}, {wait, 260}, {servo, 88}, {wait, 250}, {servo, 95}, {wait, 209}, {servo, 87}, {wait, 291}, {servo, 95}, {wait, 198}, {servo, 0}
		]
	].

scenario_game_long_03bidibidicencorqui() ->
	[
		[
			{aac, <<"game/03bidibidicencorqui.aac">>}, {wait, 67}, {servo, 52}, {wait, 646}, {servo, 35}, {wait, 380}, {servo, 52}, {wait, 521}, {servo, 33}, {wait, 167}, {servo, 56, 604}, {wait, 807}, {servo, 33}, {wait, 193}, {servo, 58}, {wait, 281}, {servo, 58}, {wait, 172}, {servo, 29}, {wait, 78}, {servo, 60}, {wait, 203}, {servo, 35}, {wait, 214}, {servo, 64}, {wait, 500}, {servo, 70}, {wait, 172}, {servo, 100}, {wait, 656}, {servo, 0}
		]
	].

scenario_game_long_05coucoudefil() ->
	[
		[
			{aac, <<"game/05coucoudefil.aac">>}, {servo, 56}, {wait, 619}, {servo, 70}, {wait, 339}, {servo, 71}, {wait, 198}, {servo, 56}, {wait, 130}, {servo, 68}, {wait, 177}, {servo, 57}, {wait, 141}, {servo, 73}, {wait, 151}, {servo, 59}, {wait, 213}, {servo, 75}, {wait, 235}, {servo, 54}, {wait, 229}, {servo, 74}, {wait, 271}, {servo, 58}, {wait, 218}, {servo, 73}, {wait, 427}, {servo, 69}, {wait, 407}, {servo, 74}, {wait, 120}, {servo, 67}, {wait, 218}, {servo, 74}, {wait, 167}, {servo, 79}, {wait, 276}, {servo, 84}, {wait, 208}, {servo, 91}, {wait, 334}, {servo, 96}, {wait, 359}, {servo, 100}, {wait, 583}, {servo, 0, 1131}
		]
	].

scenario_game_medium_02292() ->
	[
		[
			{wait, 5}, {servo, 59}, {wait, 239}, {aac, <<"game/02292.aac">>}, {wait, 396}, {servo, 65}, {wait, 427}, {servo, 73}, {wait, 453}, {servo, 62}, {wait, 297}, {servo, 48}, {wait, 433}, {servo, 34}, {wait, 380}, {servo, 100}, {wait, 479}, {servo, 0}
		]
	].

scenario_game_medium_02309() ->
	[
		[
			{aac, <<"game/02309.aac">>}, {servo, 22}, {wait, 255}, {servo, 49}, {wait, 385}, {servo, 59}, {wait, 193}, {servo, 62}, {wait, 208}, {servo, 68}, {wait, 229}, {servo, 69}, {wait, 177}, {servo, 71}, {wait, 178}, {servo, 82}, {wait, 182}, {servo, 86}, {wait, 125}, {servo, 91}, {wait, 109}, {servo, 96}, {wait, 104}, {servo, 97}, {wait, 94}, {servo, 100}, {wait, 136}, {servo, 0}
		]
	].

scenario_game_medium_02824() ->
	[
		[
			{aac, <<"game/02824.aac">>}, {wait, 5}, {servo, 24}, {wait, 416}, {servo, 31}, {wait, 141}, {servo, 37}, {wait, 188}, {servo, 46}, {wait, 161}, {servo, 51}, {wait, 109}, {servo, 55}, {wait, 157}, {servo, 61}, {wait, 218}, {servo, 68}, {wait, 136}, {servo, 69}, {wait, 161}, {servo, 74}, {wait, 120}, {servo, 77}, {wait, 172}, {servo, 85}, {wait, 307}, {servo, 90}, {wait, 245}, {servo, 93}, {wait, 240}, {servo, 100}, {wait, 140}, {servo, 0}
		]
	].

scenario_game_medium_02903() ->
	[
		[
			{wait, 10}, {servo, 55}, {wait, 296}, {aac, <<"game/02903.aac">>}, {wait, 246}, {servo, 68}, {wait, 166}, {servo, 53}, {wait, 193}, {servo, 70}, {wait, 240}, {servo, 51}, {wait, 130}, {servo, 70}, {wait, 224}, {servo, 49}, {wait, 250}, {servo, 73}, {wait, 422}, {servo, 80}, {wait, 192}, {servo, 84}, {wait, 214}, {servo, 90}, {wait, 229}, {servo, 93}, {wait, 115}, {servo, 94}, {wait, 229}, {servo, 100}, {wait, 172}, {servo, 0}
		]
	].

scenario_game_medium_03923() ->
	[
		[
			{servo, 52}, {wait, 244}, {aac, <<"game/03923.aac">>}, {wait, 146}, {servo, 37}, {wait, 141}, {servo, 50}, {wait, 198}, {servo, 36}, {wait, 182}, {servo, 100, 2297}, {wait, 2505}, {servo, 87}, {wait, 229}, {servo, 79}, {wait, 203}, {servo, 67}, {wait, 198}, {servo, 54}, {wait, 318}, {servo, 0}
		]
	].

scenario_game_short_00992() ->
	[
		[
			{wait, 20}, {servo, 100}, {wait, 412}, {aac, <<"game/00992.aac">>}, {wait, 797}, {servo, 0}
		]
	].

scenario_game_short_01261() ->
	[
		[
			{wait, 192}, {servo, 100}, {wait, 427}, {aac, <<"game/01261.aac">>}, {wait, 365}, {servo, 93}, {wait, 193}, {servo, 83}, {wait, 198}, {servo, 78}, {wait, 177}, {servo, 73}, {wait, 140}, {servo, 65}, {wait, 167}, {servo, 52}, {wait, 250}, {servo, 38}, {wait, 302}, {servo, 0}
		]
	].

scenario_game_short_01512() ->
	[
		[
			{servo, 49, 828}, {wait, 854}, {aac, <<"game/01512.aac">>}, {wait, 21}, {servo, 55}, {wait, 119}, {servo, 61}, {wait, 209}, {servo, 69}, {wait, 140}, {servo, 74}, {wait, 177}, {servo, 79}, {wait, 151}, {servo, 82}, {wait, 131}, {servo, 86}, {wait, 151}, {servo, 100}, {wait, 333}, {servo, 0}
		]
	].

scenario_game_short_01683() ->
	[
		[
			{aac, <<"game/01683.aac">>}
		]
	].

scenario_game_short_01800() ->
	[
		[
			{servo, 66}, {wait, 291}, {aac, <<"game/01800.aac">>}, {wait, 370}, {servo, 74}, {wait, 177}, {servo, 66}, {wait, 141}, {servo, 61}, {wait, 104}, {servo, 55}, {wait, 198}, {servo, 47}, {wait, 67}, {servo, 100}, {wait, 724}, {servo, 0}
		]
	].

scenario_hits_00412() ->
	[
		[
			{aac, <<"hits/00412.aac">>}
		]
	].

scenario_hits_00441() ->
	[
		[
			{aac, <<"hits/00441.aac">>}
		]
	].

scenario_hits_00496() ->
	[
		[
			{aac, <<"hits/00496.aac">>}
		]
	].

scenario_hits_00534() ->
	[
		[
			{aac, <<"hits/00534.aac">>}
		]
	].

scenario_hits_00717() ->
	[
		[
			{aac, <<"hits/00717.aac">>}
		]
	].

scenario_hits_00831() ->
	[
		[
			{aac, <<"hits/00831.aac">>}
		]
	].

scenario_hits_00888() ->
	[
		[
			{aac, <<"hits/00888.aac">>}
		]
	].

scenario_hits_00995() ->
	[
		[
			{aac, <<"hits/00995.aac">>}
		]
	].

scenario_hits_01056() ->
	[
		[
			{aac, <<"hits/01056.aac">>}
		]
	].

scenario_hits_01338() ->
	[
		[
			{aac, <<"hits/01338.aac">>}
		]
	].

scenario_hits_01857() ->
	[
		[
			{aac, <<"hits/01857.aac">>}
		]
	].

scenario_joy_01367() ->
	[
		[
			{aac, <<"joy/01367.aac">>}, {wait, 20}, {servo, 24}, {wait, 578}, {servo, 58}, {wait, 610}, {servo, 100}, {wait, 260}, {servo, 0}
		]
	].

scenario_joy_01498() ->
	[
		[
			{aac, <<"joy/01498.aac">>}, {wait, 83}, {servo, 15}, {wait, 208}, {servo, 39}, {wait, 219}, {servo, 59}, {wait, 224}, {servo, 75}, {wait, 364}, {servo, 100}, {wait, 318}, {servo, 0}
		]
	].

scenario_joy_01510() ->
	[
		[
			{aac, <<"joy/01510.aac">>}, {wait, 72}, {servo, 26}, {wait, 339}, {servo, 53}, {wait, 323}, {servo, 77}, {wait, 687}, {servo, 100}, {wait, 157}, {servo, 0}
		]
	].

scenario_joy_01878() ->
	[
		[
			{aac, <<"joy/01878.aac">>}, {wait, 125}, {servo, 57}, {wait, 651}, {servo, 76}, {wait, 651}, {servo, 17}, {wait, 510}, {servo, 100}, {wait, 521}, {servo, 0}
		]
	].

scenario_joy_02069() ->
	[
		[
			{wait, 10}, {servo, 75, 943}, {wait, 859}, {aac, <<"joy/02069.aac">>}, {wait, 381}, {servo, 68}, {wait, 427}, {servo, 80}, {wait, 380}, {servo, 67}, {wait, 276}, {servo, 80}, {wait, 380}, {servo, 100}, {wait, 146}, {servo, 0}
		]
	].

scenario_joy_02220() ->
	[
		[
			{aac, <<"joy/02220.aac">>}, {wait, 5}, {servo, 75}, {wait, 1000}, {servo, 58}, {wait, 276}, {servo, 75}, {wait, 588}, {servo, 100}, {wait, 412}, {servo, 3}
		]
	].

scenario_joy_02308() ->
	[
		[
			{aac, <<"joy/02308.aac">>}, {servo, 61, 135}, {wait, 562}, {servo, 54}, {wait, 204}, {servo, 60}, {wait, 197}, {servo, 53}, {wait, 193}, {servo, 60}, {wait, 281}, {servo, 53}, {wait, 193}, {servo, 60}, {wait, 198}, {servo, 52}, {wait, 161}, {servo, 100}, {wait, 370}, {servo, 0}
		]
	].

scenario_joy_02445() ->
	[
		[
			{servo, 75}, {wait, 119}, {aac, <<"joy/02445.aac">>}, {wait, 651}, {servo, 68}, {wait, 490}, {servo, 54}, {wait, 542}, {servo, 56}, {wait, 349}, {servo, 50}, {wait, 281}, {servo, 100}, {wait, 406}, {servo, 0}
		]
	].

scenario_joy_04086() ->
	[
		[
			{servo, 75}, {wait, 327}, {aac, <<"joy/04086.aac">>}, {wait, 3615}, {servo, 100}, {wait, 412}, {servo, 0}
		]
	].

scenario_joy_05201() ->
	[
		[
			{wait, 666}, {servo, 56, 1714}, {wait, 1438}, {aac, <<"joy/05201.aac">>}, {wait, 406}, {servo, 69, 719}, {wait, 1151}, {servo, 38, 792}, {wait, 1000}, {servo, 53, 667}, {wait, 1068}, {servo, 36, 640}, {wait, 948}, {servo, 49, 265}, {wait, 395}, {servo, 100}, {wait, 329}, {servo, 0}
		]
	].

scenario_meuh_meuh_V2() ->
	[
		[
			{servo, 60, 2500}, {wait, 1000}, {aac, <<"meuh/meuh_V2.aac">>}, {wait, 4754}, {servo, 0, 800}
		]
	].

scenario_poke_tt_coucou_poke() ->
	[
		[
			{aac, <<"poke/tt_coucou_poke.aac">>}
		]
	].

scenario_tired_01536() ->
	[
		[
			{wait, 515}, {servo, 50}, {wait, 1443}, {servo, 66, 646}, {wait, 713}, {servo, 100, 886}, {wait, 68}, {aac, <<"tired/01536.aac">>}, {wait, 1615}, {servo, 0}
		]
	].

scenario_tired_01582() ->
	[
		[
			{wait, 416}, {servo, 50, 2646}, {wait, 2589}, {aac, <<"tired/01582.aac">>}, {wait, 83}, {servo, 100, 1484}, {wait, 1573}, {servo, 0}
		]
	].

scenario_tired_01645() ->
	[
		[
			{wait, 333}, {servo, 50, 1453}, {wait, 2338}, {servo, 100, 1714}, {wait, 177}, {aac, <<"tired/01645.aac">>}, {wait, 1714}, {servo, 0, 1635}
		]
	].

scenario_tired_01859() ->
	[
		[
			{wait, 583}, {servo, 100, 2484}, {wait, 2323}, {aac, <<"tired/01859.aac">>}, {wait, 245}, {servo, 0, 1859}
		]
	].

scenario_tired_02146() ->
	[
		[
			{wait, 1062}, {servo, 50}, {wait, 927}, {servo, 100, 1089}, {wait, 969}, {aac, <<"tired/02146.aac">>}, {wait, 151}, {servo, 0, 2224}
		]
	].

scenario_tired_02234() ->
	[
		[
			{wait, 526}, {servo, 21}, {wait, 786}, {servo, 32}, {wait, 526}, {servo, 47}, {wait, 516}, {servo, 61}, {wait, 411}, {servo, 77}, {wait, 104}, {aac, <<"tired/02234.aac">>}, {wait, 52}, {servo, 100}, {wait, 1886}, {servo, 0, 1338}
		]
	].

scenario_tired_02308() ->
	[
		[
			{wait, 437}, {servo, 45}, {wait, 859}, {servo, 100, 2917}, {wait, 1302}, {aac, <<"tired/02308.aac">>}, {wait, 1881}, {servo, 0, 1448}
		]
	].

scenario_tired_02314() ->
	[
		[
			{wait, 140}, {servo, 100, 2495}, {wait, 2286}, {aac, <<"tired/02314.aac">>}, {wait, 704}, {servo, 91}, {wait, 573}, {servo, 96}, {wait, 312}, {servo, 91}, {wait, 261}, {servo, 97}, {wait, 354}, {servo, 2, 1333}
		]
	].

scenario_tired_02330() ->
	[
		[
			{wait, 57}, {servo, 100, 2724}, {wait, 2547}, {aac, <<"tired/02330.aac">>}, {wait, 375}, {servo, 88}, {wait, 328}, {servo, 97}, {wait, 453}, {servo, 87}, {wait, 292}, {servo, 98}, {wait, 474}, {servo, 0, 1005}
		]
	].

scenario_tired_02550() ->
	[
		[
			{wait, 218}, {servo, 75, 2479}, {wait, 2380}, {aac, <<"tired/02550.aac">>}, {wait, 188}, {servo, 93}, {wait, 271}, {servo, 82}, {wait, 203}, {servo, 90}, {wait, 281}, {servo, 84}, {wait, 219}, {servo, 92}, {wait, 318}, {servo, 82}, {wait, 250}, {servo, 94}, {wait, 286}, {servo, 82}, {wait, 349}, {servo, 100, -10}, {wait, 193}, {servo, 0}
		]
	].

scenario_tired_02985() ->
	[
		[
			{servo, 62, 2281}, {wait, 2578}, {servo, 100}, {wait, 47}, {aac, <<"tired/02985.aac">>}, {wait, 260}, {servo, 0, 2807}
		]
	].

scenario_upset_00570() ->
	[
		[
			{wait, 10}, {servo, 50, 2135}, {wait, 2312}, {servo, 100}, {wait, 162}, {aac, <<"upset/00570.aac">>}, {wait, 604}, {servo, 0}
		]
	].

scenario_upset_00800() ->
	[
		[
			{wait, 10}, {servo, 61, 2193}, {wait, 2219}, {servo, 100}, {wait, 83}, {aac, <<"upset/00800.aac">>}, {wait, 786}, {servo, 0}
		]
	].

scenario_upset_00806() ->
	[
		[
			{servo, 59, 1604}, {wait, 1593}, {servo, 100}, {wait, 99}, {aac, <<"upset/00806.aac">>}, {wait, 802}, {servo, 0}
		]
	].

scenario_upset_00812() ->
	[
		[
			{wait, 26}, {servo, 100, 1921}, {wait, 1854}, {aac, <<"upset/00812.aac">>}, {wait, 880}, {servo, 0}
		]
	].

scenario_upset_00911() ->
	[
		[
			{wait, 119}, {servo, 100}, {wait, 510}, {aac, <<"upset/00911.aac">>}, {wait, 917}, {servo, 0}
		]
	].

scenario_upset_01201() ->
	[
		[
			{wait, 458}, {servo, 74}, {wait, 349}, {aac, <<"upset/01201.aac">>}, {wait, 859}, {servo, 100}, {wait, 396}, {servo, 0}
		]
	].

scenario_upset_01221() ->
	[
		[
			{wait, 10}, {servo, 51, 760}, {wait, 984}, {servo, 100, 1339}, {wait, 229}, {aac, <<"upset/01221.aac">>}, {wait, 1172}, {servo, 0}
		]
	].

scenario_upset_01238() ->
	[
		[
			{servo, 50, 2098}, {wait, 2124}, {aac, <<"upset/01238.aac">>}, {wait, 68}, {servo, 59}, {wait, 130}, {servo, 51}, {wait, 105}, {servo, 59}, {wait, 166}, {servo, 49}, {wait, 120}, {servo, 61}, {wait, 130}, {servo, 49}, {wait, 120}, {servo, 62}, {wait, 94}, {servo, 72}, {wait, 120}, {servo, 100}, {wait, 286}, {servo, 0}
		]
	].

scenario_upset_01280() ->
	[
		[
			{servo, 50, 1651}, {wait, 1854}, {servo, 100}, {wait, 166}, {aac, <<"upset/01280.aac">>}, {wait, 1302}, {servo, 0}
		]
	].

scenario_upset_01327() ->
	[
		[
			{servo, 25, 192}, {wait, 2104}, {servo, 100}, {wait, 291}, {aac, <<"upset/01327.aac">>}, {wait, 1355}, {servo, 0}
		]
	].

scenario_upset_03087() ->
	[
		[
			{wait, 10}, {servo, 15}, {wait, 1901}, {servo, 100}, {wait, 318}, {aac, <<"upset/03087.aac">>}, {wait, 239}, {servo, 0, 2948}
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
