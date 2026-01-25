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
    scenario_test_1/0,
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
    | {servo, non_neg_integer(), non_neg_integer()}
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
        {aac, <<"calling/00315.aac">>}, {wait, 187}, {servo, 47}, {wait, 281}, {servo, 0}
    ].

scenario_calling_00353() ->
    [
        {aac, <<"calling/00353.aac">>}
    ].

scenario_excited_00425() ->
    [
        {wait, 109},
        {servo, 75},
        {wait, 385},
        {aac, <<"excited/00425.aac">>},
        {wait, 308},
        {servo, 3, 479}
    ].

scenario_excited_01021() ->
    [
        {aac, <<"excited/01021.aac">>},
        {servo, 49},
        {wait, 390},
        {servo, 58},
        {wait, 151},
        {servo, 64},
        {wait, 136},
        {servo, 71},
        {wait, 182},
        {servo, 45}
    ].

scenario_excited_01041() ->
    [
        {aac, <<"excited/01041.aac">>},
        {servo, 39},
        {wait, 343},
        {servo, 68},
        {wait, 162},
        {servo, 50},
        {wait, 114},
        {servo, 67},
        {wait, 120},
        {servo, 48},
        {wait, 89},
        {servo, 67},
        {wait, 166},
        {servo, 51}
    ].

scenario_excited_01340() ->
    [
        {wait, 5},
        {servo, 72, 302},
        {wait, 259},
        {aac, <<"excited/01340.aac">>},
        {wait, 183},
        {servo, 36, 1193}
    ].

scenario_excited_01572() ->
    [
        {servo, 53, 83},
        {wait, 83},
        {aac, <<"excited/01572.aac">>},
        {wait, 318},
        {servo, 64},
        {wait, 281},
        {servo, 76, 94},
        {wait, 411},
        {servo, 27, 657}
    ].

scenario_excited_01652() ->
    [
        {servo, 59, 281},
        {wait, 196},
        {aac, <<"excited/01652.aac">>},
        {wait, 267},
        {servo, 40},
        {wait, 250},
        {servo, 71},
        {wait, 427},
        {servo, 26, 839}
    ].

scenario_excited_01789() ->
    [
        {servo, 51},
        {wait, 275},
        {aac, <<"excited/01789.aac">>},
        {wait, 198},
        {servo, 64},
        {wait, 511},
        {servo, 71},
        {wait, 453},
        {servo, 78},
        {wait, 214},
        {servo, 31, 411}
    ].

scenario_excited_01795() ->
    [
        {wait, 10},
        {servo, 72},
        {wait, 358},
        {aac, <<"excited/01795.aac">>},
        {wait, 371},
        {servo, 74},
        {wait, 177},
        {servo, 59},
        {wait, 104},
        {servo, 73},
        {wait, 193},
        {servo, 59},
        {wait, 141},
        {servo, 75},
        {wait, 750},
        {servo, 51}
    ].

scenario_excited_02044() ->
    [
        {servo, 53, 500},
        {wait, 557},
        {aac, <<"excited/02044.aac">>},
        {wait, 88},
        {servo, 74, 855},
        {wait, 922},
        {servo, 68},
        {wait, 151},
        {servo, 74},
        {wait, 99},
        {servo, 68},
        {wait, 115},
        {servo, 73},
        {wait, 88},
        {servo, 65},
        {wait, 136},
        {servo, 74},
        {wait, 104},
        {servo, 67},
        {wait, 135},
        {servo, 75},
        {wait, 141},
        {servo, 65}
    ].

scenario_excited_02137() ->
    [
        {servo, 53},
        {wait, 452},
        {aac, <<"excited/02137.aac">>},
        {wait, 11},
        {servo, 68},
        {wait, 146},
        {servo, 57},
        {wait, 114},
        {servo, 68},
        {wait, 131},
        {servo, 54},
        {wait, 109},
        {servo, 67},
        {wait, 146},
        {servo, 51},
        {wait, 68},
        {servo, 75},
        {wait, 224},
        {servo, 26, 1307}
    ].

scenario_game_long_03bidibidicencorqui() ->
    [
        {aac, <<"game/03bidibidicencorqui.aac">>},
        {wait, 67},
        {servo, 52},
        {wait, 646},
        {servo, 35},
        {wait, 380},
        {servo, 52},
        {wait, 521},
        {servo, 33},
        {wait, 167},
        {servo, 56, 604},
        {wait, 807},
        {servo, 33},
        {wait, 193},
        {servo, 58},
        {wait, 281},
        {servo, 58},
        {wait, 172},
        {servo, 29},
        {wait, 78},
        {servo, 60},
        {wait, 203},
        {servo, 35},
        {wait, 214},
        {servo, 64},
        {wait, 500},
        {servo, 70},
        {wait, 276},
        {servo, 75},
        {wait, 651},
        {servo, 37}
    ].

scenario_game_long_05coucoudefil() ->
    [
        {aac, <<"game/05coucoudefil.aac">>},
        {servo, 56},
        {wait, 619},
        {servo, 70},
        {wait, 339},
        {servo, 71},
        {wait, 198},
        {servo, 56},
        {wait, 130},
        {servo, 68},
        {wait, 177},
        {servo, 57},
        {wait, 141},
        {servo, 73},
        {wait, 151},
        {servo, 59},
        {wait, 213},
        {servo, 75},
        {wait, 235},
        {servo, 54},
        {wait, 229},
        {servo, 74},
        {wait, 271},
        {servo, 58},
        {wait, 218},
        {servo, 73},
        {wait, 427},
        {servo, 69},
        {wait, 407},
        {servo, 74},
        {wait, 120},
        {servo, 67},
        {wait, 218},
        {servo, 74},
        {wait, 167},
        {servo, 77, 5},
        {wait, 276},
        {servo, 72},
        {wait, 187},
        {servo, 65},
        {wait, 94},
        {servo, 73},
        {wait, 146},
        {servo, 64},
        {wait, 141},
        {servo, 74},
        {wait, 146},
        {servo, 65},
        {wait, 135},
        {servo, 75},
        {wait, 130},
        {servo, 64},
        {wait, 146},
        {servo, 76},
        {wait, 370},
        {servo, 50, 958}
    ].

scenario_game_medium_02292() ->
    [
        {wait, 5},
        {servo, 59},
        {wait, 239},
        {aac, <<"game/02292.aac">>},
        {wait, 396},
        {servo, 65},
        {wait, 427},
        {servo, 73},
        {wait, 453},
        {servo, 62},
        {wait, 297},
        {servo, 48},
        {wait, 433},
        {servo, 34},
        {wait, 630},
        {servo, 33}
    ].

scenario_game_medium_02309() ->
    [
        {aac, <<"game/02309.aac">>},
        {servo, 22},
        {wait, 255},
        {servo, 49},
        {wait, 385},
        {servo, 59},
        {wait, 193},
        {servo, 62},
        {wait, 208},
        {servo, 68},
        {wait, 229},
        {servo, 69},
        {wait, 177},
        {servo, 71},
        {wait, 178},
        {servo, 76},
        {wait, 78},
        {servo, 69, 52},
        {wait, 109},
        {servo, 74, 36},
        {wait, 73},
        {servo, 64},
        {wait, 125},
        {servo, 58},
        {wait, 141},
        {servo, 52},
        {wait, 119},
        {servo, 47},
        {wait, 313},
        {servo, 48}
    ].

scenario_game_medium_02824() ->
    [
        {aac, <<"game/02824.aac">>},
        {wait, 5},
        {servo, 24},
        {wait, 416},
        {servo, 31},
        {wait, 141},
        {servo, 37},
        {wait, 188},
        {servo, 46},
        {wait, 161},
        {servo, 51},
        {wait, 109},
        {servo, 55},
        {wait, 157},
        {servo, 61},
        {wait, 218},
        {servo, 68},
        {wait, 136},
        {servo, 69},
        {wait, 161},
        {servo, 74},
        {wait, 120},
        {servo, 77},
        {wait, 172},
        {servo, 72},
        {wait, 250},
        {servo, 64, 172},
        {wait, 260},
        {servo, 59},
        {wait, 657},
        {servo, 59}
    ].

scenario_game_medium_02903() ->
    [
        {wait, 10},
        {servo, 55},
        {wait, 296},
        {aac, <<"game/02903.aac">>},
        {wait, 246},
        {servo, 68},
        {wait, 161},
        {servo, 41},
        {wait, 198},
        {servo, 70},
        {wait, 234},
        {servo, 39},
        {wait, 198},
        {servo, 70},
        {wait, 183},
        {servo, 39},
        {wait, 229},
        {servo, 73},
        {wait, 422},
        {servo, 76},
        {wait, 192},
        {servo, 72},
        {wait, 162},
        {servo, 64},
        {wait, 219},
        {servo, 59},
        {wait, 109},
        {servo, 52},
        {wait, 255},
        {servo, 75},
        {wait, 359},
        {servo, 75, 99}
    ].

scenario_game_medium_03923() ->
    [
        {servo, 52},
        {wait, 244},
        {aac, <<"game/03923.aac">>},
        {wait, 146},
        {servo, 37},
        {wait, 141},
        {servo, 50},
        {wait, 198},
        {servo, 36},
        {wait, 172},
        {servo, 76, 2297},
        {wait, 2421},
        {servo, 64, 89},
        {wait, 276},
        {servo, 58},
        {wait, 162},
        {servo, 52, 99},
        {wait, 182},
        {servo, 46, 89},
        {wait, 203},
        {servo, 38},
        {wait, 282},
        {servo, 37}
    ].

scenario_game_short_00992() ->
    [
        {servo, 72},
        {wait, 197},
        {aac, <<"game/00992.aac">>},
        {wait, 219},
        {servo, 50},
        {wait, 177},
        {servo, 70},
        {wait, 193},
        {servo, 51},
        {wait, 703},
        {servo, 50}
    ].

scenario_game_short_01261() ->
    [
        {wait, 203},
        {servo, 72},
        {wait, 416},
        {aac, <<"game/01261.aac">>},
        {wait, 328},
        {servo, 65},
        {wait, 183},
        {servo, 61},
        {wait, 213},
        {servo, 55},
        {wait, 183},
        {servo, 50},
        {wait, 156},
        {servo, 44},
        {wait, 177},
        {servo, 50, 5},
        {wait, 271},
        {servo, 50}
    ].

scenario_game_short_01512() ->
    [
        {servo, 49, 828},
        {wait, 848},
        {aac, <<"game/01512.aac">>},
        {wait, 27},
        {servo, 55},
        {wait, 119},
        {servo, 61},
        {wait, 209},
        {servo, 69},
        {wait, 140},
        {servo, 74},
        {wait, 177},
        {servo, 69},
        {wait, 146},
        {servo, 65},
        {wait, 136},
        {servo, 70},
        {wait, 145},
        {servo, 76},
        {wait, 308},
        {servo, 61},
        {wait, 130},
        {servo, 48},
        {wait, 224},
        {servo, 49}
    ].

scenario_game_short_01683() ->
    [
        {servo, 61},
        {wait, 223},
        {aac, <<"game/01683.aac">>},
        {wait, 250},
        {servo, 32},
        {wait, 334},
        {servo, 65},
        {wait, 359},
        {servo, 33},
        {wait, 307},
        {servo, 65},
        {wait, 745},
        {servo, 65}
    ].

scenario_game_short_01800() ->
    [
        {servo, 66},
        {wait, 291},
        {aac, <<"game/01800.aac">>},
        {wait, 370},
        {servo, 74},
        {wait, 177},
        {servo, 66},
        {wait, 141},
        {servo, 61},
        {wait, 104},
        {servo, 55},
        {wait, 198},
        {servo, 47},
        {wait, 99},
        {servo, 74},
        {wait, 453},
        {servo, 19},
        {wait, 536},
        {servo, 18}
    ].

scenario_hits_00412() ->
    [
        {aac, <<"hits/00412.aac">>}
    ].

scenario_hits_00441() ->
    [
        {aac, <<"hits/00441.aac">>}
    ].

scenario_hits_00496() ->
    [
        {aac, <<"hits/00496.aac">>}
    ].

scenario_hits_00534() ->
    [
        {aac, <<"hits/00534.aac">>}
    ].

scenario_hits_00717() ->
    [
        {aac, <<"hits/00717.aac">>}
    ].

scenario_hits_00831() ->
    [
        {aac, <<"hits/00831.aac">>}
    ].

scenario_hits_00888() ->
    [
        {aac, <<"hits/00888.aac">>}
    ].

scenario_hits_00995() ->
    [
        {aac, <<"hits/00995.aac">>}
    ].

scenario_hits_01056() ->
    [
        {aac, <<"hits/01056.aac">>}
    ].

scenario_hits_01338() ->
    [
        {aac, <<"hits/01338.aac">>}
    ].

scenario_hits_01857() ->
    [
        {aac, <<"hits/01857.aac">>}
    ].

scenario_joy_01367() ->
    [
        {aac, <<"joy/01367.aac">>},
        {wait, 20},
        {servo, 24},
        {wait, 308},
        {servo, 52},
        {wait, 297},
        {servo, 64, 219},
        {wait, 546},
        {servo, 74, 203}
    ].

scenario_joy_01498() ->
    [
        {aac, <<"joy/01498.aac">>},
        {wait, 83},
        {servo, 15},
        {wait, 208},
        {servo, 39},
        {wait, 219},
        {servo, 59},
        {wait, 224},
        {servo, 75},
        {wait, 344},
        {servo, 76},
        {wait, 119},
        {servo, 50}
    ].

scenario_joy_01510() ->
    [
        {aac, <<"joy/01510.aac">>},
        {wait, 72},
        {servo, 26},
        {wait, 339},
        {servo, 53},
        {wait, 323},
        {servo, 77},
        {wait, 370},
        {servo, 73},
        {wait, 255},
        {servo, 52}
    ].

scenario_joy_01878() ->
    [
        {aac, <<"joy/01878.aac">>},
        {wait, 125},
        {servo, 57},
        {wait, 651},
        {servo, 76},
        {wait, 244},
        {servo, 40, 870}
    ].

scenario_joy_02069() ->
    [
        {wait, 10},
        {servo, 75, 943},
        {wait, 859},
        {aac, <<"joy/02069.aac">>},
        {wait, 381},
        {servo, 68},
        {wait, 427},
        {servo, 80},
        {wait, 380},
        {servo, 67},
        {wait, 276},
        {servo, 80},
        {wait, 208},
        {servo, 64},
        {wait, 443},
        {servo, 65}
    ].

scenario_joy_02220() ->
    [
        {aac, <<"joy/02220.aac">>},
        {wait, 5},
        {servo, 75},
        {wait, 1000},
        {servo, 58},
        {wait, 286},
        {servo, 68},
        {wait, 360},
        {servo, 71},
        {wait, 427},
        {servo, 78, 198}
    ].

scenario_joy_02308() ->
    [
        {aac, <<"joy/02308.aac">>},
        {servo, 61, 135},
        {wait, 562},
        {servo, 54},
        {wait, 204},
        {servo, 60},
        {wait, 197},
        {servo, 53},
        {wait, 193},
        {servo, 60},
        {wait, 281},
        {servo, 53},
        {wait, 193},
        {servo, 60},
        {wait, 198},
        {servo, 52},
        {wait, 156},
        {servo, 60},
        {wait, 187},
        {servo, 51}
    ].

scenario_joy_02445() ->
    [
        {servo, 75},
        {wait, 119},
        {aac, <<"joy/02445.aac">>},
        {wait, 651},
        {servo, 68},
        {wait, 490},
        {servo, 54},
        {wait, 542},
        {servo, 56},
        {wait, 421},
        {servo, 76}
    ].

scenario_joy_04086() ->
    [
        {servo, 75}, {wait, 327}, {aac, <<"joy/04086.aac">>}, {wait, 4110}, {servo, 75}
    ].

scenario_joy_05201() ->
    [
        {wait, 281},
        {aac, <<"joy/05201.aac">>},
        {wait, 385},
        {servo, 56, 1714},
        {wait, 1849},
        {servo, 36, 719},
        {wait, 990},
        {servo, 65, 792},
        {wait, 1286},
        {servo, 35, 667}
    ].

scenario_meuh_meuh_V2() ->
    [
        {servo, 60, 2500},
        {wait, 1000},
        {aac, <<"meuh/meuh_V2.aac">>},
        {wait, 4754},
        {servo, 0, 800}
    ].

scenario_poke_tt_coucou_poke() ->
    [
        {aac, <<"poke/tt_coucou_poke.aac">>}
    ].

scenario_test_1() ->
    [
        {wait, 451},
        {servo, 17},
        {wait, 2540},
        {servo, 85, 1592},
        {wait, 2414},
        {servo, 100, 590},
        {wait, 1666},
        {servo, 0, 1000}
    ].

scenario_tired_01536() ->
    [
        {wait, 479},
        {servo, 51},
        {wait, 448},
        {servo, 69, 1880},
        {wait, 1812},
        {aac, <<"tired/01536.aac">>},
        {wait, 344},
        {servo, 75},
        {wait, 94},
        {servo, 9, 1208}
    ].

scenario_tired_01582() ->
    [
        {wait, 416},
        {servo, 50, 2646},
        {wait, 2589},
        {aac, <<"tired/01582.aac">>},
        {wait, 73},
        {servo, 76, 1484},
        {wait, 1531},
        {servo, 50}
    ].

scenario_tired_01645() ->
    [
        {servo, 51, 1010},
        {wait, 1859},
        {servo, 74, 588},
        {wait, 150},
        {aac, <<"tired/01645.aac">>},
        {wait, 720},
        {servo, 52},
        {wait, 244},
        {servo, 75, 698}
    ].

scenario_tired_01859() ->
    [
        {servo, 75, 2942},
        {wait, 2833},
        {aac, <<"tired/01859.aac">>},
        {wait, 156},
        {servo, 25, 1859}
    ].

scenario_tired_02146() ->
    [
        {servo, 50, 1380},
        {wait, 2380},
        {servo, 76, 640},
        {wait, 578},
        {aac, <<"tired/02146.aac">>},
        {wait, 250},
        {servo, 67},
        {wait, 125},
        {servo, 74},
        {wait, 167},
        {servo, 68},
        {wait, 98},
        {servo, 28, 2224}
    ].

scenario_tired_02234() ->
    [
        {servo, 21, 583},
        {wait, 1312},
        {servo, 32, 255},
        {wait, 526},
        {servo, 47, 328},
        {wait, 516},
        {servo, 61, 281},
        {wait, 411},
        {servo, 77},
        {wait, 104},
        {aac, <<"tired/02234.aac">>},
        {wait, 1990},
        {servo, 47, 474}
    ].

scenario_tired_02308() ->
    [
        {wait, 20},
        {servo, 50, 1131},
        {wait, 1276},
        {servo, 75, 1339},
        {wait, 1291},
        {aac, <<"tired/02308.aac">>},
        {wait, 100},
        {servo, 71},
        {wait, 115},
        {servo, 69},
        {wait, 192},
        {servo, 68},
        {wait, 188},
        {servo, 67},
        {wait, 182},
        {servo, 66},
        {wait, 156},
        {servo, 67},
        {wait, 115},
        {servo, 70},
        {wait, 115},
        {servo, 73},
        {wait, 88},
        {servo, 74},
        {wait, 203},
        {servo, 76},
        {wait, 136},
        {servo, 46, 765}
    ].

scenario_tired_02314() ->
    [
        {wait, 98},
        {servo, 76, 2495},
        {wait, 2328},
        {aac, <<"tired/02314.aac">>},
        {wait, 574},
        {servo, 62},
        {wait, 312},
        {servo, 75},
        {wait, 318},
        {servo, 61},
        {wait, 161},
        {servo, 75},
        {wait, 604},
        {servo, 38, 422}
    ].

scenario_tired_02330() ->
    [
        {wait, 20},
        {servo, 75, 2724},
        {wait, 2584},
        {aac, <<"tired/02330.aac">>},
        {wait, 375},
        {servo, 67, 192},
        {wait, 328},
        {servo, 74, 166},
        {wait, 453},
        {servo, 69, 213},
        {wait, 292},
        {servo, 62, 349},
        {wait, 453},
        {servo, 75, 474}
    ].

scenario_tired_02550() ->
    [
        {servo, 75, 2630},
        {wait, 2598},
        {aac, <<"tired/02550.aac">>},
        {wait, 157},
        {servo, 65},
        {wait, 276},
        {servo, 75},
        {wait, 172},
        {servo, 65},
        {wait, 297},
        {servo, 75},
        {wait, 161},
        {servo, 65, 323},
        {wait, 594},
        {servo, 75},
        {wait, 130},
        {servo, 66},
        {wait, 161},
        {servo, 73},
        {wait, 349},
        {servo, 62},
        {wait, 198},
        {servo, 42}
    ].

scenario_tired_02985() ->
    [
        {wait, 5},
        {servo, 76, 2666},
        {wait, 2620},
        {aac, <<"tired/02985.aac">>},
        {wait, 692},
        {servo, 71},
        {wait, 130},
        {servo, 75},
        {wait, 63},
        {servo, 71, 94},
        {wait, 78},
        {servo, 75},
        {wait, 125},
        {servo, 71},
        {wait, 57},
        {servo, 76},
        {wait, 136},
        {servo, 45, 2140}
    ].

scenario_upset_00570() ->
    [
        {wait, 10},
        {servo, 49, 588},
        {wait, 2349},
        {servo, 100},
        {wait, 125},
        {aac, <<"upset/00570.aac">>},
        {wait, 292},
        {servo, 9}
    ].

scenario_upset_00800() ->
    [
        {wait, 10},
        {servo, 50, 302},
        {wait, 495},
        {servo, 39},
        {wait, 208},
        {servo, 49},
        {wait, 271},
        {servo, 36},
        {wait, 266},
        {servo, 49},
        {wait, 250},
        {servo, 36},
        {wait, 286},
        {servo, 50},
        {wait, 224},
        {servo, 35},
        {wait, 130},
        {servo, 100},
        {wait, 172},
        {aac, <<"upset/00800.aac">>},
        {wait, 490},
        {servo, 0}
    ].

scenario_upset_00806() ->
    [
        {servo, 75, 718},
        {wait, 1692},
        {aac, <<"upset/00806.aac">>},
        {wait, 203},
        {servo, 69},
        {wait, 110},
        {servo, 75},
        {wait, 83},
        {servo, 69},
        {wait, 115},
        {servo, 75},
        {wait, 99},
        {servo, 68},
        {wait, 104},
        {servo, 100},
        {wait, 672},
        {servo, 0}
    ].

scenario_upset_00812() ->
    [
        {wait, 15},
        {servo, 50, 521},
        {wait, 1865},
        {aac, <<"upset/00812.aac">>},
        {wait, 125},
        {servo, 38},
        {wait, 52},
        {servo, 50},
        {wait, 130},
        {servo, 39},
        {wait, 78},
        {servo, 49},
        {wait, 52},
        {servo, 39},
        {wait, 63},
        {servo, 50},
        {wait, 62},
        {servo, 38},
        {wait, 89},
        {servo, 100},
        {wait, 677},
        {servo, 0}
    ].

scenario_upset_00911() ->
    [
        {servo, 75, 651},
        {wait, 629},
        {aac, <<"upset/00911.aac">>},
        {wait, 94},
        {servo, 57, 224},
        {wait, 402},
        {servo, 73},
        {wait, 291},
        {servo, 57, 209}
    ].

scenario_upset_01201() ->
    [
        {servo, 49, 885},
        {wait, 807},
        {aac, <<"upset/01201.aac">>},
        {wait, 344},
        {servo, 36},
        {wait, 541},
        {servo, 100, 167},
        {wait, 526},
        {servo, 0}
    ].

scenario_upset_01221() ->
    [
        {wait, 10},
        {servo, 51, 760},
        {wait, 1120},
        {servo, 76, 68},
        {wait, 93},
        {aac, <<"upset/01221.aac">>},
        {wait, 79},
        {servo, 63},
        {wait, 88},
        {servo, 74},
        {wait, 73},
        {servo, 62},
        {wait, 99},
        {servo, 74},
        {wait, 99},
        {servo, 61},
        {wait, 99},
        {servo, 74},
        {wait, 78},
        {servo, 61},
        {wait, 94},
        {servo, 75},
        {wait, 99},
        {servo, 60},
        {wait, 88},
        {servo, 75},
        {wait, 136},
        {servo, 59},
        {wait, 140},
        {servo, 74},
        {wait, 115},
        {servo, 57}
    ].

scenario_upset_01238() ->
    [
        {servo, 50, 989},
        {wait, 2124},
        {aac, <<"upset/01238.aac">>},
        {wait, 68},
        {servo, 59},
        {wait, 130},
        {servo, 51},
        {wait, 105},
        {servo, 59},
        {wait, 166},
        {servo, 49},
        {wait, 120},
        {servo, 61},
        {wait, 130},
        {servo, 49},
        {wait, 120},
        {servo, 62},
        {wait, 94},
        {servo, 100, 109},
        {wait, 510},
        {servo, 25}
    ].

scenario_upset_01280() ->
    [
        {servo, 51, 973},
        {wait, 1104},
        {servo, 45},
        {wait, 88},
        {servo, 50},
        {wait, 104},
        {servo, 45},
        {wait, 105},
        {servo, 50},
        {wait, 93},
        {servo, 45},
        {wait, 94},
        {servo, 49},
        {wait, 115},
        {servo, 45},
        {wait, 93},
        {servo, 51},
        {wait, 110},
        {servo, 44},
        {wait, 99},
        {servo, 51},
        {wait, 15},
        {aac, <<"upset/01280.aac">>},
        {wait, 63},
        {servo, 56},
        {wait, 83},
        {servo, 61},
        {wait, 110},
        {servo, 66},
        {wait, 114},
        {servo, 70},
        {wait, 125},
        {servo, 73},
        {wait, 78},
        {servo, 77},
        {wait, 125},
        {servo, 24, 641}
    ].

scenario_upset_01327() ->
    [
        {servo, 40, 187},
        {wait, 640},
        {servo, 75, 807},
        {wait, 1755},
        {aac, <<"upset/01327.aac">>},
        {wait, 73},
        {servo, 100},
        {wait, 172},
        {servo, 75, 125},
        {wait, 156},
        {servo, 100},
        {wait, 167},
        {servo, 74},
        {wait, 177},
        {servo, 100, 115},
        {wait, 167},
        {servo, 75, 62},
        {wait, 208},
        {servo, 100},
        {wait, 469},
        {servo, 0, 208}
    ].

scenario_upset_03087() ->
    [
        {wait, 10},
        {servo, 15},
        {wait, 505},
        {servo, 24},
        {wait, 396},
        {servo, 32},
        {wait, 385},
        {servo, 38},
        {wait, 615},
        {servo, 100},
        {wait, 318},
        {aac, <<"upset/03087.aac">>},
        {wait, 239},
        {servo, 0, 2948}
    ].

scenario_upset_silent() ->
    [
        {servo, 100, 302}, {wait, 593}, {servo, 50}, {wait, 1391}, {servo, 0, 2026}
    ].

scenario_upset_silent2() ->
    [
        {wait, 10},
        {servo, 0},
        {wait, 26},
        {servo, 100},
        {wait, 641},
        {servo, 40},
        {wait, 411},
        {servo, 99},
        {wait, 422},
        {servo, 39},
        {wait, 490},
        {servo, 100},
        {wait, 458},
        {servo, 0}
    ].

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
