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
	scenario_calling_00099/0,
	scenario_calling_00140/0,
	scenario_calling_00145/0,
	scenario_calling_00368/0,
	scenario_calling_00384/0,
	scenario_calling_00393/0,
	scenario_calling_00411/0,
	scenario_calling_00416/0,
	scenario_calling_00462/0,
	scenario_calling_00467/0,
	scenario_calling_00557/0,
	scenario_calling_00573/0,
	scenario_excited_00188/0,
	scenario_excited_00425/0,
	scenario_excited_00438/0,
	scenario_excited_00535/0,
	scenario_excited_00603/0,
	scenario_excited_00784/0,
	scenario_excited_00806/0,
	scenario_excited_00812/0,
	scenario_excited_00830/0,
	scenario_excited_00946/0,
	scenario_excited_01015/0,
	scenario_excited_01021/0,
	scenario_excited_01029/0,
	scenario_excited_01041/0,
	scenario_excited_01050/0,
	scenario_excited_01062/0,
	scenario_excited_01067/0,
	scenario_excited_01088/0,
	scenario_excited_01318/0,
	scenario_excited_01327/0,
	scenario_game_long_03765/0,
	scenario_game_long_04086/0,
	scenario_game_long_04239/0,
	scenario_game_long_04843/0,
	scenario_game_long_04933/0,
	scenario_game_long_05030/0,
	scenario_game_medium_02388/0,
	scenario_game_medium_02525/0,
	scenario_game_medium_02537/0,
	scenario_game_medium_02686/0,
	scenario_game_medium_02821/0,
	scenario_game_medium_02865/0,
	scenario_game_medium_02907/0,
	scenario_game_medium_02933/0,
	scenario_game_medium_03018/0,
	scenario_game_medium_03063/0,
	scenario_game_medium_03077/0,
	scenario_game_medium_03079/0,
	scenario_game_medium_03130/0,
	scenario_game_medium_03147/0,
	scenario_game_medium_03149/0,
	scenario_game_medium_03223/0,
	scenario_game_medium_03385/0,
	scenario_game_short_00642/0,
	scenario_game_short_00671/0,
	scenario_game_short_00696/0,
	scenario_game_short_00707/0,
	scenario_game_short_00879/0,
	scenario_game_short_00880/0,
	scenario_game_short_00886/0,
	scenario_game_short_00961/0,
	scenario_game_short_00992/0,
	scenario_game_short_01000/0,
	scenario_game_short_01004/0,
	scenario_game_short_01065/0,
	scenario_game_short_01120/0,
	scenario_game_short_01257/0,
	scenario_game_short_01258/0,
	scenario_game_short_01300/0,
	scenario_game_short_01303/0,
	scenario_game_short_01408/0,
	scenario_game_short_01409/0,
	scenario_game_short_01427/0,
	scenario_game_short_01439/0,
	scenario_game_short_01452/0,
	scenario_game_short_01477/0,
	scenario_hits_00082/0,
	scenario_hits_00117/0,
	scenario_hits_00155/0,
	scenario_hits_00178/0,
	scenario_hits_00183/0,
	scenario_hits_00194/0,
	scenario_hits_00204/0,
	scenario_hits_00212/0,
	scenario_hits_00224/0,
	scenario_hits_00227/0,
	scenario_hits_00231/0,
	scenario_hits_00234/0,
	scenario_hits_00235/0,
	scenario_hits_00247/0,
	scenario_hits_00249/0,
	scenario_hits_00973/0,
	scenario_hits_00977/0,
	scenario_hits_00990/0,
	scenario_hits_00999/0,
	scenario_hits_01000/0,
	scenario_hits_01012/0,
	scenario_hits_01027/0,
	scenario_hits_01046/0,
	scenario_hits_01071/0,
	scenario_hits_01076/0,
	scenario_hits_01124/0,
	scenario_hits_01131/0,
	scenario_hits_01136/0,
	scenario_hits_01157/0,
	scenario_hits_01160/0,
	scenario_hits_01167/0,
	scenario_hits_01176/0,
	scenario_hits_01181/0,
	scenario_hits_01193/0,
	scenario_hits_01201/0,
	scenario_hits_01234/0,
	scenario_hits_01237/0,
	scenario_hits_01250/0,
	scenario_hits_01288/0,
	scenario_hits_01293/0,
	scenario_hits_01304/0,
	scenario_hits_01327/0,
	scenario_hits_01387/0,
	scenario_hits_01475/0,
	scenario_joy_00958/0,
	scenario_joy_00988/0,
	scenario_joy_00992/0,
	scenario_joy_00994/0,
	scenario_joy_00996/0,
	scenario_joy_01126/0,
	scenario_joy_01143/0,
	scenario_joy_01151/0,
	scenario_joy_01164/0,
	scenario_joy_01167/0,
	scenario_joy_01191/0,
	scenario_joy_01197/0,
	scenario_joy_01200/0,
	scenario_joy_01218/0,
	scenario_joy_01237/0,
	scenario_joy_01242/0,
	scenario_joy_01251/0,
	scenario_joy_01263/0,
	scenario_joy_01280/0,
	scenario_joy_01320/0,
	scenario_joy_01321/0,
	scenario_joy_01367/0,
	scenario_joy_01391/0,
	scenario_joy_01394/0,
	scenario_joy_01417/0,
	scenario_joy_01447/0,
	scenario_joy_01453/0,
	scenario_joy_01455/0,
	scenario_joy_01458/0,
	scenario_joy_01490/0,
	scenario_joy_01493/0,
	scenario_joy_01502/0,
	scenario_joy_01510/0,
	scenario_joy_01524/0,
	scenario_joy_01526/0,
	scenario_joy_01548/0,
	scenario_joy_01554/0,
	scenario_joy_01961/0,
	scenario_joy_02028/0,
	scenario_joy_02033/0,
	scenario_joy_02042/0,
	scenario_joy_02045/0,
	scenario_joy_02065/0,
	scenario_joy_02110/0,
	scenario_joy_02220/0,
	scenario_joy_02275/0,
	scenario_joy_02390/0,
	scenario_joy_02397/0,
	scenario_joy_02805/0,
	scenario_joy_03367/0,
	scenario_meuh_meuh_V2/0,
	scenario_poke_00186/0,
	scenario_poke_00192/0,
	scenario_poke_00202/0,
	scenario_poke_00263/0,
	scenario_poke_00275/0,
	scenario_poke_00445/0,
	scenario_poke_00501/0,
	scenario_poke_00522/0,
	scenario_poke_00533/0,
	scenario_poke_00535/0,
	scenario_poke_00612/0,
	scenario_poke_00638/0,
	scenario_poke_00698/0,
	scenario_poke_00699/0,
	scenario_poke_00719/0,
	scenario_poke_00953/0,
	scenario_poke_01042/0,
	scenario_poke_01134/0,
	scenario_test_1/0,
	scenario_tired_01006/0,
	scenario_tired_01164/0,
	scenario_tired_01247/0,
	scenario_tired_01312/0,
	scenario_tired_01360/0,
	scenario_tired_01536/0,
	scenario_tired_01544/0,
	scenario_tired_01582/0,
	scenario_tired_01583/0,
	scenario_tired_01607/0,
	scenario_tired_02519/0,
	scenario_tired_02523/0,
	scenario_tired_02550/0,
	scenario_tired_02578/0,
	scenario_tired_02657/0,
	scenario_tired_02677/0,
	scenario_tired_02821/0,
	scenario_tired_02825/0,
	scenario_tired_02892/0,
	scenario_tired_02903/0,
	scenario_tired_02985/0,
	scenario_tired_03087/0,
	scenario_tired_03122/0,
	scenario_tired_03543/0,
	scenario_tired_03595/0,
	scenario_tired_03624/0,
	scenario_tired_03825/0,
	scenario_tired_03887/0,
	scenario_tired_04327/0,
	scenario_upset_00144/0,
	scenario_upset_00414/0,
	scenario_upset_00582/0,
	scenario_upset_00588/0,
	scenario_upset_00694/0,
	scenario_upset_00742/0,
	scenario_upset_00767/0,
	scenario_upset_00803/0,
	scenario_upset_00809/0,
	scenario_upset_00839/0,
	scenario_upset_00878/0,
	scenario_upset_00930/0,
	scenario_upset_00968/0,
	scenario_upset_01559/0,
	scenario_upset_01583/0,
	scenario_upset_01594/0,
	scenario_upset_01603/0,
	scenario_upset_01608/0,
	scenario_upset_01631/0,
	scenario_upset_01719/0,
	scenario_upset_01763/0,
	scenario_upset_01778/0,
	scenario_upset_01779/0,
	scenario_upset_01938/0,
	scenario_upset_01940/0,
	scenario_upset_01968/0,
	scenario_upset_02032/0,
	scenario_upset_02137/0,
	scenario_upset_02143/0,
	scenario_upset_02175/0,
	scenario_upset_02245/0,
	scenario_upset_02249/0,
	scenario_upset_02374/0,
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
    case AllOfType of
        [] -> [];
        _ ->
            Sorted = lists:sort(AllOfType),
            Selected = lists:nth(Ith, Sorted),
            ?MODULE:Selected()
    end.

scenario_calling_00099() ->
	[
		{aac, <<"calling/00099.aac">>}
	].

scenario_calling_00140() ->
	[
		{aac, <<"calling/00140.aac">>}
	].

scenario_calling_00145() ->
	[
		{aac, <<"calling/00145.aac">>}
	].

scenario_calling_00368() ->
	[
		{aac, <<"calling/00368.aac">>}
	].

scenario_calling_00384() ->
	[
		{aac, <<"calling/00384.aac">>}
	].

scenario_calling_00393() ->
	[
		{aac, <<"calling/00393.aac">>}
	].

scenario_calling_00411() ->
	[
		{aac, <<"calling/00411.aac">>}
	].

scenario_calling_00416() ->
	[
		{aac, <<"calling/00416.aac">>}
	].

scenario_calling_00462() ->
	[
		{aac, <<"calling/00462.aac">>}
	].

scenario_calling_00467() ->
	[
		{aac, <<"calling/00467.aac">>}
	].

scenario_calling_00557() ->
	[
		{aac, <<"calling/00557.aac">>}
	].

scenario_calling_00573() ->
	[
		{aac, <<"calling/00573.aac">>}
	].

scenario_excited_00188() ->
	[
		{servo, 30}, {wait, 173}, {servo, 0}, {wait, 133}, {aac, <<"excited/00188.aac">>}, {wait, 41}, {servo, 30}
	].

scenario_excited_00425() ->
	[
		{servo, 25}, {wait, 138}, {servo, 0}, {wait, 145}, {servo, 25}, {wait, 145}, {servo, 0}, {wait, 144}, {servo, 25}, {wait, 18}, {aac, <<"excited/00425.aac">>}
	].

scenario_excited_00438() ->
	[
		{aac, <<"excited/00438.aac">>}, {wait, 11}, {servo, 50}, {wait, 272}, {servo, 25, 313}
	].

scenario_excited_00535() ->
	[
		{aac, <<"excited/00535.aac">>}, {servo, 19}, {wait, 162}, {servo, 8}, {wait, 109}, {servo, 20}, {wait, 110}, {servo, 8}, {wait, 116}, {servo, 20}, {wait, 116}, {servo, 7}, {wait, 150}, {servo, 20}
	].

scenario_excited_00603() ->
	[
		{wait, 23}, {servo, 20}, {wait, 139}, {servo, 0}, {wait, 138}, {servo, 24}, {wait, 134}, {servo, 8}, {wait, 115}, {servo, 25}, {wait, 104}, {servo, 6}, {wait, 128}, {servo, 25}, {wait, 98}, {servo, 11}, {wait, 87}, {servo, 50}, {wait, 33}, {aac, <<"excited/00603.aac">>}, {wait, 297}, {servo, 12, 370}
	].

scenario_excited_00784() ->
	[
		{aac, <<"excited/00784.aac">>}, {wait, 40}, {servo, 75}, {wait, 405}, {servo, 12, 533}
	].

scenario_excited_00806() ->
	[
		{wait, 17}, {servo, 18}, {wait, 162}, {servo, 12}, {wait, 127}, {servo, 17}, {wait, 128}, {servo, 11}, {wait, 127}, {servo, 17}, {wait, 139}, {servo, 12}, {wait, 115}, {aac, <<"excited/00806.aac">>}, {wait, 47}, {servo, 19}, {wait, 110}, {servo, 11}, {wait, 127}, {servo, 73}, {wait, 631}, {servo, 10}
	].

scenario_excited_00812() ->
	[
		{wait, 23}, {servo, 78}, {wait, 75}, {aac, <<"excited/00812.aac">>}, {wait, 613}, {servo, 78}, {wait, 64}, {servo, 11}
	].

scenario_excited_00830() ->
	[
		{aac, <<"excited/00830.aac">>}, {wait, 40}, {servo, 37}, {wait, 486}, {servo, 65}, {wait, 353}, {servo, 83}
	].

scenario_excited_00946() ->
	[
		{wait, 23}, {servo, 83}, {wait, 237}, {aac, <<"excited/00946.aac">>}, {wait, 191}, {servo, 73}, {wait, 110}, {servo, 82}, {wait, 81}, {servo, 72}, {wait, 92}, {servo, 83}, {wait, 87}, {servo, 72}, {wait, 87}, {servo, 81}, {wait, 87}, {servo, 71}, {wait, 92}, {servo, 81}, {wait, 70}, {servo, 15}
	].

scenario_excited_01015() ->
	[
		{servo, 39}, {wait, 57}, {aac, <<"excited/01015.aac">>}, {wait, 203}, {servo, 53}, {wait, 145}, {servo, 61}, {wait, 121}, {servo, 72}, {wait, 127}, {servo, 82}, {wait, 278}, {servo, 25}
	].

scenario_excited_01021() ->
	[
		{aac, <<"excited/01021.aac">>}, {wait, 17}, {servo, 24}, {wait, 127}, {servo, 14}, {wait, 64}, {servo, 36}, {wait, 145}, {servo, 25}, {wait, 63}, {servo, 46}, {wait, 116}, {servo, 32}, {wait, 93}, {servo, 59}, {wait, 156}, {servo, 45}, {wait, 104}, {servo, 71}
	].

scenario_excited_01029() ->
	[
		{wait, 23}, {servo, 17}, {wait, 23}, {aac, <<"excited/01029.aac">>}, {wait, 75}, {servo, 8}, {wait, 75}, {servo, 22}, {wait, 81}, {servo, 7}, {wait, 110}, {servo, 22}, {wait, 99}, {servo, 7}, {wait, 81}, {servo, 82}, {wait, 654}, {servo, 11}
	].

scenario_excited_01041() ->
	[
		{wait, 23}, {servo, 82}, {wait, 81}, {aac, <<"excited/01041.aac">>}, {wait, 318}, {servo, 50}, {wait, 168}, {servo, 61}, {wait, 58}, {servo, 49}, {wait, 81}, {servo, 60}, {wait, 58}, {servo, 48}, {wait, 69}, {servo, 59}, {wait, 69}, {servo, 46}, {wait, 87}, {servo, 59}, {wait, 81}, {servo, 46}, {wait, 81}, {servo, 58}, {wait, 93}, {servo, 45}
	].

scenario_excited_01050() ->
	[
		{aac, <<"excited/01050.aac">>}, {wait, 34}, {servo, 84}, {wait, 411}, {servo, 76}, {wait, 70}, {servo, 84}, {wait, 63}, {servo, 75}, {wait, 75}, {servo, 84}, {wait, 53}, {servo, 75}, {wait, 69}, {servo, 84}, {wait, 64}, {servo, 76}, {wait, 63}, {servo, 84}, {wait, 52}, {servo, 77}, {wait, 64}, {servo, 85}, {wait, 58}, {servo, 75}, {wait, 64}, {servo, 85, 57}, {wait, 75}, {servo, 75}
	].

scenario_excited_01062() ->
	[
		{aac, <<"excited/01062.aac">>}, {wait, 46}, {servo, 23}, {wait, 144}, {servo, 11}, {wait, 99}, {servo, 23}, {wait, 98}, {servo, 11}, {wait, 81}, {servo, 83}, {wait, 388}, {servo, 3}
	].

scenario_excited_01067() ->
	[
		{aac, <<"excited/01067.aac">>}, {wait, 28}, {servo, 82}, {wait, 429}, {servo, 56}, {wait, 168}, {servo, 81}, {wait, 376}, {servo, 49}, {wait, 202}, {servo, 59}
	].

scenario_excited_01088() ->
	[
		{aac, <<"excited/01088.aac">>}, {wait, 23}, {servo, 36}, {wait, 185}, {servo, 8}, {wait, 145}, {servo, 24}, {wait, 75}, {servo, 8}, {wait, 92}, {servo, 83}, {wait, 446}, {servo, 9}
	].

scenario_excited_01318() ->
	[
		{aac, <<"excited/01318.aac">>}, {wait, 17}, {servo, 25}, {wait, 139}, {servo, 7}, {wait, 104}, {servo, 23}, {wait, 93}, {servo, 6}, {wait, 104}, {servo, 24}, {wait, 110}, {servo, 7}, {wait, 104}, {servo, 83}, {wait, 503}, {servo, 7}
	].

scenario_excited_01327() ->
	[
		{aac, <<"excited/01327.aac">>}, {wait, 23}, {servo, 20}, {wait, 110}, {servo, 12}, {wait, 57}, {servo, 19}, {wait, 53}, {servo, 11}, {wait, 63}, {servo, 20}, {wait, 52}, {servo, 9}, {wait, 70}, {servo, 20}, {wait, 75}, {servo, 10}, {wait, 75}, {servo, 18}, {wait, 70}, {servo, 10}, {wait, 86}, {servo, 18}, {wait, 64}, {servo, 9}, {wait, 64}, {servo, 18}, {wait, 69}, {servo, 9}, {wait, 70}, {servo, 82}, {wait, 411}, {servo, 10}
	].

scenario_game_long_03765() ->
	[
		{aac, <<"game/03765.aac">>}
	].

scenario_game_long_04086() ->
	[
		{aac, <<"game/04086.aac">>}
	].

scenario_game_long_04239() ->
	[
		{aac, <<"game/04239.aac">>}
	].

scenario_game_long_04843() ->
	[
		{aac, <<"game/04843.aac">>}
	].

scenario_game_long_04933() ->
	[
		{aac, <<"game/04933.aac">>}
	].

scenario_game_long_05030() ->
	[
		{aac, <<"game/05030.aac">>}
	].

scenario_game_medium_02388() ->
	[
		{aac, <<"game/02388.aac">>}
	].

scenario_game_medium_02525() ->
	[
		{aac, <<"game/02525.aac">>}
	].

scenario_game_medium_02537() ->
	[
		{aac, <<"game/02537.aac">>}
	].

scenario_game_medium_02686() ->
	[
		{aac, <<"game/02686.aac">>}
	].

scenario_game_medium_02821() ->
	[
		{aac, <<"game/02821.aac">>}
	].

scenario_game_medium_02865() ->
	[
		{aac, <<"game/02865.aac">>}
	].

scenario_game_medium_02907() ->
	[
		{aac, <<"game/02907.aac">>}
	].

scenario_game_medium_02933() ->
	[
		{aac, <<"game/02933.aac">>}
	].

scenario_game_medium_03018() ->
	[
		{aac, <<"game/03018.aac">>}
	].

scenario_game_medium_03063() ->
	[
		{aac, <<"game/03063.aac">>}
	].

scenario_game_medium_03077() ->
	[
		{aac, <<"game/03077.aac">>}
	].

scenario_game_medium_03079() ->
	[
		{aac, <<"game/03079.aac">>}
	].

scenario_game_medium_03130() ->
	[
		{aac, <<"game/03130.aac">>}
	].

scenario_game_medium_03147() ->
	[
		{aac, <<"game/03147.aac">>}
	].

scenario_game_medium_03149() ->
	[
		{aac, <<"game/03149.aac">>}
	].

scenario_game_medium_03223() ->
	[
		{aac, <<"game/03223.aac">>}
	].

scenario_game_medium_03385() ->
	[
		{aac, <<"game/03385.aac">>}
	].

scenario_game_short_00642() ->
	[
		{aac, <<"game/00642.aac">>}
	].

scenario_game_short_00671() ->
	[
		{aac, <<"game/00671.aac">>}
	].

scenario_game_short_00696() ->
	[
		{aac, <<"game/00696.aac">>}
	].

scenario_game_short_00707() ->
	[
		{aac, <<"game/00707.aac">>}
	].

scenario_game_short_00879() ->
	[
		{aac, <<"game/00879.aac">>}
	].

scenario_game_short_00880() ->
	[
		{aac, <<"game/00880.aac">>}
	].

scenario_game_short_00886() ->
	[
		{aac, <<"game/00886.aac">>}
	].

scenario_game_short_00961() ->
	[
		{aac, <<"game/00961.aac">>}
	].

scenario_game_short_00992() ->
	[
		{aac, <<"game/00992.aac">>}
	].

scenario_game_short_01000() ->
	[
		{aac, <<"game/01000.aac">>}
	].

scenario_game_short_01004() ->
	[
		{aac, <<"game/01004.aac">>}
	].

scenario_game_short_01065() ->
	[
		{aac, <<"game/01065.aac">>}
	].

scenario_game_short_01120() ->
	[
		{aac, <<"game/01120.aac">>}
	].

scenario_game_short_01257() ->
	[
		{aac, <<"game/01257.aac">>}
	].

scenario_game_short_01258() ->
	[
		{aac, <<"game/01258.aac">>}
	].

scenario_game_short_01300() ->
	[
		{aac, <<"game/01300.aac">>}
	].

scenario_game_short_01303() ->
	[
		{aac, <<"game/01303.aac">>}
	].

scenario_game_short_01408() ->
	[
		{aac, <<"game/01408.aac">>}
	].

scenario_game_short_01409() ->
	[
		{aac, <<"game/01409.aac">>}
	].

scenario_game_short_01427() ->
	[
		{aac, <<"game/01427.aac">>}
	].

scenario_game_short_01439() ->
	[
		{aac, <<"game/01439.aac">>}
	].

scenario_game_short_01452() ->
	[
		{aac, <<"game/01452.aac">>}
	].

scenario_game_short_01477() ->
	[
		{aac, <<"game/01477.aac">>}
	].

scenario_hits_00082() ->
	[
		{aac, <<"hits/00082.aac">>}
	].

scenario_hits_00117() ->
	[
		{aac, <<"hits/00117.aac">>}
	].

scenario_hits_00155() ->
	[
		{aac, <<"hits/00155.aac">>}
	].

scenario_hits_00178() ->
	[
		{aac, <<"hits/00178.aac">>}
	].

scenario_hits_00183() ->
	[
		{aac, <<"hits/00183.aac">>}
	].

scenario_hits_00194() ->
	[
		{aac, <<"hits/00194.aac">>}
	].

scenario_hits_00204() ->
	[
		{aac, <<"hits/00204.aac">>}
	].

scenario_hits_00212() ->
	[
		{aac, <<"hits/00212.aac">>}
	].

scenario_hits_00224() ->
	[
		{aac, <<"hits/00224.aac">>}
	].

scenario_hits_00227() ->
	[
		{aac, <<"hits/00227.aac">>}
	].

scenario_hits_00231() ->
	[
		{aac, <<"hits/00231.aac">>}
	].

scenario_hits_00234() ->
	[
		{aac, <<"hits/00234.aac">>}
	].

scenario_hits_00235() ->
	[
		{aac, <<"hits/00235.aac">>}
	].

scenario_hits_00247() ->
	[
		{aac, <<"hits/00247.aac">>}
	].

scenario_hits_00249() ->
	[
		{aac, <<"hits/00249.aac">>}
	].

scenario_hits_00973() ->
	[
		{aac, <<"hits/00973.aac">>}
	].

scenario_hits_00977() ->
	[
		{aac, <<"hits/00977.aac">>}
	].

scenario_hits_00990() ->
	[
		{aac, <<"hits/00990.aac">>}
	].

scenario_hits_00999() ->
	[
		{aac, <<"hits/00999.aac">>}
	].

scenario_hits_01000() ->
	[
		{aac, <<"hits/01000.aac">>}
	].

scenario_hits_01012() ->
	[
		{aac, <<"hits/01012.aac">>}
	].

scenario_hits_01027() ->
	[
		{aac, <<"hits/01027.aac">>}
	].

scenario_hits_01046() ->
	[
		{aac, <<"hits/01046.aac">>}
	].

scenario_hits_01071() ->
	[
		{aac, <<"hits/01071.aac">>}
	].

scenario_hits_01076() ->
	[
		{aac, <<"hits/01076.aac">>}
	].

scenario_hits_01124() ->
	[
		{aac, <<"hits/01124.aac">>}
	].

scenario_hits_01131() ->
	[
		{aac, <<"hits/01131.aac">>}
	].

scenario_hits_01136() ->
	[
		{aac, <<"hits/01136.aac">>}
	].

scenario_hits_01157() ->
	[
		{aac, <<"hits/01157.aac">>}
	].

scenario_hits_01160() ->
	[
		{aac, <<"hits/01160.aac">>}
	].

scenario_hits_01167() ->
	[
		{aac, <<"hits/01167.aac">>}
	].

scenario_hits_01176() ->
	[
		{aac, <<"hits/01176.aac">>}
	].

scenario_hits_01181() ->
	[
		{aac, <<"hits/01181.aac">>}
	].

scenario_hits_01193() ->
	[
		{aac, <<"hits/01193.aac">>}
	].

scenario_hits_01201() ->
	[
		{aac, <<"hits/01201.aac">>}
	].

scenario_hits_01234() ->
	[
		{aac, <<"hits/01234.aac">>}
	].

scenario_hits_01237() ->
	[
		{aac, <<"hits/01237.aac">>}
	].

scenario_hits_01250() ->
	[
		{aac, <<"hits/01250.aac">>}
	].

scenario_hits_01288() ->
	[
		{aac, <<"hits/01288.aac">>}
	].

scenario_hits_01293() ->
	[
		{aac, <<"hits/01293.aac">>}
	].

scenario_hits_01304() ->
	[
		{aac, <<"hits/01304.aac">>}
	].

scenario_hits_01327() ->
	[
		{aac, <<"hits/01327.aac">>}
	].

scenario_hits_01387() ->
	[
		{aac, <<"hits/01387.aac">>}
	].

scenario_hits_01475() ->
	[
		{aac, <<"hits/01475.aac">>}
	].

scenario_joy_00958() ->
	[
		{aac, <<"joy/00958.aac">>}
	].

scenario_joy_00988() ->
	[
		{aac, <<"joy/00988.aac">>}
	].

scenario_joy_00992() ->
	[
		{aac, <<"joy/00992.aac">>}
	].

scenario_joy_00994() ->
	[
		{aac, <<"joy/00994.aac">>}
	].

scenario_joy_00996() ->
	[
		{aac, <<"joy/00996.aac">>}
	].

scenario_joy_01126() ->
	[
		{aac, <<"joy/01126.aac">>}
	].

scenario_joy_01143() ->
	[
		{aac, <<"joy/01143.aac">>}
	].

scenario_joy_01151() ->
	[
		{aac, <<"joy/01151.aac">>}
	].

scenario_joy_01164() ->
	[
		{aac, <<"joy/01164.aac">>}
	].

scenario_joy_01167() ->
	[
		{aac, <<"joy/01167.aac">>}
	].

scenario_joy_01191() ->
	[
		{aac, <<"joy/01191.aac">>}
	].

scenario_joy_01197() ->
	[
		{aac, <<"joy/01197.aac">>}
	].

scenario_joy_01200() ->
	[
		{aac, <<"joy/01200.aac">>}
	].

scenario_joy_01218() ->
	[
		{aac, <<"joy/01218.aac">>}
	].

scenario_joy_01237() ->
	[
		{aac, <<"joy/01237.aac">>}
	].

scenario_joy_01242() ->
	[
		{aac, <<"joy/01242.aac">>}
	].

scenario_joy_01251() ->
	[
		{aac, <<"joy/01251.aac">>}
	].

scenario_joy_01263() ->
	[
		{aac, <<"joy/01263.aac">>}
	].

scenario_joy_01280() ->
	[
		{aac, <<"joy/01280.aac">>}
	].

scenario_joy_01320() ->
	[
		{aac, <<"joy/01320.aac">>}
	].

scenario_joy_01321() ->
	[
		{aac, <<"joy/01321.aac">>}
	].

scenario_joy_01367() ->
	[
		{aac, <<"joy/01367.aac">>}
	].

scenario_joy_01391() ->
	[
		{aac, <<"joy/01391.aac">>}
	].

scenario_joy_01394() ->
	[
		{aac, <<"joy/01394.aac">>}
	].

scenario_joy_01417() ->
	[
		{aac, <<"joy/01417.aac">>}
	].

scenario_joy_01447() ->
	[
		{aac, <<"joy/01447.aac">>}
	].

scenario_joy_01453() ->
	[
		{aac, <<"joy/01453.aac">>}
	].

scenario_joy_01455() ->
	[
		{aac, <<"joy/01455.aac">>}
	].

scenario_joy_01458() ->
	[
		{aac, <<"joy/01458.aac">>}
	].

scenario_joy_01490() ->
	[
		{aac, <<"joy/01490.aac">>}
	].

scenario_joy_01493() ->
	[
		{aac, <<"joy/01493.aac">>}
	].

scenario_joy_01502() ->
	[
		{aac, <<"joy/01502.aac">>}
	].

scenario_joy_01510() ->
	[
		{aac, <<"joy/01510.aac">>}
	].

scenario_joy_01524() ->
	[
		{aac, <<"joy/01524.aac">>}
	].

scenario_joy_01526() ->
	[
		{aac, <<"joy/01526.aac">>}
	].

scenario_joy_01548() ->
	[
		{aac, <<"joy/01548.aac">>}
	].

scenario_joy_01554() ->
	[
		{aac, <<"joy/01554.aac">>}
	].

scenario_joy_01961() ->
	[
		{aac, <<"joy/01961.aac">>}
	].

scenario_joy_02028() ->
	[
		{aac, <<"joy/02028.aac">>}
	].

scenario_joy_02033() ->
	[
		{aac, <<"joy/02033.aac">>}
	].

scenario_joy_02042() ->
	[
		{aac, <<"joy/02042.aac">>}
	].

scenario_joy_02045() ->
	[
		{aac, <<"joy/02045.aac">>}
	].

scenario_joy_02065() ->
	[
		{aac, <<"joy/02065.aac">>}
	].

scenario_joy_02110() ->
	[
		{aac, <<"joy/02110.aac">>}
	].

scenario_joy_02220() ->
	[
		{aac, <<"joy/02220.aac">>}
	].

scenario_joy_02275() ->
	[
		{aac, <<"joy/02275.aac">>}
	].

scenario_joy_02390() ->
	[
		{aac, <<"joy/02390.aac">>}
	].

scenario_joy_02397() ->
	[
		{aac, <<"joy/02397.aac">>}
	].

scenario_joy_02805() ->
	[
		{aac, <<"joy/02805.aac">>}
	].

scenario_joy_03367() ->
	[
		{aac, <<"joy/03367.aac">>}
	].

scenario_meuh_meuh_V2() ->
	[
		{servo, 60, 2500}, {wait, 1000}, {aac, <<"meuh/meuh_V2.aac">>}, {wait, 4754}, {servo, 0, 800}
	].

scenario_poke_00186() ->
	[
		{aac, <<"poke/00186.aac">>}
	].

scenario_poke_00192() ->
	[
		{aac, <<"poke/00192.aac">>}
	].

scenario_poke_00202() ->
	[
		{aac, <<"poke/00202.aac">>}
	].

scenario_poke_00263() ->
	[
		{aac, <<"poke/00263.aac">>}
	].

scenario_poke_00275() ->
	[
		{aac, <<"poke/00275.aac">>}
	].

scenario_poke_00445() ->
	[
		{aac, <<"poke/00445.aac">>}
	].

scenario_poke_00501() ->
	[
		{aac, <<"poke/00501.aac">>}
	].

scenario_poke_00522() ->
	[
		{aac, <<"poke/00522.aac">>}
	].

scenario_poke_00533() ->
	[
		{aac, <<"poke/00533.aac">>}
	].

scenario_poke_00535() ->
	[
		{aac, <<"poke/00535.aac">>}
	].

scenario_poke_00612() ->
	[
		{aac, <<"poke/00612.aac">>}
	].

scenario_poke_00638() ->
	[
		{aac, <<"poke/00638.aac">>}
	].

scenario_poke_00698() ->
	[
		{aac, <<"poke/00698.aac">>}
	].

scenario_poke_00699() ->
	[
		{aac, <<"poke/00699.aac">>}
	].

scenario_poke_00719() ->
	[
		{aac, <<"poke/00719.aac">>}
	].

scenario_poke_00953() ->
	[
		{aac, <<"poke/00953.aac">>}
	].

scenario_poke_01042() ->
	[
		{aac, <<"poke/01042.aac">>}
	].

scenario_poke_01134() ->
	[
		{aac, <<"poke/01134.aac">>}
	].

scenario_test_1() ->
	[
		{servo, 100}, {wait, 100}, {servo, 0}
	].

scenario_tired_01006() ->
	[
		{aac, <<"tired/01006.aac">>}
	].

scenario_tired_01164() ->
	[
		{aac, <<"tired/01164.aac">>}
	].

scenario_tired_01247() ->
	[
		{aac, <<"tired/01247.aac">>}
	].

scenario_tired_01312() ->
	[
		{aac, <<"tired/01312.aac">>}
	].

scenario_tired_01360() ->
	[
		{aac, <<"tired/01360.aac">>}
	].

scenario_tired_01536() ->
	[
		{aac, <<"tired/01536.aac">>}
	].

scenario_tired_01544() ->
	[
		{aac, <<"tired/01544.aac">>}
	].

scenario_tired_01582() ->
	[
		{aac, <<"tired/01582.aac">>}
	].

scenario_tired_01583() ->
	[
		{aac, <<"tired/01583.aac">>}
	].

scenario_tired_01607() ->
	[
		{aac, <<"tired/01607.aac">>}
	].

scenario_tired_02519() ->
	[
		{aac, <<"tired/02519.aac">>}
	].

scenario_tired_02523() ->
	[
		{aac, <<"tired/02523.aac">>}
	].

scenario_tired_02550() ->
	[
		{aac, <<"tired/02550.aac">>}
	].

scenario_tired_02578() ->
	[
		{aac, <<"tired/02578.aac">>}
	].

scenario_tired_02657() ->
	[
		{aac, <<"tired/02657.aac">>}
	].

scenario_tired_02677() ->
	[
		{aac, <<"tired/02677.aac">>}
	].

scenario_tired_02821() ->
	[
		{aac, <<"tired/02821.aac">>}
	].

scenario_tired_02825() ->
	[
		{aac, <<"tired/02825.aac">>}
	].

scenario_tired_02892() ->
	[
		{aac, <<"tired/02892.aac">>}
	].

scenario_tired_02903() ->
	[
		{aac, <<"tired/02903.aac">>}
	].

scenario_tired_02985() ->
	[
		{aac, <<"tired/02985.aac">>}
	].

scenario_tired_03087() ->
	[
		{aac, <<"tired/03087.aac">>}
	].

scenario_tired_03122() ->
	[
		{aac, <<"tired/03122.aac">>}
	].

scenario_tired_03543() ->
	[
		{aac, <<"tired/03543.aac">>}
	].

scenario_tired_03595() ->
	[
		{aac, <<"tired/03595.aac">>}
	].

scenario_tired_03624() ->
	[
		{aac, <<"tired/03624.aac">>}
	].

scenario_tired_03825() ->
	[
		{aac, <<"tired/03825.aac">>}
	].

scenario_tired_03887() ->
	[
		{aac, <<"tired/03887.aac">>}
	].

scenario_tired_04327() ->
	[
		{aac, <<"tired/04327.aac">>}
	].

scenario_upset_00144() ->
	[
		{aac, <<"upset/00144.aac">>}
	].

scenario_upset_00414() ->
	[
		{aac, <<"upset/00414.aac">>}
	].

scenario_upset_00582() ->
	[
		{aac, <<"upset/00582.aac">>}
	].

scenario_upset_00588() ->
	[
		{aac, <<"upset/00588.aac">>}
	].

scenario_upset_00694() ->
	[
		{aac, <<"upset/00694.aac">>}
	].

scenario_upset_00742() ->
	[
		{aac, <<"upset/00742.aac">>}
	].

scenario_upset_00767() ->
	[
		{aac, <<"upset/00767.aac">>}
	].

scenario_upset_00803() ->
	[
		{aac, <<"upset/00803.aac">>}
	].

scenario_upset_00809() ->
	[
		{aac, <<"upset/00809.aac">>}
	].

scenario_upset_00839() ->
	[
		{aac, <<"upset/00839.aac">>}
	].

scenario_upset_00878() ->
	[
		{aac, <<"upset/00878.aac">>}
	].

scenario_upset_00930() ->
	[
		{aac, <<"upset/00930.aac">>}
	].

scenario_upset_00968() ->
	[
		{aac, <<"upset/00968.aac">>}
	].

scenario_upset_01559() ->
	[
		{aac, <<"upset/01559.aac">>}
	].

scenario_upset_01583() ->
	[
		{aac, <<"upset/01583.aac">>}
	].

scenario_upset_01594() ->
	[
		{aac, <<"upset/01594.aac">>}
	].

scenario_upset_01603() ->
	[
		{aac, <<"upset/01603.aac">>}
	].

scenario_upset_01608() ->
	[
		{aac, <<"upset/01608.aac">>}
	].

scenario_upset_01631() ->
	[
		{aac, <<"upset/01631.aac">>}
	].

scenario_upset_01719() ->
	[
		{aac, <<"upset/01719.aac">>}
	].

scenario_upset_01763() ->
	[
		{aac, <<"upset/01763.aac">>}
	].

scenario_upset_01778() ->
	[
		{aac, <<"upset/01778.aac">>}
	].

scenario_upset_01779() ->
	[
		{aac, <<"upset/01779.aac">>}
	].

scenario_upset_01938() ->
	[
		{aac, <<"upset/01938.aac">>}
	].

scenario_upset_01940() ->
	[
		{aac, <<"upset/01940.aac">>}
	].

scenario_upset_01968() ->
	[
		{aac, <<"upset/01968.aac">>}
	].

scenario_upset_02032() ->
	[
		{aac, <<"upset/02032.aac">>}
	].

scenario_upset_02137() ->
	[
		{aac, <<"upset/02137.aac">>}
	].

scenario_upset_02143() ->
	[
		{aac, <<"upset/02143.aac">>}
	].

scenario_upset_02175() ->
	[
		{aac, <<"upset/02175.aac">>}
	].

scenario_upset_02245() ->
	[
		{aac, <<"upset/02245.aac">>}
	].

scenario_upset_02249() ->
	[
		{aac, <<"upset/02249.aac">>}
	].

scenario_upset_02374() ->
	[
		{aac, <<"upset/02374.aac">>}
	].

scenario_upset_silent() ->
	[
		{servo, 100, 302}, {wait, 593}, {servo, 50}, {wait, 1391}, {servo, 0, 2026}
	].

scenario_upset_silent2() ->
	[
		{wait, 10}, {servo, 0}, {wait, 26}, {servo, 100}, {wait, 641}, {servo, 40}, {wait, 411}, {servo, 99}, {wait, 422}, {servo, 39}, {wait, 490}, {servo, 100}, {wait, 458}, {servo, 0}
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
