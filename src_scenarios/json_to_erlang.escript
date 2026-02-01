#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noinput

%% SPDX-License-Identifier: Apache-2.0

%% JSON to Erlang Scenario HRL Generator
%%
%% Converts choreography sequences from JSON format to an Erlang header file
%% containing all choreographies as separate exported functions, plus
%% statically generated count/1 and get/2 implementations.
%% Requires OTP 27+ for json:decode/1.

-mode(compile).

main([]) ->
    main(["src_scenarios/choreographies.json", "_build/generated/la_machine_scenarios.hrl"]);
main([JsonFile]) ->
    main([JsonFile, "_build/generated/la_machine_scenarios.hrl"]);
main([JsonFile, HrlFile]) ->
    ScriptFile = escript:script_name(),
    case needs_rebuild(JsonFile, ScriptFile, HrlFile) of
        false ->
            ok;
        true ->
            convert(JsonFile, HrlFile)
    end;
main(_) ->
    io:format(standard_error, "Usage: json_to_erlang.escript [json_file] [hrl_file]~n", []),
    halt(1).

needs_rebuild(JsonFile, ScriptFile, HrlFile) ->
    HrlMtime = filelib:last_modified(HrlFile),
    case HrlMtime of
        0 ->
            true;
        _ ->
            JsonMtime = filelib:last_modified(JsonFile),
            ScriptMtime = filelib:last_modified(ScriptFile),
            JsonMtime > HrlMtime orelse ScriptMtime > HrlMtime
    end.

convert(JsonFile, HrlFile) ->
    io:format("Converting choreographies: ~s -> ~s~n", [JsonFile, HrlFile]),
    {ok, JsonBin} = file:read_file(JsonFile),
    Choreographies = json:decode(JsonBin),
    Count = map_size(Choreographies),
    io:format("Loaded ~b choreographies~n", [Count]),
    case Count of
        0 ->
            io:format(standard_error, "No choreographies found~n", []),
            halt(1);
        _ ->
            Blocks = parse_choreographies(Choreographies),
            io:format("Converted ~b choreography blocks~n", [length(Blocks)]),
            HrlCode = generate_hrl(Blocks, "scenario_"),
            ok = filelib:ensure_dir(HrlFile),
            ok = file:write_file(HrlFile, HrlCode),
            io:format("Generated ~s~n", [HrlFile])
    end.

%% ---------------------------------------------------------------------------
%% JSON -> blocks
%% ---------------------------------------------------------------------------

parse_choreographies(Map) ->
    Pairs = maps:to_list(Map),
    lists:flatmap(
        fun({NameBin, CommandsBin}) ->
            Name = binary_to_list(NameBin),
            Commands = binary_to_list(CommandsBin),
            handle_special(Name, Commands)
        end,
        Pairs
    ).

handle_special(Name, Commands) ->
    case string:find(Commands, " ], [ ") of
        nomatch ->
            case format_commands(Commands) of
                "" -> [];
                Formatted -> [{Name, Formatted}]
            end;
        _ ->
            Parts = string:split(Commands, " ], [ ", all),
            Total = length(Parts),
            lists:filtermap(
                fun({Ix, Part}) ->
                    Cleaned = clean_part(Part, Ix, Total),
                    BlockName =
                        case Total of
                            1 -> Name;
                            _ -> Name ++ "_part_" ++ integer_to_list(Ix)
                        end,
                    case format_commands(Cleaned) of
                        "" -> false;
                        Formatted -> {true, {BlockName, Formatted}}
                    end
                end,
                lists:zip(lists:seq(1, Total), Parts)
            )
    end.

clean_part(Part, Ix, Total) ->
    P1 =
        case Ix of
            1 -> string:trim(Part, leading, " [");
            _ -> Part
        end,
    case Ix of
        Total -> string:trim(P1, trailing, " ]");
        _ -> P1
    end.

format_commands(Commands) ->
    case string:trim(Commands) of
        "" ->
            "";
        Trimmed ->
            Collapsed = re:replace(Trimmed, "\\s+", " ", [global, {return, list}]),
            re:replace(Collapsed, ",\\s*$", "", [{return, list}])
    end.

%% ---------------------------------------------------------------------------
%% Type grouping
%% ---------------------------------------------------------------------------

%% Determine the type for each scenario name using prefix matching.
%% The type is the longest _-boundary prefix shared with at least one other
%% name. If no shared prefix exists, the type is the first word.
group_by_type(Names) ->
    Sorted = lists:sort(Names),
    lists:foldl(
        fun(Name, TypeMap) ->
            Type = find_type(Name, Sorted),
            maps:update_with(Type, fun(L) -> [Name | L] end, [Name], TypeMap)
        end,
        #{},
        Sorted
    ).

find_type(Name, AllNames) ->
    Prefixes = underscore_prefixes(Name),
    find_shared_prefix(lists:reverse(Prefixes), Name, AllNames).

%% Build all _-boundary prefixes of a name, shortest first.
%% "game_long_03765" -> ["game", "game_long"]
underscore_prefixes(Name) ->
    Parts = string:split(Name, "_", all),
    build_prefixes(Parts, []).

build_prefixes([_], Acc) ->
    lists:reverse(Acc);
build_prefixes([Part | Rest], []) ->
    build_prefixes(Rest, [Part]);
build_prefixes([Part | Rest], [Prev | _] = Acc) ->
    build_prefixes(Rest, [Prev ++ "_" ++ Part | Acc]).

%% Try prefixes from longest to shortest, return the first one shared
%% with at least one other name.
find_shared_prefix([], Name, _AllNames) ->
    %% No shared prefix found, use first word
    [First | _] = string:split(Name, "_"),
    First;
find_shared_prefix([Prefix | Rest], Name, AllNames) ->
    PrefixUnderscore = Prefix ++ "_",
    PrefixULen = length(PrefixUnderscore),
    HasMatch = lists:any(
        fun(N) ->
            N =/= Name andalso
                lists:sublist(N, PrefixULen) =:= PrefixUnderscore
        end,
        AllNames
    ),
    case HasMatch of
        true -> Prefix;
        false -> find_shared_prefix(Rest, Name, AllNames)
    end.

%% ---------------------------------------------------------------------------
%% HRL generation
%% ---------------------------------------------------------------------------

generate_hrl(Blocks, Prefix) ->
    Sorted = lists:sort(Blocks),
    NonEmpty = [{N, C} || {N, C} <- Sorted, C =/= ""],
    Names = [N || {N, _} <- NonEmpty],
    TypeMap = group_by_type(Names),
    Functions = generate_functions(NonEmpty, Prefix),
    CountClauses = generate_count(TypeMap),
    GetClauses = generate_get(TypeMap, Prefix),
    iolist_to_binary([
        "%% Generated file - do not edit\n"
        "%% Generated from choreographies.json by json_to_erlang.escript\n"
        "\n",
        Functions,
        "\n",
        CountClauses,
        "\n",
        GetClauses
    ]).

generate_functions(Blocks, Prefix) ->
    FuncBlocks = lists:map(
        fun({Name, Commands}) ->
            [
                Prefix,
                Name,
                "() ->\n"
                "    [\n"
                "        ",
                Commands,
                "\n"
                "    ].\n"
            ]
        end,
        Blocks
    ),
    lists:join("\n", FuncBlocks).

generate_count(TypeMap) ->
    Types = lists:sort(maps:to_list(TypeMap)),
    Clauses = lists:map(
        fun({Type, Names}) ->
            ["count(", Type, ") -> ", integer_to_list(length(Names)), ";\n"]
        end,
        Types
    ),
    [Clauses, "count(_) -> 0.\n"].

generate_get(TypeMap, Prefix) ->
    Types = lists:sort(maps:to_list(TypeMap)),
    Clauses = lists:flatmap(
        fun({Type, Names}) ->
            SortedNames = lists:sort(Names),
            lists:map(
                fun({Ix, Name}) ->
                    [
                        "get(",
                        Type,
                        ", ",
                        integer_to_list(Ix),
                        ") -> ",
                        Prefix,
                        Name,
                        "();\n"
                    ]
                end,
                lists:zip(lists:seq(1, length(SortedNames)), SortedNames)
            )
        end,
        Types
    ),
    [Clauses, "get(_, _) -> [].\n"].
