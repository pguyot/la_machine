#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0 -noinput

%% SPDX-License-Identifier: Apache-2.0

%% Build Assets Script
%%
%% Generates both:
%% 1. Choreographies HRL from JSON (la_machine_scenarios.hrl)
%% 2. Sounds ZIP archive and index (sounds.bin, la_machine_sounds_index.hrl)
%%
%% Auto-detects 4mb profile from REBAR_BUILD_DIR to use appropriate sources.
%% Requires OTP 27+ for json:decode/1.

-mode(compile).

-define(LOCAL_FILE_HEADER_SIG, 16#04034b50).
-define(LOCAL_FILE_HEADER_SIZE, 30).

main(_Args) ->
    case os:getenv("REBAR_ROOT_DIR") of
        false ->
            io:format(standard_error, "Error: REBAR_ROOT_DIR not set~n", []),
            erlang:halt(1);
        RootDir ->
            Is4mb = is_4mb_profile(),
            BuildDir = filename:join([RootDir, "_build", "generated"]),
            ok = filelib:ensure_dir(filename:join(BuildDir, "dummy")),

            %% Build choreographies
            JsonFile =
                case Is4mb of
                    true -> filename:join(RootDir, "choreographies-4mb.json");
                    false -> filename:join(RootDir, "choreographies.json")
                end,
            HrlFile = filename:join(BuildDir, "la_machine_scenarios.hrl"),
            ok = build_choreographies(JsonFile, HrlFile, RootDir, Is4mb),

            %% Build sounds partition
            SoundsDir =
                case Is4mb of
                    true -> filename:join(RootDir, "sounds-4mb");
                    false -> filename:join(RootDir, "sounds")
                end,
            ok = build_sounds_partition(SoundsDir, BuildDir)
    end.

%%=============================================================================
%% Profile detection
%%=============================================================================

is_4mb_profile() ->
    case os:getenv("REBAR_BUILD_DIR") of
        false ->
            false;
        BuildDir ->
            BaseName = filename:basename(BuildDir),
            string:find(BaseName, "4mb") =/= nomatch
    end.

%%=============================================================================
%% Choreographies generation
%%=============================================================================

build_choreographies(JsonFile, HrlFile, RootDir, Is4mb) ->
    ScriptFile = escript:script_name(),
    case needs_rebuild(JsonFile, ScriptFile, HrlFile) of
        false ->
            ok;
        true ->
            convert_choreographies(JsonFile, HrlFile, RootDir, Is4mb)
    end.

needs_rebuild(SourceFile, ScriptFile, HrlFile) ->
    HrlMtime = filelib:last_modified(HrlFile),
    case HrlMtime of
        0 ->
            true;
        _ ->
            %% Check if source file changed
            SourceMtime = filelib:last_modified(SourceFile),
            ScriptMtime = filelib:last_modified(ScriptFile),
            TimeChanged = SourceMtime > HrlMtime orelse ScriptMtime > HrlMtime,
            %% Check if source path changed (profile switch)
            SourceChanged = read_source_from_hrl(HrlFile) =/= SourceFile,
            TimeChanged orelse SourceChanged
    end.

read_source_from_hrl(HrlFile) ->
    case file:read_file(HrlFile) of
        {ok, Content} ->
            case re:run(Content, "^%% Source: (.+)$", [multiline, {capture, [1], list}]) of
                {match, [SourcePath]} -> SourcePath;
                nomatch -> undefined
            end;
        {error, _} ->
            undefined
    end.

convert_choreographies(JsonFile, HrlFile, RootDir, Is4mb) ->
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
            SoundsDir =
                case Is4mb of
                    true -> filename:join(RootDir, "sounds-4mb");
                    false -> filename:join(RootDir, "sounds")
                end,
            validate_choreography_sounds(Choreographies, SoundsDir),
            Blocks = parse_choreographies(Choreographies),
            io:format("Converted ~b choreography blocks~n", [length(Blocks)]),
            HrlCode = generate_hrl(Blocks, "scenario_", JsonFile),
            ok = file:write_file(HrlFile, HrlCode),
            io:format("Generated ~s~n", [HrlFile]),
            ok
    end.

validate_choreography_sounds(Choreographies, SoundsDir) ->
    SoundPaths = extract_sound_paths(Choreographies),
    UniquePaths = lists:usort(SoundPaths),
    io:format("Found ~b unique sound references~n", [length(UniquePaths)]),
    Missing = lists:filter(
        fun(SoundPath) ->
            FullPath = filename:join(SoundsDir, SoundPath),
            not filelib:is_regular(FullPath)
        end,
        UniquePaths
    ),
    case Missing of
        [] ->
            ok;
        _ ->
            io:format(standard_error, "ERROR: ~b missing sound file(s):~n", [length(Missing)]),
            lists:foreach(
                fun(Path) -> io:format(standard_error, "  - ~s~n", [Path]) end,
                Missing
            ),
            halt(1)
    end.

extract_sound_paths(Choreographies) ->
    lists:flatmap(
        fun({_Name, Commands}) ->
            extract_sounds_from_commands(binary_to_list(Commands))
        end,
        maps:to_list(Choreographies)
    ).

extract_sounds_from_commands(Commands) ->
    case re:run(Commands, "<<\"([^\"]+\\.mp3)\">>", [global, {capture, [1], list}]) of
        {match, Matches} -> [Path || [Path] <- Matches];
        nomatch -> []
    end.

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
            [{Name, format_commands(Commands)}];
        _ ->
            Parts = string:split(Commands, " ], [ ", all),
            Total = length(Parts),
            lists:map(
                fun({Ix, Part}) ->
                    Cleaned = clean_part(Part, Ix, Total),
                    BlockName =
                        case Total of
                            1 -> Name;
                            _ -> Name ++ "_part_" ++ integer_to_list(Ix)
                        end,
                    {BlockName, format_commands(Cleaned)}
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
    %% The Java editor generates scenario names ending with _\d+ (e.g.
    %% game_short_00642), so stripping that suffix gives us the type.
    %% Hand-authored silent scenarios (e.g. calling_silent, excited_silent1)
    %% don't follow this pattern, so we fall back to a longest-shared-prefix
    %% heuristic for those.
    case re:run(Name, "^(.+)_[0-9]+$", [{capture, [1], list}]) of
        {match, [Type]} ->
            Type;
        nomatch ->
            Prefixes = underscore_prefixes(Name),
            find_shared_prefix(lists:reverse(Prefixes), Name, AllNames)
    end.

underscore_prefixes(Name) ->
    Parts = string:split(Name, "_", all),
    build_prefixes(Parts, []).

build_prefixes([_], Acc) ->
    lists:reverse(Acc);
build_prefixes([Part | Rest], []) ->
    build_prefixes(Rest, [Part]);
build_prefixes([Part | Rest], [Prev | _] = Acc) ->
    build_prefixes(Rest, [Prev ++ "_" ++ Part | Acc]).

find_shared_prefix([], Name, _AllNames) ->
    [First | _] = string:split(Name, "_"),
    First;
find_shared_prefix([Prefix | Rest], Name, AllNames) ->
    PrefixUnderscore = Prefix ++ "_",
    PrefixULen = length(PrefixUnderscore),
    HasMatch = lists:any(
        fun(N) ->
            N =/= Name andalso lists:sublist(N, PrefixULen) =:= PrefixUnderscore
        end,
        AllNames
    ),
    case HasMatch of
        true -> Prefix;
        false -> find_shared_prefix(Rest, Name, AllNames)
    end.

generate_hrl(Blocks, Prefix, SourceFile) ->
    Sorted = lists:sort(Blocks),
    Names = [N || {N, _} <- Sorted],
    TypeMap = group_by_type(Names),
    Functions = generate_functions(Sorted, Prefix),
    CountClauses = generate_count(TypeMap),
    GetClauses = generate_get(TypeMap, Prefix),
    iolist_to_binary([
        "%% Generated file - do not edit\n"
        "%% Source: ",
        SourceFile,
        "\n"
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
            [Prefix, Name, "() ->\n    [\n        ", Commands, "\n    ].\n"]
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
                    ["get(", Type, ", ", integer_to_list(Ix), ") -> ", Prefix, Name, "();\n"]
                end,
                lists:zip(lists:seq(1, length(SortedNames)), SortedNames)
            )
        end,
        Types
    ),
    [Clauses, "get(_, _) -> [].\n"].

%%=============================================================================
%% Sounds partition generation
%%=============================================================================

build_sounds_partition(SoundsDir, BuildDir) ->
    OutputBin = filename:join(BuildDir, "sounds.bin"),
    OutputHrl = filename:join(BuildDir, "la_machine_sounds_index.hrl"),
    ScriptFile = escript:script_name(),
    case needs_sounds_rebuild(SoundsDir, ScriptFile, OutputBin, OutputHrl) of
        false ->
            ok;
        true ->
            do_build_sounds_partition(SoundsDir, OutputBin, OutputHrl)
    end.

needs_sounds_rebuild(SoundsDir, ScriptFile, OutputBin, OutputHrl) ->
    BinExists = filelib:is_regular(OutputBin),
    HrlExists = filelib:is_regular(OutputHrl),
    ChecksumHrl = filename:join(filename:dirname(OutputHrl), "la_machine_sounds_checksum.hrl"),
    ChecksumHrlExists = filelib:is_regular(ChecksumHrl),
    case BinExists andalso HrlExists andalso ChecksumHrlExists of
        false ->
            true;
        true ->
            %% Check if source directory changed (profile switch)
            SourceChanged = read_source_from_hrl(OutputHrl) =/= SoundsDir,
            %% Check if script changed
            HrlMtime = filelib:last_modified(OutputHrl),
            ScriptMtime = filelib:last_modified(ScriptFile),
            ScriptChanged = ScriptMtime > HrlMtime,
            %% Check if any sound file changed
            LatestSoundMtime = latest_sound_mtime(SoundsDir),
            SoundChanged = LatestSoundMtime > HrlMtime,
            SourceChanged orelse ScriptChanged orelse SoundChanged
    end.

do_build_sounds_partition(SoundsDir, OutputBin, OutputHrl) ->
    case create_sounds_zip(SoundsDir) of
        {ok, ZipBinary} ->
            Index = build_zip_index(ZipBinary),
            %% Build and write the index HRL (includes the checksum HRL)
            IndexContent = build_sounds_index_hrl_content(Index, SoundsDir),
            write_if_changed(OutputHrl, IndexContent),
            %% Compute SHA1 of the index HRL content
            Sha1 = crypto:hash(sha, IndexContent),
            %% Write sounds.bin with SHA1 appended at the end
            ChecksumOffset = byte_size(ZipBinary),
            ok = file:write_file(OutputBin, [ZipBinary, Sha1]),
            %% Write checksum HRL with SHA1 value and offset
            ChecksumHrl = filename:join(
                filename:dirname(OutputHrl), "la_machine_sounds_checksum.hrl"
            ),
            ChecksumContent = build_sounds_checksum_hrl_content(Sha1, ChecksumOffset),
            write_if_changed(ChecksumHrl, ChecksumContent),
            io:format("Generated ~s (~B bytes, checksum at offset ~B)~n", [
                OutputBin, byte_size(ZipBinary) + 20, ChecksumOffset
            ]),
            ok;
        {error, {not_dir, Dir}} ->
            io:format(standard_error, "Error: ~s is not a directory\n", [Dir]),
            erlang:halt(1);
        {error, {no_mp3_found, Dir}} ->
            io:format(standard_error, "Error: no .mp3 sounds were found in ~s\n", [Dir]),
            erlang:halt(1);
        {error, {invalid_mp3, File, Reason}} ->
            io:format(
                standard_error,
                "Error: ~s is not a valid 44.1 kHz or 48 kHz mono MP3: ~s\n",
                [File, Reason]
            ),
            erlang:halt(1);
        {error, {zip_create_error, Reason}} ->
            io:format(standard_error, "Error: zip creation failed (~p)\n", [Reason]),
            erlang:halt(1);
        {error, Other} ->
            io:format(standard_error, "Error: ~p\n", [Other]),
            erlang:halt(1)
    end.

create_sounds_zip(SoundsDir) ->
    maybe
        ok ?=
            case filelib:is_dir(SoundsDir) of
                false -> {error, {not_dir, SoundsDir}};
                true -> ok
            end,
        Files = lists:sort(find_mp3_files(SoundsDir)),
        ok ?=
            case Files of
                [] -> {error, {no_mp3_found, SoundsDir}};
                _ -> ok
            end,
        ok ?= validate_mp3_files(Files),
        ZipEntries = lists:map(
            fun(FullPath) ->
                RelPath = make_relative_path(FullPath, SoundsDir),
                {ok, Content} = file:read_file(FullPath),
                {RelPath, Content}
            end,
            Files
        ),
        case zip:create("sounds.zip", ZipEntries, [{compress, []}, memory]) of
            {ok, {"sounds.zip", Binary}} -> {ok, Binary};
            {error, Reason} -> {error, {zip_create_error, Reason}}
        end
    end.

build_zip_index(ZipBinary) ->
    {ok, Table} = zip:table(ZipBinary),
    lists:filtermap(
        fun
            ({zip_file, Filename, _FileInfo, _Comment, LocalHeaderOffset, UncompressedSize}) ->
                DataOffset = compute_data_offset(ZipBinary, LocalHeaderOffset),
                FilenameBin = list_to_binary(Filename),
                {true, {FilenameBin, {DataOffset, UncompressedSize}}};
            ({zip_comment, _}) ->
                false
        end,
        Table
    ).

compute_data_offset(ZipBinary, LocalHeaderOffset) ->
    <<?LOCAL_FILE_HEADER_SIG:32/little, _VersionNeeded:16/little, _Flags:16/little,
        _CompressionMethod:16/little, _ModTime:16/little, _ModDate:16/little, _CRC32:32/little,
        _CompressedSize:32/little, _UncompressedSize:32/little, FilenameLen:16/little,
        ExtraLen:16/little,
        _/binary>> =
        binary:part(ZipBinary, LocalHeaderOffset, ?LOCAL_FILE_HEADER_SIZE),
    LocalHeaderOffset + ?LOCAL_FILE_HEADER_SIZE + FilenameLen + ExtraLen.

build_sounds_index_hrl_content(Index, SoundsDir) ->
    Header = list_to_binary([
        "%% Generated file - do not edit\n"
        "%% Source: ",
        SoundsDir,
        "\n"
        "\n"
        "-include(\"la_machine_sounds_checksum.hrl\").\n"
        "\n"
        "-define(SOUNDS_INDEX, #{\n"
    ]),
    Entries = format_index_entries(Index),
    Footer = <<"}).\n">>,
    list_to_binary([Header, Entries, Footer]).

build_sounds_checksum_hrl_content(Sha1, Offset) ->
    Bytes = binary_to_list(Sha1),
    HexParts = [io_lib:format("16#~2.16.0B", [B]) || B <- Bytes],
    HexBytes = lists:join(", ", HexParts),
    iolist_to_binary([
        "%% Generated file - do not edit\n"
        "\n"
        "-define(SOUNDS_BIN_CHECKSUM, <<",
        HexBytes,
        ">>).\n"
        "-define(SOUNDS_BIN_CHECKSUM_OFFSET, ",
        integer_to_list(Offset),
        ").\n"
        "-define(SOUNDS_BIN_CHECKSUM_SIZE, 20).\n"
    ]).

write_if_changed(FilePath, Content) ->
    case file:read_file(FilePath) of
        {ok, Content} -> ok;
        _ -> ok = file:write_file(FilePath, Content)
    end.

format_index_entries(Index) ->
    format_index_entries(Index, []).

format_index_entries([], Acc) ->
    lists:reverse(Acc);
format_index_entries([{Filename, {DataOffset, DataSize}}], Acc) ->
    Line = io_lib:format("    <<~p>> => {~B, ~B}\n", [
        binary_to_list(Filename), DataOffset, DataSize
    ]),
    lists:reverse([list_to_binary([Line]) | Acc]);
format_index_entries([{Filename, {DataOffset, DataSize}} | Rest], Acc) ->
    Line = io_lib:format("    <<~p>> => {~B, ~B},\n", [
        binary_to_list(Filename), DataOffset, DataSize
    ]),
    format_index_entries(Rest, [list_to_binary([Line]) | Acc]).

%%=============================================================================
%% MP3 validation
%%=============================================================================

validate_mp3_files([]) ->
    ok;
validate_mp3_files([File | Rest]) ->
    case validate_mp3_file(File) of
        ok -> validate_mp3_files(Rest);
        {error, Reason} -> {error, {invalid_mp3, File, Reason}}
    end.

validate_mp3_file(File) ->
    case file:read_file(File) of
        {ok, Content} -> validate_mp3_content(Content);
        {error, Reason} -> {error, io_lib:format("cannot read file: ~p", [Reason])}
    end.

validate_mp3_content(
    <<"ID3", _MajorVersion:8, _MinorVersion:8, _Flags:8, Size0:8, Size1:8, Size2:8, Size3:8,
        Rest/binary>>
) ->
    TagSize = (Size0 bsl 21) bor (Size1 bsl 14) bor (Size2 bsl 7) bor Size3,
    case Rest of
        <<_:TagSize/binary, FrameData/binary>> -> validate_mp3_frame(FrameData);
        _ -> {error, "truncated ID3v2 tag"}
    end;
validate_mp3_content(Content) ->
    validate_mp3_frame(Content).

validate_mp3_frame(<<16#FF, B1, B2, B3, _Rest/binary>>) when (B1 band 16#E0) =:= 16#E0 ->
    MpegVersion = (B1 bsr 3) band 16#03,
    SampleRateIndex = (B2 bsr 2) band 16#03,
    ChannelMode = (B3 bsr 6) band 16#03,
    case ChannelMode of
        2#11 ->
            case MpegVersion of
                2#11 ->
                    case SampleRateIndex of
                        % 44100 Hz
                        2#00 -> ok;
                        % 48000 Hz
                        2#01 -> ok;
                        2#10 -> {error, "sample rate is 32000 Hz, expected 48kHz or 44.1kHz"};
                        2#11 -> {error, "reserved sample rate"}
                    end;
                2#10 ->
                    {error, "MPEG-2, expected MPEG-1 for 48 kHz"};
                2#00 ->
                    {error, "MPEG-2.5, expected MPEG-1 for 48 kHz"};
                2#01 ->
                    {error, "reserved MPEG version"}
            end;
        _ ->
            {error, "not mono (stereo or dual channel)"}
    end;
validate_mp3_frame(<<_, Rest/binary>>) when byte_size(Rest) >= 4 ->
    validate_mp3_frame(Rest);
validate_mp3_frame(_) ->
    {error, "MP3 frame sync not found"}.

%%=============================================================================
%% File utilities
%%=============================================================================

find_mp3_files(Dir) ->
    find_mp3_files(Dir, []).

find_mp3_files(Dir, Acc) ->
    case file:list_dir(Dir) of
        {ok, Entries} ->
            lists:foldl(
                fun(Entry, AccIn) ->
                    FullPath = filename:join(Dir, Entry),
                    case filelib:is_dir(FullPath) of
                        true ->
                            find_mp3_files(FullPath, AccIn);
                        false ->
                            case filename:extension(Entry) of
                                ".mp3" -> [FullPath | AccIn];
                                _ -> AccIn
                            end
                    end
                end,
                Acc,
                Entries
            );
        {error, _} ->
            Acc
    end.

latest_sound_mtime(SoundsDir) ->
    Mp3Files = find_mp3_files(SoundsDir),
    lists:foldl(
        fun(Path, MaxMtime) ->
            Mtime = filelib:last_modified(Path),
            max(Mtime, MaxMtime)
        end,
        0,
        Mp3Files
    ).

make_relative_path(FullPath, BaseDir) ->
    FullPathNorm = filename:absname(FullPath),
    BaseDirNorm = filename:absname(BaseDir),
    case string:prefix(FullPathNorm, BaseDirNorm) of
        nomatch ->
            filename:basename(FullPath);
        RelPath ->
            case RelPath of
                [$/ | Rest] -> Rest;
                Rest -> Rest
            end
    end.
