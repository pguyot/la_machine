#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

%%-----------------------------------------------------------------------------
%% @doc Create an uncompressed ZIP archive from the sounds directory and
%% generate a pre-compiled index header file.
%%
%% Uses environment variable (from rebar):
%% - `REBAR_ROOT_DIR': Root directory containing sounds/
%%-----------------------------------------------------------------------------
main(_Args) ->
    case os:getenv("REBAR_ROOT_DIR") of
        false ->
            io:format(standard_error, "Error: REBAR_ROOT_DIR not set~n", []),
            erlang:halt(1);
        RootDir ->
            BuildDir = filename:join([RootDir, "_build", "generated"]),
            SoundsDir = filename:join(RootDir, "sounds"),
            case create_sounds_partition(SoundsDir, BuildDir) of
                ok ->
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
                        [
                            File, Reason
                        ]
                    ),
                    erlang:halt(1);
                {error, {zip_create_error, Reason}} ->
                    io:format(standard_error, "Error: zip creation failed (~p)\n", [Reason]),
                    erlang:halt(1);
                {error, Other} ->
                    io:format(standard_error, "Error: ~p\n", [Other]),
                    erlang:halt(1)
            end
    end.

create_sounds_partition(SoundsDir, BuildDir) ->
    maybe
        {ok, ZipBinary} ?= create_sounds_zip(SoundsDir),
        Index = build_index(ZipBinary),
        OutputBin = filename:join(BuildDir, "sounds.bin"),
        OutputHrl = filename:join(BuildDir, "la_machine_sounds_index.hrl"),
        ok ?= file:write_file(OutputBin, ZipBinary),
        ok ?= write_index_hrl(OutputHrl, Index)
    end.

create_sounds_zip(SoundsDir) ->
    maybe
        ok ?=
            case filelib:is_dir(SoundsDir) of
                false ->
                    {error, {not_dir, SoundsDir}};
                true ->
                    ok
            end,
        Files = find_mp3_files(SoundsDir),
        ok ?=
            case Files of
                [] ->
                    {error, {no_mp3_found, SoundsDir}};
                _ ->
                    ok
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
        %% No compression (store only)
        case zip:create("sounds.zip", ZipEntries, [{compress, []}, memory]) of
            {ok, {"sounds.zip", Binary}} ->
                {ok, Binary};
            {error, Reason} ->
                {error, {zip_create_error, Reason}}
        end
    end.

%%-----------------------------------------------------------------------------
%% Index building — use zip:table/1 to list entries, then read local headers
%% to compute the actual data offsets.
%%-----------------------------------------------------------------------------

-define(LOCAL_FILE_HEADER_SIG, 16#04034b50).
-define(LOCAL_FILE_HEADER_SIZE, 30).

%% @doc Build an index of {Filename, {DataOffset, DataSize}} from a ZIP binary.
build_index(ZipBinary) ->
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

%% @doc Compute the offset of the actual file data from a local file header.
compute_data_offset(ZipBinary, LocalHeaderOffset) ->
    <<?LOCAL_FILE_HEADER_SIG:32/little, _VersionNeeded:16/little, _Flags:16/little,
        _CompressionMethod:16/little, _ModTime:16/little, _ModDate:16/little, _CRC32:32/little,
        _CompressedSize:32/little, _UncompressedSize:32/little, FilenameLen:16/little,
        ExtraLen:16/little,
        _/binary>> =
        binary:part(ZipBinary, LocalHeaderOffset, ?LOCAL_FILE_HEADER_SIZE),
    LocalHeaderOffset + ?LOCAL_FILE_HEADER_SIZE + FilenameLen + ExtraLen.

%%-----------------------------------------------------------------------------
%% .hrl generation
%%-----------------------------------------------------------------------------

write_index_hrl(OutputHrl, Index) ->
    Header =
        <<
            "%% Generated by create_sounds_partition.escript — do not edit.\n"
            "-define(SOUNDS_INDEX, #{\n"
        >>,
    Entries = format_index_entries(Index),
    Footer = <<"}).\n">>,
    Content = list_to_binary([Header, Entries, Footer]),
    case file:read_file(OutputHrl) of
        {ok, Content} ->
            ok;
        _ ->
            file:write_file(OutputHrl, Content)
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

%%-----------------------------------------------------------------------------
%% MP3 validation
%%-----------------------------------------------------------------------------

%% @doc Validate all MP3 files are 44.1kHz or 48 kHz mono.
validate_mp3_files([]) ->
    ok;
validate_mp3_files([File | Rest]) ->
    case validate_mp3_file(File) of
        ok ->
            validate_mp3_files(Rest);
        {error, Reason} ->
            {error, {invalid_mp3, File, Reason}}
    end.

%% @doc Validate a single MP3 file is 48 kHz mono.
validate_mp3_file(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            validate_mp3_content(Content);
        {error, Reason} ->
            {error, io_lib:format("cannot read file: ~p", [Reason])}
    end.

%% @doc Validate MP3 content, skipping ID3v2 tag if present.
validate_mp3_content(
    <<"ID3", _MajorVersion:8, _MinorVersion:8, _Flags:8, Size0:8, Size1:8, Size2:8, Size3:8,
        Rest/binary>>
) ->
    %% ID3v2 tag size is encoded as syncsafe integer (7 bits per byte)
    TagSize = (Size0 bsl 21) bor (Size1 bsl 14) bor (Size2 bsl 7) bor Size3,
    case Rest of
        <<_:TagSize/binary, FrameData/binary>> ->
            validate_mp3_frame(FrameData);
        _ ->
            {error, "truncated ID3v2 tag"}
    end;
validate_mp3_content(Content) ->
    validate_mp3_frame(Content).

%% @doc Find and validate the MP3 frame header.
%% Frame sync: 11 bits of 1s (0xFF followed by 0xE0 or higher in high 3 bits)
%% Header layout (4 bytes):
%% Byte 0: 0xFF (sync)
%% Byte 1: 111VVLLP where VV=version, LL=layer, P=protection
%% Byte 2: BBBBSSPR where BBBB=bitrate, SS=sample rate, P=padding, R=private
%% Byte 3: CCXXXXXX where CC=channel mode
validate_mp3_frame(<<16#FF, B1, B2, B3, _Rest/binary>>) when (B1 band 16#E0) =:= 16#E0 ->
    %% B1: 111VVLLP - VV=version(2 bits), LL=layer(2 bits), P=protection(1 bit)
    MpegVersion = (B1 bsr 3) band 16#03,
    %% B2: BBBBSSPR - BBBB=bitrate(4), SS=samplerate(2), P=padding(1), R=private(1)
    SampleRateIndex = (B2 bsr 2) band 16#03,
    %% B3: CCXXXXXX - CC=channel mode(2 bits)
    ChannelMode = (B3 bsr 6) band 16#03,

    %% Channel mode 11 = mono
    case ChannelMode of
        2#11 ->
            %% Check MPEG version is MPEG-1 (binary 11 = 3)
            case MpegVersion of
                2#11 ->
                    %% For MPEG-1:
                    case SampleRateIndex of
                        % sample rate index 00 = 44100 Hz
                        2#00 ->
                            ok;
                        % sample rate index 01 = 48000 Hz
                        2#01 ->
                            ok;
                        2#10 ->
                            {error, "sample rate is 32000 Hz, expected 48kHz or 44.1kHz"};
                        2#11 ->
                            {error, "reserved sample rate"}
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

%%-----------------------------------------------------------------------------
%% File utilities
%%-----------------------------------------------------------------------------

%% @doc Find all .mp3 files recursively in the given directory.
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

%% @doc Make a path relative to the base directory.
%% Returns a string like "poke/00202.mp3"
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
