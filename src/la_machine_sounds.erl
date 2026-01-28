%
% This file is part of La Machine
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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
%% @doc La Machine sounds interface for reading sounds from a ZIP archive
%% stored in a partition.
%%
%% The ZIP archive must be uncompressed (store method only).
%% @end
%%-----------------------------------------------------------------------------
-module(la_machine_sounds).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    get_sound/1
]).

-export([
    parse_zip_directory/3,
    mmap_file_from_zip/4
]).

-ifdef(TEST).
-export([
    find_eocd/1,
    parse_central_directory/2,
    parse_central_file_header/1,
    get_file_data_offset/1
]).
-endif.

-define(SOUNDS_PARTITION, <<"sounds">>).

%% ZIP signatures (little-endian)
-define(LOCAL_FILE_HEADER_SIG, 16#04034b50).
-define(CENTRAL_DIR_HEADER_SIG, 16#02014b50).
-define(EOCD_SIG, 16#06054b50).

%% ZIP header sizes
-define(LOCAL_FILE_HEADER_SIZE, 30).
-define(CENTRAL_DIR_HEADER_SIZE, 46).
-define(EOCD_MIN_SIZE, 22).

%%-----------------------------------------------------------------------------
%% @doc Get a sound file as an mmap binary from the sounds partition.
%%
%% The sound file is read from the "sounds" partition. First 32 bits is the
%% size of the zip archive, followed by the zip archive which has a directory
%% at the end.
%% @end
%%-----------------------------------------------------------------------------
-spec get_sound(Filename :: binary()) -> {ok, binary()} | {error, term()}.
get_sound(Filename) ->
    maybe
        {ok, <<Size:32>>} ?= esp:partition_read(?SOUNDS_PARTITION, 0, 4),
        {ok, Directory} ?= parse_zip_directory(?SOUNDS_PARTITION, 4, Size),
        case maps:get(Filename, Directory, undefined) of
            undefined ->
                {error, {file_not_found, Filename}};
            {Offset, Size} ->
                mmap_file_from_zip(?SOUNDS_PARTITION, Offset, Size, Filename)
        end
    end.

%%-----------------------------------------------------------------------------
%% @doc Parse the ZIP directory from a partition.
%%
%% Returns a map of filename => {local_header_offset, uncompressed_size}.
%% @end
%%-----------------------------------------------------------------------------
-spec parse_zip_directory(
    Partition :: binary(), Offset :: non_neg_integer(), Length :: non_neg_integer()
) ->
    {ok, #{binary() => {non_neg_integer(), non_neg_integer()}}} | {error, term()}.
parse_zip_directory(Partition, Offset, Length) ->
    %% Read the last part of the archive to find the EOCD
    %% EOCD is at most 65535 + 22 bytes from the end (max comment size + EOCD size)
    EocdSearchSize = min(Length, 65557),
    EocdSearchOffset = Offset + Length - EocdSearchSize,
    case esp:partition_read(Partition, EocdSearchOffset, EocdSearchSize) of
        {ok, EocdSearchData} ->
            case find_eocd(EocdSearchData) of
                {ok, {CentralDirOffset, CentralDirSize, _EntryCount}} ->
                    %% Read the central directory
                    AbsoluteCentralDirOffset = Offset + CentralDirOffset,
                    case esp:partition_read(Partition, AbsoluteCentralDirOffset, CentralDirSize) of
                        {ok, CentralDirData} ->
                            {ok, parse_central_directory(CentralDirData, #{})};
                        error ->
                            {error, partition_read_failed}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @doc Memory-map a file from the ZIP archive in a partition.
%%
%% LocalHeaderOffset is the offset of the local file header within the archive.
%% @end
%%-----------------------------------------------------------------------------
-spec mmap_file_from_zip(
    Partition :: binary(),
    LocalHeaderOffset :: non_neg_integer(),
    UncompressedSize :: non_neg_integer(),
    Filename :: binary()
) -> {ok, binary()} | {error, term()}.
mmap_file_from_zip(Partition, LocalHeaderOffset, UncompressedSize, Filename) ->
    %% Read the local file header to get the actual data offset
    case esp:partition_read(Partition, LocalHeaderOffset, ?LOCAL_FILE_HEADER_SIZE) of
        {ok, LocalHeader} ->
            case get_file_data_offset(LocalHeader) of
                {ok, DataOffset} ->
                    FileDataOffset = LocalHeaderOffset + DataOffset,
                    case esp:partition_map(Partition, FileDataOffset, UncompressedSize) of
                        {ok, MappedBinary} ->
                            {ok, MappedBinary};
                        error ->
                            {error, partition_mmap_failed}
                    end;
                {error, {compression_not_supported, _}} = Error ->
                    Error;
                {error, invalid_local_header} ->
                    {error, {invalid_local_header, Filename}}
            end;
        error ->
            {error, partition_read_failed}
    end.

%% @private
%% @doc Find the End of Central Directory record in the given binary.
%% Returns {CentralDirOffset, CentralDirSize, EntryCount} or error.
-spec find_eocd(binary()) ->
    {ok, {non_neg_integer(), non_neg_integer(), non_neg_integer()}} | {error, eocd_not_found}.
find_eocd(Data) ->
    find_eocd(Data, byte_size(Data) - ?EOCD_MIN_SIZE).

find_eocd(_Data, Offset) when Offset < 0 ->
    {error, eocd_not_found};
find_eocd(Data, Offset) ->
    case Data of
        <<_:Offset/binary, ?EOCD_SIG:32/little, _DiskNum:16/little, _DiskWithCD:16/little,
            _EntriesOnDisk:16/little, TotalEntries:16/little, CentralDirSize:32/little,
            CentralDirOffset:32/little, _CommentLen:16/little, _/binary>> ->
            {ok, {CentralDirOffset, CentralDirSize, TotalEntries}};
        _ ->
            find_eocd(Data, Offset - 1)
    end.

%% @private
%% @doc Parse the central directory and build a map of filename => {offset, size}.
-spec parse_central_directory(binary(), #{binary() => {non_neg_integer(), non_neg_integer()}}) ->
    #{binary() => {non_neg_integer(), non_neg_integer()}}.
parse_central_directory(<<>>, Acc) ->
    Acc;
parse_central_directory(Data, Acc) ->
    case parse_central_file_header(Data) of
        {ok, {Filename, LocalHeaderOffset, UncompressedSize, Rest}} ->
            NewAcc = maps:put(Filename, {LocalHeaderOffset, UncompressedSize}, Acc),
            parse_central_directory(Rest, NewAcc);
        {error, _} ->
            %% Stop parsing on error, return what we have
            Acc
    end.

%% @private
%% @doc Parse a single central directory file header.
-spec parse_central_file_header(binary()) ->
    {ok, {binary(), non_neg_integer(), non_neg_integer(), binary()}} | {error, term()}.
parse_central_file_header(
    <<?CENTRAL_DIR_HEADER_SIG:32/little, _VersionMadeBy:16/little, _VersionNeeded:16/little,
        _Flags:16/little, CompressionMethod:16/little, _ModTime:16/little, _ModDate:16/little,
        _CRC32:32/little, _CompressedSize:32/little, UncompressedSize:32/little,
        FilenameLen:16/little, ExtraLen:16/little, CommentLen:16/little, _DiskStart:16/little,
        _InternalAttr:16/little, _ExternalAttr:32/little, LocalHeaderOffset:32/little, Rest/binary>>
) when CompressionMethod =:= 0 ->
    %% Only support stored (no compression)
    case Rest of
        <<Filename:FilenameLen/binary, _Extra:ExtraLen/binary, _Comment:CommentLen/binary,
            Remaining/binary>> ->
            {ok, {Filename, LocalHeaderOffset, UncompressedSize, Remaining}};
        _ ->
            {error, truncated_header}
    end;
parse_central_file_header(
    <<?CENTRAL_DIR_HEADER_SIG:32/little, _:16/little, _:16/little, _:16/little,
        CompressionMethod:16/little, _/binary>>
) when CompressionMethod =/= 0 ->
    {error, {compression_not_supported, CompressionMethod}};
parse_central_file_header(_) ->
    {error, invalid_header}.

%% @private
%% @doc Get the offset from the start of the local file header to the file data.
-spec get_file_data_offset(binary()) -> {ok, non_neg_integer()} | {error, term()}.
get_file_data_offset(
    <<?LOCAL_FILE_HEADER_SIG:32/little, _VersionNeeded:16/little, _Flags:16/little,
        CompressionMethod:16/little, _ModTime:16/little, _ModDate:16/little, _CRC32:32/little,
        _CompressedSize:32/little, _UncompressedSize:32/little, FilenameLen:16/little,
        ExtraLen:16/little, _/binary>>
) when CompressionMethod =:= 0 ->
    {ok, ?LOCAL_FILE_HEADER_SIZE + FilenameLen + ExtraLen};
get_file_data_offset(
    <<?LOCAL_FILE_HEADER_SIG:32/little, _:16/little, _:16/little, CompressionMethod:16/little,
        _/binary>>
) when CompressionMethod =/= 0 ->
    {error, {compression_not_supported, CompressionMethod}};
get_file_data_offset(_) ->
    {error, invalid_local_header}.

%%-----------------------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------------------
-ifdef(TEST).

%% Helper to create a minimal valid local file header (stored/no compression)
make_local_file_header(Filename, Data) ->
    FilenameLen = byte_size(Filename),
    DataSize = byte_size(Data),
    <<?LOCAL_FILE_HEADER_SIG:32/little, 20:16/little, 0:16/little, 0:16/little, 0:16/little,
        0:16/little, 0:32/little, DataSize:32/little, DataSize:32/little, FilenameLen:16/little,
        0:16/little, Filename/binary, Data/binary>>.

%% Helper to create a central directory file header
make_central_file_header(Filename, LocalHeaderOffset, DataSize) ->
    FilenameLen = byte_size(Filename),
    <<?CENTRAL_DIR_HEADER_SIG:32/little, 20:16/little, 20:16/little, 0:16/little, 0:16/little,
        0:16/little, 0:16/little, 0:32/little, DataSize:32/little, DataSize:32/little,
        FilenameLen:16/little, 0:16/little, 0:16/little, 0:16/little, 0:16/little, 0:32/little,
        LocalHeaderOffset:32/little, Filename/binary>>.

%% Helper to create an EOCD record
make_eocd(CentralDirOffset, CentralDirSize, EntryCount) ->
    <<?EOCD_SIG:32/little, 0:16/little, 0:16/little, EntryCount:16/little, EntryCount:16/little,
        CentralDirSize:32/little, CentralDirOffset:32/little, 0:16/little>>.

find_eocd_test_() ->
    [
        {"EOCD at end of data",
            ?_assertEqual(
                {ok, {100, 200, 5}},
                find_eocd(make_eocd(100, 200, 5))
            )},
        {"EOCD with padding before",
            ?_assertEqual(
                {ok, {100, 200, 5}},
                find_eocd(<<0, 0, 0, 0, (make_eocd(100, 200, 5))/binary>>)
            )},
        {"EOCD not found",
            ?_assertEqual(
                {error, eocd_not_found},
                find_eocd(<<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>)
            )},
        {"Empty data",
            ?_assertEqual(
                {error, eocd_not_found},
                find_eocd(<<>>)
            )}
    ].

parse_central_file_header_test_() ->
    [
        {"Valid header",
            ?_assertEqual(
                {ok, {<<"test.txt">>, 0, 100, <<>>}},
                parse_central_file_header(make_central_file_header(<<"test.txt">>, 0, 100))
            )},
        {"Header with remaining data",
            ?_assertEqual(
                {ok, {<<"foo.aac">>, 42, 256, <<"remaining">>}},
                parse_central_file_header(
                    <<(make_central_file_header(<<"foo.aac">>, 42, 256))/binary, "remaining">>
                )
            )},
        {"Invalid signature",
            ?_assertEqual(
                {error, invalid_header},
                parse_central_file_header(<<0, 0, 0, 0>>)
            )}
    ].

parse_central_directory_test_() ->
    Header1 = make_central_file_header(<<"file1.txt">>, 0, 100),
    Header2 = make_central_file_header(<<"file2.txt">>, 150, 200),
    [
        {"Empty directory",
            ?_assertEqual(
                #{},
                parse_central_directory(<<>>, #{})
            )},
        {"Single file",
            ?_assertEqual(
                #{<<"file1.txt">> => {0, 100}},
                parse_central_directory(Header1, #{})
            )},
        {"Multiple files",
            ?_assertEqual(
                #{<<"file1.txt">> => {0, 100}, <<"file2.txt">> => {150, 200}},
                parse_central_directory(<<Header1/binary, Header2/binary>>, #{})
            )}
    ].

get_file_data_offset_test_() ->
    [
        {"Simple header no extra",
            ?_assertEqual(
                {ok, 30 + 8},
                get_file_data_offset(
                    <<?LOCAL_FILE_HEADER_SIG:32/little, 20:16/little, 0:16/little, 0:16/little,
                        0:16/little, 0:16/little, 0:32/little, 100:32/little, 100:32/little,
                        8:16/little, 0:16/little>>
                )
            )},
        {"Header with extra field",
            ?_assertEqual(
                {ok, 30 + 8 + 10},
                get_file_data_offset(
                    <<?LOCAL_FILE_HEADER_SIG:32/little, 20:16/little, 0:16/little, 0:16/little,
                        0:16/little, 0:16/little, 0:32/little, 100:32/little, 100:32/little,
                        8:16/little, 10:16/little>>
                )
            )},
        {"Compressed file rejected",
            ?_assertEqual(
                {error, {compression_not_supported, 8}},
                get_file_data_offset(
                    <<?LOCAL_FILE_HEADER_SIG:32/little, 20:16/little, 0:16/little, 8:16/little,
                        0:16/little, 0:16/little, 0:32/little, 100:32/little, 100:32/little,
                        8:16/little, 0:16/little>>
                )
            )},
        {"Invalid header",
            ?_assertEqual(
                {error, invalid_local_header},
                get_file_data_offset(<<0, 0, 0, 0>>)
            )}
    ].

%% Integration test with a complete mini ZIP archive
complete_zip_test_() ->
    %% Build a minimal ZIP archive with one file
    Filename = <<"test.aac">>,
    FileData = <<"fake audio data">>,
    LocalHeader = make_local_file_header(Filename, FileData),
    LocalHeaderOffset = 0,
    LocalHeaderSize = byte_size(LocalHeader),
    CentralHeader = make_central_file_header(Filename, LocalHeaderOffset, byte_size(FileData)),
    CentralDirOffset = LocalHeaderSize,
    CentralDirSize = byte_size(CentralHeader),
    Eocd = make_eocd(CentralDirOffset, CentralDirSize, 1),
    ZipArchive = <<LocalHeader/binary, CentralHeader/binary, Eocd/binary>>,

    [
        {"Find EOCD in complete archive",
            ?_assertEqual(
                {ok, {CentralDirOffset, CentralDirSize, 1}},
                find_eocd(ZipArchive)
            )},
        {"Parse central directory from complete archive",
            ?_assertEqual(
                #{Filename => {LocalHeaderOffset, byte_size(FileData)}},
                parse_central_directory(CentralHeader, #{})
            )},
        {"Get file data offset from local header",
            ?_assertEqual(
                {ok, ?LOCAL_FILE_HEADER_SIZE + byte_size(Filename)},
                get_file_data_offset(binary:part(LocalHeader, 0, ?LOCAL_FILE_HEADER_SIZE))
            )}
    ].

-endif.
