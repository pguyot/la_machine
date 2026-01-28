#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

%%-----------------------------------------------------------------------------
%% @doc Create an uncompressed ZIP archive from the sounds directory.
%%
%% Uses environment variables (from rebar):
%% - `REBAR_ROOT_DIR': Root directory containing sounds/
%% - `REBAR_BUILD_DIR': Build directory where sounds.zip will be created
%%-----------------------------------------------------------------------------
main(_Args) ->
    RootDir = os:getenv("REBAR_ROOT_DIR"),
    BuildDir = os:getenv("REBAR_BUILD_DIR"),

    case {RootDir, BuildDir} of
        {false, _} ->
            io:format(standard_error, "Error: REBAR_ROOT_DIR not set~n", []),
            erlang:halt(1);
        {_, false} ->
            io:format(standard_error, "Error: REBAR_BUILD_DIR not set~n", []),
            erlang:halt(1);
        {_, _} ->
            SoundsDir = filename:join(RootDir, "sounds"),
            OutputBin = filename:join(BuildDir, "sounds.bin"),
            case create_sounds_bin(SoundsDir, OutputBin) of
                ok ->
                    ok;
                {error, {not_dir, Dir}} ->
                    io:format(standard_error, "Error: ~s is not a directory\n", [Dir]),
                    erlang:halt(1);
                {error, {no_aac_found, Dir}} ->
                    io:format(standard_error, "Error: no .aac sound were found in ~s\n", [Dir]),
                    erlang:halt(1);
                {error, {zip_create_error, Reason}} ->
                    io:format(standard_error, "Error: zip creation failed (~p)\n", [Reason]),
                    erlang:halt(1);
                {error, Other} ->
                    io:format(standard_error, "Error: ~p\n", [Other]),
                    erlang:halt(1)
            end
    end.

create_sounds_bin(SoundsDir, OutputBin) ->
    maybe
        {ok, Binary} ?= create_sounds_zip(SoundsDir),
        Size = byte_size(Binary),
        ok ?= file:write_file(OutputBin, <<Size:32, Binary/binary>>)
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
        Files = find_aac_files(SoundsDir),
        ok ?=
            case Files of
                [] ->
                    {error, {no_aac_found, SoundsDir}};
                _ ->
                    ok
            end,
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

%% @doc Find all .aac files recursively in the given directory.
find_aac_files(Dir) ->
    find_aac_files(Dir, []).

find_aac_files(Dir, Acc) ->
    case file:list_dir(Dir) of
        {ok, Entries} ->
            lists:foldl(
                fun(Entry, AccIn) ->
                    FullPath = filename:join(Dir, Entry),
                    case filelib:is_dir(FullPath) of
                        true ->
                            find_aac_files(FullPath, AccIn);
                        false ->
                            case filename:extension(Entry) of
                                ".aac" -> [FullPath | AccIn];
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
%% Returns a string like "poke/00202.aac"
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
