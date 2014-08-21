#!/usr/bin/env escript
%% -*- coding: utf-8 -*-
%%! +Ktrue

-mode(compile).

%% --------------------------------------------------------------------------
%% Usage
%% --------------------------------------------------------------------------

usage() ->
    io:format("Usage: ~n").

%% --------------------------------------------------------------------------
%% Compiler
%% --------------------------------------------------------------------------

result_reporter() ->
    receive
        {Id, _Result} ->
            io:format(standard_io, "~p~n", [Id])
    end,
    result_reporter().

compile_worker(ResultPid, Id, File, Include, IncludeLib, OutputDir) ->
    Res = compile(File, Include, IncludeLib, OutputDir),
    ResultPid ! {Id, Res}.

compile(File, Include, IncludeLib, OutputDir) ->
    compile:file(File, [{i, Include}, {i, IncludeLib}, {outdir, OutputDir}]).

%% --------------------------------------------------------------------------
%% Commands
%% --------------------------------------------------------------------------

command_loop(ResultPid) ->
    RawLine = io:get_line(standard_io, ""),
    StrippedLine = string:strip(RawLine, both, $\n),
    Words = string:tokens(StrippedLine, " "),
    case Words of
        [StringId, File, Include, IncludeLib, OutputDir] ->
            {Id, _Rest} = string:to_integer(StringId),
            spawn_link(fun () -> compile_worker(ResultPid, Id, File, Include,
                                                IncludeLib, OutputDir) end),
            command_loop(ResultPid);
        ["quit"] ->
            ok
    end.

%% --------------------------------------------------------------------------
%% Main
%% --------------------------------------------------------------------------

main([]) ->
    StdOutWriterPid = spawn_link(fun result_reporter/0),
    command_loop(StdOutWriterPid);
main(_) ->
    usage().
