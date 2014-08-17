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
        {Id, Result} ->
            io:format("~d~n", [Id])
    end,
    result_reporter().

compile_worker(ResultPid, Id, File, Include, IncludeLib, OutputDir) ->
    Res = compile(File, Include, IncludeLib, OutputDir),
    ResultPid ! {Id, Res}.

compile_loop(ResultPid) ->
    RawLine = io:get_line(standard_io, ""),
    StrippedLine = string:strip(RawLine, both, $\n),
    [StringId, File, Include, IncludeLib, OutputDir] = string:tokens(StrippedLine, " "),
    {Id, _Rest} = string:to_integer(StringId),
    spawn_link(fun () -> compile_worker(ResultPid, Id, File, Include,
                                        IncludeLib, OutputDir) end),
    compile_loop(ResultPid).

compile(File, Include, IncludeLib, OutputDir) ->
    compile:file(File, [{i, Include}, {i, IncludeLib}, {outdir, OutputDir}]).

%% --------------------------------------------------------------------------
%% Main
%% --------------------------------------------------------------------------

main([]) ->
    StdOutWriterPid = spawn_link(fun result_reporter/0),
    compile_loop(StdOutWriterPid);
main(_) ->
    usage().
