%%%-------------------------------------------------------------------
%%% @author Lewis Grozinger <>
%%% @copyright (C) 2017, Lewis Grozinger
%%% @doc
%%%
%%% @end
%%% Created :  4 Oct 2017 by Lewis Grozinger <>
%%%-------------------------------------------------------------------
-module(dslc).

-export([file_to_forms/1]).

-spec file_to_forms(File) -> Forms when
      File  :: string(),
      Forms :: [erl_parse:abstract_form()].

file_to_forms(File) ->
    {ok, FileAsBin} = file:read_file(File),
    FileAsString = binary:bin_to_list(FileAsBin),
    TokensList = dsl_scan:scan(FileAsString),
    Forms = [F || {ok, F} <- lists:map(fun dsl_parse:parse_form/1, TokensList)],
    dsl_transform:parse_transform(Forms).
