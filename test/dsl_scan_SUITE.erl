%%%-------------------------------------------------------------------
%%% @author lewis grozinger <lewis@lewis.lewis>
%%% @copyright (C) 2017, lewis grozinger
%%% @doc
%%% Tests for the extra scanning utilities for the behaviour and topology DSL.
%%% @end
%%% Created : 20 Aug 2017 by lewis grozinger <lewis@lewis.lewis>
%%%-------------------------------------------------------------------
-module(dsl_scan_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [test_module_loadable,
     test_reserved_words,
     test_standard_erl_scan,
     test_dsl_erl_scan].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_module_loadable(__Config) ->
    {module, dsl_scan} = code:load_file(dsl_scan),
    ok.

test_reserved_words(__Config) -> 
    Words = reserved_list(),
    case lists:all(fun dsl_scan:reserved_word/1, Words) of
	true  -> ok;
	false -> ct:fail({error, "reserved word not recognised"})
    end, 
    case dsl_scan:reserved_word('notreserved') of
	true  -> ct:fail({error, "atom incorrectly categorised"});
	false -> ok
    end.

test_standard_erl_scan(__Config) ->
    {ok, FileAsBin} = file:read_file("../../dsl_scan_SUITE_data/standard.erl"),
    FileAsString = binary:bin_to_list(FileAsBin),
    {ok, Expected, _End} = erl_scan:string(FileAsString),
    {ok, Expected, _} = dsl_scan:string(FileAsString).

test_dsl_erl_scan(__Config) ->
    {ok, FileAsBin} = file:read_file("../../dsl_scan_SUITE_data/extended.erl"),
    FileAsString = binary:bin_to_list(FileAsBin),
    NewWords = {reserved_word_fun, fun dsl_scan:reserved_word/1},
    {ok, Expected, _End} = erl_scan:string(FileAsString, 1, NewWords),
    {ok, Expected, _} = dsl_scan:string(FileAsString).

reserved_list() ->
    ['after',
     'begin',
     'case',
     'try',
     'cond',
     'catch',
     'andalso',
     'orelse',
     'end',
     'fun',
     'if',
     'let',
     'of',
     'receive',
     'when',
     'bnot',
     'not',
     'div',
     'rem',
     'band',
     'and',
     'bor',
     'bxor',
     'bsl',
     'bsr',
     'or',
     'xor',
     'become',
     'receiving',
     'send',
     'to',
     'alarm',
     'nout',
     'nin',
     'spontaneous'].

