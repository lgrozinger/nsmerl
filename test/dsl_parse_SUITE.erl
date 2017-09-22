%%%-------------------------------------------------------------------
%%% @author lewis grozinger <lewis@lewis.lewis>
%%% @copyright (C) 2017, lewis grozinger
%%% @doc
%%%
%%% @end
%%% Created : 24 Aug 2017 by lewis grozinger <lewis@lewis.lewis>
%%%-------------------------------------------------------------------
-module(dsl_parse_SUITE).

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
    [test_split_empty,
     test_split_one_form,
     test_split_two_forms,
     test_split_no_dot,
     test_parse_nin,
     test_parse_nout,
     test_parse_become,
     test_parse_sendto,
     test_parse_rule].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_split_empty(_Config) -> 
    [] = dsl_parse:split([]).

test_split_one_form(_Config) -> 
    [[{dot, _}]] = dsl_parse:split([{dot, 1}]).

test_split_no_dot(_Config) -> 
    {ok, Tokens, _End} = dsl_scan:string("X = Y + 2 - 1 / 6.67,"),
    [Tokens] = dsl_parse:split(Tokens).

test_split_two_forms(_Config) -> 
    {ok, AttributeForm, _End} = dsl_scan:string("-module(testing)."),
    {ok, FunctionForm, _End}  = dsl_scan:string("Y(X)->42+X."),
    {ok, BothForms, _End}     = dsl_scan:string("-module(testing). Y(X)->42+X."),
    [AttributeForm, FunctionForm] = dsl_parse:split(BothForms).

test_parse_rule(_Config) ->
    RuleString = "green * receiving {msg, Content} -> become blue.",
    {ok, Tokens, _End} = dsl_scan:string(RuleString), 
    Expected = {rule, {state, 'green'}, 
		{event, 'receiving'},
		{pattern, {tuple, 1, [{atom, 1, msg}, {var, 1, 'Content'}]}},
		{actions, [{become, {state, 'blue'}}]}},
    {ok, Actually} = dsl_parse:parse(Tokens),
    Expected = Actually.

test_parse_become(_Config) ->
    String = "become green.",
    {ok, Tokens, _End} = dsl_scan:string(String),
    Expected = [{become, {state, 'green'}}],
    {ok, Actually} = dsl_parse:parse_exprs(Tokens),
    Expected = Actually.

test_parse_alarm(_Config) ->
    [].

test_parse_sendto(_Config) ->
    String = "send 42 to [name].",
    {ok, Tokens, End} = dsl_scan:string(String),
    Expected = [{sendto,
		 {msg, erl_parse:abstract(42, 1)},
		 {recipients, erl_parse:abstract([name], End)}}],
    {ok, Actually} = dsl_parse:parse_exprs(Tokens), 
    Expected = Actually.

test_parse_nin(_Config) ->
    String = "nin.",
    {ok, Tokens, _End} = dsl_scan:string(String),
    Expected = [{nin}],
    {ok, Actually} = dsl_parse:parse_exprs(Tokens),
    Expected = Actually.

test_parse_nout(_Config) ->
    String = "nout.",
    {ok, Tokens, _End} = dsl_scan:string(String),
    Expected = [{nout}],
    {ok, Actually} = dsl_parse:parse_exprs(Tokens),
    Expected = Actually.

test_parse_example_source(_Config) ->
    {ok, Tokens, _End} = dsl_scan:file("../../dsl_parse_SUITE_data/example.nsm").
