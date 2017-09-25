%%%-------------------------------------------------------------------
%%% @author lewis grozinger <lewis@lewis.lewis>
%%% @copyright (C) 2017, lewis grozinger
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erl_tree_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("syntax_tools/include/merl.hrl").


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
     test_function,
     test_add_function_clause_empty_function,
     test_add_function_clause,
     test_remote_call,
     test_remote_call_with_args].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_module_loadable(_Config) -> 
    {module, erl_tree} = code:load_file(erl_tree),
    ok.

test_function(_Config) ->
    Expected = {tree, function, {attr, 0, [], none}, {func, function, []}},
    Actually = erl_tree:function(function),
    Expected = Actually.

test_add_function_clause_empty_function(_Config) ->
    %%% add a clause to a function with none.
    NoClauseTree = erl_tree:function(function),
    [] = erl_syntax:function_clauses(NoClauseTree),
    NewClause = erl_syntax:clause([], [], [erl_syntax:atom(ok)]),
    ClauseTree = erl_tree:add_clause(NewClause, NoClauseTree),
    [NewClause] = erl_syntax:function_clauses(ClauseTree).

test_add_function_clause(_Config) ->
    Clause1 = erl_syntax:clause([], [], [erl_syntax:atom(clause1)]),
    Clause2 = erl_syntax:clause([], [], [erl_syntax:atom(clause2)]),
    Function1 = erl_tree:add_clause(Clause1, erl_tree:function(function)),
    Function2 = erl_tree:add_clause(Clause2, Function1),
    [Clause2, Clause1] = erl_syntax:function_clauses(Function2).

test_remote_call(_Config) ->
    Expected = merl:quote(0, "module:function()"),
    Actually = erl_syntax:revert(erl_tree:remote_call('module', 'function', [])),
    Expected = Actually.

test_remote_call_with_args(_Config) ->
    Arg1 = {arg1, 42},
    Arg2 = arg2,
    Arg3 = 3,

    RemoteTree = erl_tree:remote_call('module', 'function', [Arg1, Arg2, Arg3]),
    MerlTree   = merl:quote(0, "module:function(_@Arg1@, _@Arg2@, _@Arg3@)"),

    Actually = erl_syntax:revert(RemoteTree),
    Expected = erl_syntax:revert(MerlTree),
    Expected = Actually.


    
