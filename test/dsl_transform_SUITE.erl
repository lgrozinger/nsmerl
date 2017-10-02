%%%-------------------------------------------------------------------
%%% @author lewis grozinger <lewis@lewis.lewis>
%%% @copyright (C) 2017, lewis grozinger
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dsl_transform_SUITE).

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
     test_become_transform,
     test_nin_transform,
     test_nout_transform,
     test_send_to_transform].

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
    {module, dsl_transform} = code:load_file(dsl_transform),
    ok.


test_become_transform(__Config) ->
    %% get the tree
    {ok, DslTokens, _} = dsl_scan:string("become green."), 
    {ok, DslTree}      = dsl_parse:parse_exprs(DslTokens),
    %% set the transform state
    Used = sets:add_element('V0', sets:new()),
    S    = {Used, 'V0'},
    %% do the transform
    {_NewS, StandardTree} = dsl_transform:become(DslTree, S),
    Actually = erl_syntax:revert(StandardTree),

    {match,_,{var,_,_},
     {call,_,
      {remote,_,{atom,_,'nsmops'},{atom,_,'become'}},
      [{atom,_,'green'},{var,_,'V0'}]}} = Actually.

test_nin_transform(__Config) ->
    %% set the transform state
    Used = sets:add_element('V0', sets:new()),
    S    = {Used, 'V0'},
    StandardTree = dsl_transform:nin(S),
    Actually = erl_syntax:revert(StandardTree),

    {call,_,
     {remote,_,{atom,_,'nsmops'},{atom,_,'nin'}},
     [{var,_,'V0'}]} = Actually.
    
test_nout_transform(__Config) ->
    %% set the transform state
    Used = sets:add_element('V0', sets:new()),
    S    = {Used, 'V0'},
    StandardTree = dsl_transform:nout(S),
    Actually = erl_syntax:revert(StandardTree),

    {call,_,
     {remote,_,{atom,_,'nsmops'},{atom,_,'nout'}},
     [{var,_,'V0'}]} = Actually.

test_send_to_transform(__Config) ->
    {ok, DslTokens, _} = dsl_scan:string("send MsgData to [name]."),
    {ok, DslTree} = dsl_parse:parse_exprs(DslTokens),
    Used = sets:add_element('V0', sets:new()),
    S = {Used, 'V0'},
    ErlTree = dsl_transform:sendto(DslTree, S),
    Actually = erl_syntax:revert(ErlTree),
    {call, _,
     {remote, _, {atom, _, 'nsmops'}, {atom, _, 'sendto'}},
     [{var, _, 'MsgData'},
      {cons, _, {atom, _, 'name'}, {nil, _}},
      {var, _, 'V0'}]} = Actually.
    
