%%%-------------------------------------------------------------------
%%% @author lewis grozinger <lewis@lewis.lewis>
%%% @copyright (C) 2017, lewis grozinger
%%% @doc
%%%
%%% @end
%%% Created : 16 Aug 2017 by lewis grozinger <lewis@lewis.lewis>
%%%-------------------------------------------------------------------
-module(nsmops_SUITE).

-compile(export_all).

-include("santoro.hrl").

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
    %% an example state record suitable for many tests
    [{state0, example_state()} | Config].

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
     test_become,
     test_sendto_unknown,
     test_sendto,
     test_memory_reference,
     test_memory_reference_failure,
     test_memory_assign,
     test_memory_reassign,
     test_nin,
     test_nout,
     test_add_to_nin,
     test_add_to_nout,
     test_sub_from_nin,
     test_sub_from_nout].

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
    {module, nsmops} = code:load_file(nsmops),
    ok.

test_become(Config) -> 
    %% get example state record from Config
    {state0, State} = lists:keyfind(state0, 1, Config),
    %% check the returned state has status 'red', as expected
    #state{status = red} = nsmops:become(red, State).

test_sendto_unknown(Config) ->
    %% get example state record from Config
    {state0, State} = lists:keyfind(state0, 1, Config),
    {error, __Reason} = nsmops:sendto({message}, ['notmaster'], State). 

test_sendto(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    TestState = nsmops:add_to_nout([{'thisone', self()}], State),
    ok = nsmops:sendto({message}, ['thisone'], TestState),
    receive 
	{message} -> ok
    end.

test_memory_reference(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    42 = nsmops:mem_get("a", State).

test_memory_reference_failure(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    {error, __Reason} = nsmops:mem_get("b", State).

test_memory_assign(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    Value = {test_value, 911},
    #state{memory = #{"b" := Value}} = nsmops:mem_put("b", Value, State).

test_memory_reassign(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    Value = {test_value, 911},
    #state{memory = #{"a" := Value}} = nsmops:mem_put("a", Value, State).

test_add_to_nin(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    ExtraIn = [{new1, self()}, {new2, self()}, {new3, self()}],
    NewState = nsmops:add_to_nin(ExtraIn, State),
    Expected = nsmops:nin(State) ++ [Name || {Name, __PID} <- ExtraIn],
    Actually = nsmops:nin(NewState), 
    case list_misc:same_set(Expected, Actually) of
	true  -> ok;
	false -> ct:fail({{expected, Expected}, {actual, Actually}})
    end.

test_add_to_nout(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    ExtraOut = [{new1, self()}, {new2, self()}, {new3, self()}],
    NewState = nsmops:add_to_nout(ExtraOut, State),
    Expected = nsmops:nout(State) ++ [Name || {Name, __PID} <- ExtraOut],
    Actually = nsmops:nout(NewState), 
    case list_misc:same_set(Expected, Actually) of
	true  -> ok;
	false -> ct:fail({{expected, Expected}, {actual, Actually}})
    end.

test_sub_from_nin(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    #state{inlist = []} = nsmops:sub_from_nin([master], State).

test_sub_from_nout(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    #state{outlist = []} = nsmops:sub_from_nout([master], State).

test_nin(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    [master] = nsmops:nin(State).

test_nout(Config) ->
    {state0, State} = lists:keyfind(state0, 1, Config),
    [master] = nsmops:nout(State).

example_state() ->
    #state{name = test,
	   status = green, 
	   outlist = [{master, self()}], 
	   inlist = [{master, self()}],
	   memory = #{"a" => 42}}.


    
