%%%-------------------------------------------------------------------
%%% @author Lewis Grozinger <lewis.grozinger@student.manchester.ac.uk>
%%% @doc
%%% Contains tests for the static analysis of the DSL used to define 
%%% entity behaviour.
%%% @end
%%%-------------------------------------------------------------------
-module(dsl_SUITE).

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
    [test_eval_become,
     test_eval_multiple_become,
     test_eval_nin,
     test_eval_nout,
     test_eval_sendto].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_eval_become(_Config) -> 
    %% setup the state
    State0 = #state{name=entity,status=green},
    %% setup the environment (binding structure)
    Before = erl_eval:add_binding('V0', State0, erl_eval:new_bindings()),
    
    %% tokenise, parse, and transform a string
    {ok, Tokens, _End} = dsl_scan:string("become passed."),
    {ok, [ExtTree]}    = dsl_parse:parse_exprs(Tokens), 

    Used = sets:add_element('V0', sets:new()),
    S    = {Used, 'V0'},

    {_, ErlTree}          = dsl_transform:become(ExtTree, S),
    {value, Value, _} = erl_eval:expr(erl_syntax:revert(ErlTree), Before),
    
    #state{status=Passed} = Value,
    Passed = passed.

test_eval_multiple_become(_Config) -> 
    %% setup the state
    State0 = #state{name=entity,status=green},
    %% setup the environment (binding structure)
    Before = erl_eval:add_binding('V0', State0, erl_eval:new_bindings()),
    
    %% tokenise, parse, and transform a string
    {ok, Tokens1, _End} = dsl_scan:string("become first."),
    {ok, [ExtTree1]}      = dsl_parse:parse_exprs(Tokens1), 
    {ok, Tokens2, _End} = dsl_scan:string("become second."),
    {ok, [ExtTree2]}      = dsl_parse:parse_exprs(Tokens2), 
    
    Used = sets:add_element('V0', sets:new()),
    S    = {Used, 'V0'},

    {S1, Tree1} = dsl_transform:become(ExtTree1, S),
    {_,  Tree2} = dsl_transform:become(ExtTree2, S1),

    {value, Value, _} = erl_eval:exprs(erl_syntax:revert_forms([Tree1, Tree2]), Before),
    
    #state{status=Second} = Value,
    Second = second.
  
test_eval_nin(_Config) ->
    %% setup the state
    InList = [{entity1, self()}, {entity2, self()}],
    State0 = #state{name=entity,inlist=InList},
    %% setup the environment (binding structure)
    Before = erl_eval:add_binding('V0', State0, erl_eval:new_bindings()),
   
    Used = sets:add_element('V0', sets:new()),
    S    = {Used, 'V0'},

    Tree = dsl_transform:nin(S),
    Nin = [Name || {Name, _} <- InList],
    {value, Nin, _} = erl_eval:expr(erl_syntax:revert(Tree), Before).
    
test_eval_nout(_Config) ->    
    %% setup the state
    OutList = [{out1, self()}, {out2, self()}],
    State0 = #state{name=who, outlist=OutList},
    %% setup the evaluation environment
    Binding = erl_eval:add_binding('V0', State0, erl_eval:new_bindings()),
       
    Used = sets:add_element('V0', sets:new()),
    S = {Used, 'V0'},
    
    Tree = dsl_transform:nout(S),
    Expect = [Name || {Name, _} <- OutList],
    {value, Expect, _} = erl_eval:expr(erl_syntax:revert(Tree), Binding).
		     
    
test_eval_sendto(_Config) ->
    % setup the state
    OutList = [{entity, self()}],
    State0 = #state{name=entity,outlist=OutList},
    % setup the eval environment
    Binding = erl_eval:add_binding('V0', State0, erl_eval:new_bindings()),
    
    Used = sets:add_element('V0', sets:new()),
    S = {Used, 'V0'},

    {ok, Tokens, _End} = dsl_scan:string("send {num, 42} to [entity]."),
    {ok, [ExtTree]} = dsl_parse:parse_exprs(Tokens),
    
    Tree = dsl_transform:sendto(ExtTree, S),
    {value, ok, _B} = erl_eval:expr(erl_syntax:revert(Tree), Binding),
    receive
	{msg, {num, 42}} ->
	    ok
    after
	1000 ->
	    ct:fail({fail, "Expected message not received within timeout~n"})
    end.
