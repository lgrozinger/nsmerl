%%%-------------------------------------------------------------------
%%% @author lewis grozinger <lewis@lewis.lewis>
%%% @copyright (C) 2017, lewis grozinger
%%% @doc
%%% Functions which perform the transformation from dsl_parse trees to
%%% erl_parse trees, i.e. from the behaviour DSL to standard Erlang.
%%% @end
%%%-------------------------------------------------------------------
-module(dsl_transform).

%% API
-export([become/2,
	 nin/1,
	 nout/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the syntax tree for a become statement.
%% @end
%%--------------------------------------------------------------------
-spec become(DslTree, TransformState) -> {NewTransformState, Tree} when
      DslTree           :: {become, State},
      State             :: atom(),
      TransformState    :: {sets:set(), atom()},
      NewTransformState :: {sets:set(), atom()},
      Tree              :: erl_syntax:syntaxTree().

become(DslTree, {Used, Current}) ->
    NewCurrent = erl_syntax_lib:new_variable_name(Used),
    NewUsed    = sets:add_element(NewCurrent, Used),
    [{become, {state, NewStatus}}] = DslTree,
    
    Pattern = erl_syntax:variable(NewCurrent),
    Body = erl_tree:remote_call('nsmops', 'become', [NewStatus, {var, Current}]),
    {{NewUsed, NewCurrent}, erl_syntax:match_expr(Pattern, Body)}.

%%--------------------------------------------------------------------
%% @doc
%% Returns the syntax tree for the nin statement.
%% @end
%%--------------------------------------------------------------------
-spec nin(TransformState) -> Tree when 
      TransformState :: {sets:set(), atom()},
      Tree           :: erl_syntax:syntaxTree().

nin({_, Current}) ->
    erl_tree:remote_call('nsmops', 'nin', [{var, Current}]).

%%--------------------------------------------------------------------
%% @doc
%% Returns the syntax tree for the nout statement.
%% @end
%%--------------------------------------------------------------------
-spec nout(TransformState) -> Tree when 
      TransformState :: {sets:set(), atom()},
      Tree           :: erl_syntax:syntaxTree().

nout({_, Current}) ->
    erl_tree:remote_call('nsmops', 'nout', [{var, Current}]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

