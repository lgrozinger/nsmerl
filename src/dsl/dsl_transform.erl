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
-export([become/2]).

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
    
    erl_tree:function(test).

    
	      

%%%===================================================================
%%% Internal functions
%%%===================================================================

