%%%-------------------------------------------------------------------
%%% @author lewis grozinger <lewis.grozinger@student.manchester.ac.uk>
%%% @copyright (C) 2017, lewis grozinger
%%% @doc
%%% Contains functions for manipulating and creating Erlang abstract 
%%% syntax trees.
%%% @end
%%%-------------------------------------------------------------------
-module(erl_tree).

-include_lib("syntax_tools/include/merl.hrl").

%% API
-export([function/1,
	 add_clause/2,
	 remote_call/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns a function with no clauses and the name Name.
%% @params Name the name of the function.
%% @returns The erl_syntax syntax tree of a function with no clauses 
%% and name Name.
%% @end
%%--------------------------------------------------------------------
-spec function(Name) -> Tree when 
      Name :: atom() | string(),
      Tree :: erl_syntax:syntaxTree().

function(Name) ->
    erl_syntax:function(Name, []).

%%--------------------------------------------------------------------
%% @doc
%% Takes a syntaxTree() representing a function and returns a new 
%% tree representing the same function but with an extra clause.
%% @params NewClause the clause to add to the function.
%% @params Function the function tree to add the clause to.
%% @returns a tree representing the same function, but with NewClause
%% added to its list of clauses.
%% @end
%%--------------------------------------------------------------------
-spec add_clause(NewClause, Function) -> Tree when
      NewClause :: erl_syntax:syntaxTree(),
      Function  :: erl_syntax:syntaxTree(),
      Tree      :: erl_syntax:syntaxTree().

add_clause(NewClause, Function) ->
    OldClauses = erl_syntax:function_clauses(Function),
    NewClauses = [NewClause|OldClauses],
    erl_syntax:function(erl_syntax:function_name(Function), NewClauses).
    
%%--------------------------------------------------------------------
%% @doc
%% Constructs an Erlang syntax tree representing a remote function call.
%% That is the result represents the code "module:function(Args)" if the
%% arguments given are module, function and Args respectively.
%% @param Module the name of the module which contains the function.
%% @param Functions the name of the function to be applied.
%% @param Args a list of arguments to be passed to the function.
%% @returns an Erlang syntax tree representation of the function call.
%% @end
%%--------------------------------------------------------------------
-spec remote_call(Module, Function, Args) -> Tree when
      Module   :: atom(),
      Function :: atom(),
      Args     :: [any()],
      Tree     :: erl_syntax:syntaxTree().
						  
remote_call(Module, Function, Args) ->
    ModuleTree   = erl_syntax:atom(Module),
    FunctionTree = erl_syntax:atom(Function),
    OperatorTree = erl_syntax:module_qualifier(ModuleTree, FunctionTree),
    
    % construct the new argument list with the terms in abstract form
    AbstractArgs = lists:map(fun merl_convert/1, Args),
    
    erl_syntax:application(OperatorTree, AbstractArgs).
     

%%%===================================================================
%%% Internal functions
%%%===================================================================

merl_convert({var, VarName}) ->
    erl_syntax:variable(VarName);
merl_convert(Term) ->
    ?Q("_@Term@").


