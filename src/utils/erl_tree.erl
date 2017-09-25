%%%-------------------------------------------------------------------
%%% @author lewis grozinger <lewis.grozinger@student.manchester.ac.uk>
%%% @copyright (C) 2017, lewis grozinger
%%% @doc
%%% Contains functions for manipulating and creating Erlang abstract 
%%% syntax trees.
%%% @end
%%%-------------------------------------------------------------------
-module(erl_tree).

%% API
-export([function/1,
	 add_clause/2]).

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
    

%%%===================================================================
%%% Internal functions
%%%===================================================================
