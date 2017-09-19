%%%-------------------------------------------------------------------
%%% @author lewis grozinger <lewis@lewis.lewis>
%%% @copyright (C) 2017, lewis grozinger
%%% @doc
%%%
%%% @end
%%% Created : 17 Aug 2017 by lewis grozinger <lewis@lewis.lewis>
%%%-------------------------------------------------------------------
-module(list_misc).

-export([a_subset_of_b/2, same_set/2]).


a_subset_of_b(ListA, ListB) ->
    lists:all(fun(X) -> lists:member(X, ListB) end, ListA).

same_set(ListA, ListB) ->
    lists:sort(ListA) =:= lists:sort(ListB).
