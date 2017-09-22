%%%-------------------------------------------------------------------
%%% @author lewis grozinger <lewis@lewis.lewis>
%%% @copyright (C) 2017, lewis grozinger
%%% @doc
%%% Tokenises input just as erl_scan would, but adds several categories
%%% which are used by the DSL to define behaviour and topology of the system.
%%% @end
%%% Created : 21 Aug 2017 by lewis grozinger <lewis@lewis.lewis>
%%%-------------------------------------------------------------------
-module(dsl_scan).

-export([reserved_word/1, string/1]).

%%--------------------------------------------------------------------
%% @doc
%% Tests whether or not Word is a reserved word.
%% @param Word the atom to test.
%% @returns true if Word is a reserved word, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec reserved_word(Word) -> true | false when
      Word :: atom().

reserved_word('send')        -> true;
reserved_word('to')          -> true;
reserved_word('receiving')   -> true;
reserved_word('alarm')       -> true;
reserved_word('spontaneous') -> true;
reserved_word('nin')         -> true;
reserved_word('nout')        -> true;
reserved_word(Word)          -> erl_scan:reserved_word(Word).


%%--------------------------------------------------------------------
%% @doc
%% Scans the string Input to produce a list of tokens, which will be used
%% by the parser to construct the Erlang ASF.
%% @param Input the input string to tokenise.
%% @returns a list of tokens taken from Input.
%% @end
%%--------------------------------------------------------------------
-spec string(Input) -> Tokens | Error when
      Input :: string(),
      Tokens :: {ok, erl_scan:tokens(), EndLocation},
      Error :: {error, erl_scan:error_info(), ErrLocation}.

string(Input) ->
    erl_scan:string(Input, 1, {reserved_word_fun, reserved_word/1}).


    
    



