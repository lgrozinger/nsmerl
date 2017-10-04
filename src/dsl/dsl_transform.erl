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
	 nout/1,
	 sendto/2,
	 parse_transform/1,
	 add_clause/2,
	 abs_default_rule/0,
	 rule/5]).

-include_lib("syntax_tools/include/merl.hrl").

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms) ->
    forms(Forms).

forms(Fs) ->   
    %% make catch all rule function
    Rules = abs_default_rule(),
    
    %% traverse forms
    {F1, RulesT} = forms(Fs, Rules),
    
    %% make listen function
    F1 ++ [abs_state_record(), abs_listen_function(), RulesT].
    

forms([F1|Rest], R1) ->
    {Ft, Rt} = form(F1, R1),
    {Fs, Rs} = forms(Rest, Rt),
    case Ft of 
	{} -> {Fs, Rs};
	_  -> {[Ft|Fs], Rs}
    end;
forms([], R1) ->
    {[], R1}.

form(Attribute, Rules) when element(1, Attribute) =:= attribute ->
    {Attribute, Rules};
form(Function, Rules) when element(1, Function) =:= function ->
    {Function, Rules};
form({rule, S, E, P, As}, Rules) ->
    rule(S, E, P, As, Rules).
    
rule({state, S}, {event, E}, {pattern, P}, {actions, As}, Rules) ->
    %% set up the transform state for this scope - the variable names occuring
    %% in P and As are off limits, as is 'State'
    AsUsed = lists:map(fun erl_syntax_lib:variables/1, As),
    PUsed = erl_syntax_lib:variables(P),
    Used = sets:add_element('State', sets:union([PUsed|AsUsed])),
    T = {Used, 'State'},

    io:format("rule found.\n"),
    %% construct a clause
    %% first transform the actions - these are really expressions
    Exprs = exprs(As, T),
    Status = erl_syntax:atom(S),
    Event = erl_syntax:atom(E),
    State = erl_syntax:variable('State'),
    Clause = erl_syntax:clause([Status, Event, P, State], [], Exprs),
    {{}, erl_syntax:revert(add_clause(Clause, Rules))}.
   
exprs([E|Rest], S) ->
    {Et, St} = expr(E, S),
    Es = exprs(Rest, St),
    [Et|Es];
exprs([], _S) ->
    [].

expr({become, S}, State) ->
    {St, F} = become({become, S}, State),
    {F, St};
expr({sendto, Msg, Targets}, State) ->
    {_, Mt} = expr(Msg, State),
    {_, Tt} = expr(Targets, State),
    {S, F} = sendto({sendto, Mt, Tt}, State),
    {F, S};
expr({nin}, State) ->
    {nin(State), State};
expr({nout}, State) ->
    {nout(State), State};
expr(Any, State) ->
    {Any, State}.

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
    {become, {state, NewStatus}} = DslTree,
    
    Pattern = erl_syntax:variable(NewCurrent),
    Body = erl_tree:remote_call('nsmops', 'become', [NewStatus, {var, Current}]),
    T = {{NewUsed, NewCurrent}, erl_syntax:match_expr(Pattern, Body)},
    erl_syntax:revert(T).

%%--------------------------------------------------------------------
%% @doc
%% Returns the syntax tree for the nin statement.
%% @end
%%--------------------------------------------------------------------
-spec nin(TransformState) -> Tree when 
      TransformState :: {sets:set(), atom()},
      Tree           :: erl_syntax:syntaxTree().

nin({_, Current}) ->
    T = erl_tree:remote_call('nsmops', 'nin', [{var, Current}]),
    erl_syntax:revert(T).

%%--------------------------------------------------------------------
%% @doc
%% Returns the syntax tree for the nout statement.
%% @end
%%--------------------------------------------------------------------
-spec nout(TransformState) -> Tree when 
      TransformState :: {sets:set(), atom()},
      Tree           :: erl_syntax:syntaxTree().

nout({_, Current}) ->
    T = erl_tree:remote_call('nsmops', 'nout', [{var, Current}]),
    erl_syntax:revert(T).


%%--------------------------------------------------------------------
%% @doc
%% Returns the syntax tree for the send ... to ... statement
%% @end
%%--------------------------------------------------------------------
-spec sendto(Tree, TransformState) -> ErlTree when 
      Tree :: {sendto, {msg, any()}, {recipients, any()}},
      TransformState :: {sets:set(), atom()},
      ErlTree :: erl_syntax:syntaxTree().

sendto(Tree, TransformState) ->
    {sendto, {msg, AfMsg}, {recipients, AfTargets}} = Tree,
    Nsmops = erl_syntax:atom('nsmops'),
    SendTo = erl_syntax:atom('sendto'),
    {_Used, State} = TransformState,
    CurrentState = erl_syntax:variable(State),
    T = erl_syntax:application(Nsmops, SendTo, [AfMsg, AfTargets,CurrentState]),
    erl_syntax:revert(T).

    
    

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec abs_variable(State | Name) -> {NewState, Tree} | {var, 0, atom()} when
      State    :: {sets:set(), atom()},
      Name     :: atom(),
      NewState :: {sets:set(), atom()},
      Tree     :: erl_syntax:syntaxTree().

abs_variable({U0, _V0}) ->
    V1 = erl_syntax_lib:new_variable_name(U0),
    U1 = sets:add_element(V1, U0),
    {{U1, V1}, {var, 0, V1}};
abs_variable(Name) ->
    {var, 0, Name}.
    


abs_listen_function() ->
    ?Q(["listen(State) -> ",
	"    #state{status=S} = State,",
	"    receive ",
	"        {msg, Msg} -> ",
	"            rule(S, receiving, Msg, State);",
        "        {alarm, Alarm} -> ",
        "            rule(S, alarm, Alarm, State);",
        "        {god, God} -> ",
	"            rule(S, spontaneous, God, State)",
	"    end."]).

abs_default_rule() ->
    ?Q(["rule(_, _, _, State) ->",
	"    listen(State)."]).

    
abs_state_record() ->
    ?Q(["-record(state, {"
	"          name,"
        "          status = green,"
        "          outlist = [],"
	"          inlist = [],"
	"          memory = #{} })."]).
add_clause(C, F) ->
    OldClauses = erl_syntax:function_clauses(F),
    NewClauses = [C|OldClauses],
    erl_syntax:function(erl_syntax:function_name(F), NewClauses).
