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
    
rule({state, S}, {event, E}, {pattern, P}, {actions, As0}, Rules) ->
    %% set up the transform state for this scope - the variable names occuring
    %% in P and As are off limits, as is 'State'
    AsUsed = lists:map(fun variables/1, As0),
    PUsed = variables(P),
    Used = sets:add_element('State', sets:union([PUsed|AsUsed])),
    T0 = {Used, 'State'},

    io:format("rule found.\n"),
    %% construct a clause
    %% first transform the actions - these are really expressions
    {As1, T1} = exprs(As0, T0),
    Exprs = As1 ++ [abs_recursive_listen(T1)],
    Status = erl_syntax:atom(S),
    Event = erl_syntax:atom(E),
    State = erl_syntax:variable('State'),
    Clause = erl_syntax:clause([Status, Event, P, State], [], Exprs),
    {{}, erl_syntax:revert(add_clause(Clause, Rules))}.
   
exprs([E|Rest], S0) ->
    {Et, S1} = expr(E, S0),
    {Es, S2} = exprs(Rest, S1),
    {[Et|Es], S2};
exprs([], S0) ->
    {[], S0}.

expr({become, S}, State) ->
    {St, F} = become({become, S}, State),
    {F, St};
expr({sendto, Msg, Targets}, State) ->
    {Mt, _} = expr(Msg, State),
    {Tt, _} = expr(Targets, State),
    F = sendto({sendto, Mt, Tt}, State),
    {F, State};
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
    {S, T} = {{NewUsed, NewCurrent}, erl_syntax:match_expr(Pattern, Body)},
    {S, erl_syntax:revert(T)}.

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

abs_recursive_listen({_Used, Current}) ->
    Name = erl_syntax:atom('listen'),
    Arg  = [erl_syntax:variable(Current)],
    erl_syntax:revert(erl_syntax:application(Name, Arg)).

add_clause(C, F) ->
    OldClauses = erl_syntax:function_clauses(F),
    NewClauses = [C|OldClauses],
    erl_syntax:function(erl_syntax:function_name(F), NewClauses).

variables({become, _X}) ->
    sets:new();
variables({sendto, {msg, X}, {recipients, Y}}) ->
    sets:union(variables(X), variables(Y));
variables({nout}) ->
    sets:new();
variables({nin}) ->
    sets:new();
variables(T) ->
    erl_syntax_lib:variables(T).
