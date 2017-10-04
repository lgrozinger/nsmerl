%%%-------------------------------------------------------------------
%%% @author Lewis Grozinger <lewis.grozinger@student.manchester.ac.uk>
%%% @doc The module provides functions which implement the semantics of
%%% the primitives described by N.Santoro in 'Design and Analysis of
%%% Distributed Algorithms'.
%%% Functions which alter the state of the vertex/node on which they operate
%%% return the new modified state. Operations which do not alter state 
%%% in general only return some value or perform some side effect (message
%%% passing).
%%% <h3>Some terminology</h3>
%%% <ul>
%%%   <li>State refers to the state record defined in santoro.hrl and 
%%%       encapsulates the in-list, out-list, name, status and local memory of
%%%       the node.
%%%   </li>
%%%   <li>Status refers to the state of the node at any point in time, and
%%%       effects which rule is executed in response to an event.
%%%   </li>
%%%   <li>Rules are sets of actions to be executed by the node in response
%%%       to events; which rule is triggered in response to an event also 
%%%       depends on the node's status.
%%%   </li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(nsmops).

-include("santoro.hrl").

-export([become/2, 
	 sendto/3, 
	 mem_get/2, 
	 mem_put/3, 
	 nin/1, 
	 nout/1,
	 add_to_nout/2,
	 add_to_nin/2,
	 sub_from_nout/2,
         sub_from_nin/2]).


%%--------------------------------------------------------------------
%% @doc 
%% Takes a state and a target status name and returns a new state
%% which is a copy of the old but with 'status' set to the target status.
%% @param NewStatus The name of the target status for this state.
%% @param CurrentState The state record to return an altered copy of.
%% @returns A new state record, a copy of CurrentState, but with status
%% NewStatus.
%% @end
%%--------------------------------------------------------------------
-spec become(NewStatus, CurrentState) -> NewState when
      NewStatus    :: atom(),
      CurrentState :: state(), 
      NewState     :: state().

become(NewStatus, CurrentState) ->
    CurrentState#state{status = NewStatus}.


%%--------------------------------------------------------------------
%% @doc Sends Message to all the vertices with names in SendList, provided
%% that the name appears in the outlist of CurrentState.
%% @param Message the message to be sent, which is some tuple.
%% @param SendList a list of names addressing vertices to which the message
%% should be sent.
%% @param CurrentState the state record of the sending vertex.
%% @returns ok if the operation is a success, {error, Reason} otherwise.
%% @end
%%--------------------------------------------------------------------
-spec sendto(Message, SendList, CurrentState) -> ok | {error, Reason} when
      Message      :: any(),
      SendList     :: [atom()],
      CurrentState :: state(),
      Reason       :: string().

sendto(Message, SendList, CurrentState) ->
    #state{outlist = OutList} = CurrentState,
    %% tests if each name in the SendList is a name in the OutList
    %% stops vertices from sending messages to those not on their OutList
    case list_misc:a_subset_of_b(SendList, [N || {N, __P} <- OutList]) of
	true  -> send_msg_to_names({msg, Message}, SendList, OutList),
		 ok;
	false -> {error, "SendList contains names which Nout does not."}
    end.


%%--------------------------------------------------------------------
%% @doc Returns the erlang term which is the value of the entry in the 
%% CurrentState record's memory map with key Key, or {error, Reason} if
%% for example, the key does not already exist.
%% @param Key the key corresponding to the desired entry in the map, in 
%% other words the "variable" name.
%% @param CurrentState a state record representing the current state of
%% the memory of the vertex.
%% @returns the value of the "variable" called Key, or {error, Reason} if
%% for example, the "variable" has not yet been "assigned".
%% @end
%%--------------------------------------------------------------------
-spec mem_get(Key, CurrentState) -> Value | {error, Reason} when
      Key          :: string(),
      CurrentState :: state(),
      Value        :: any(),
      Reason       :: string().

mem_get(Key, CurrentState) ->
    #state{memory = Mem} = CurrentState,
    try
	#{Key := Value} = Mem,
	Value
    catch
	error:__X -> {error, "Illegal access to unassigned variable."}
    end.

%%--------------------------------------------------------------------
%% @doc Updates or creates an entry in the CurrentState record's memory
%% map with value Value, emulating variable assignment or declaration and
%% assignment.
%% @param Key the name of the emulated variable.
%% @param Value the value which should be assigned to the emulated variable.
%% @param CurrentState the state record representing the current state of the
%% vertex.
%% @returns A new state record, which is a copy of CurrentState, except that 
%% it's memory map reflects the assignment.
%% @end
%%--------------------------------------------------------------------
-spec mem_put(Key, Value, CurrentState) -> NewState | {error, Reason} when
      Key          :: string(),
      CurrentState :: state(),
      NewState     :: state(),
      Value        :: any(),
      Reason       :: string().

mem_put(Key, Value, CurrentState) ->
    #state{memory = Mem} = CurrentState,
    NewMem = Mem#{Key => Value},
    CurrentState#state{memory = NewMem}.

%%--------------------------------------------------------------------
%% @doc Gets the list of out-neighbours from a vertex's state record.
%% @param State the state record of the vertex.
%% @returns the list of out-neighbours of the vertex.
%% @end
%%--------------------------------------------------------------------
-spec nout(State) -> NameList when
      State :: state(),
      NameList :: [atom()]. 

nout(#state{outlist = Nout}) ->
    [Name || {Name, __Pid} <- Nout].

%%--------------------------------------------------------------------
%% @doc Gets the list of in-neighbours from a vertex's state record.
%% @param State the state record of the vertex.
%% @returns the list of in-neighbours of the vertex.
%% @end
%%--------------------------------------------------------------------
-spec nin(State) -> NameList when
      State :: state(),
      NameList :: [atom()]. 

nin(#state{inlist = Nin}) ->
    [Name || {Name, __Pid} <- Nin].

%%--------------------------------------------------------------------
%% @doc Adds the neighbours in the Extras list to the Nout list of the 
%% vertex.
%% @param Extras the neighbours to add to the outlist.
%% @param CurrentState the current state record containing the outlist
%% to mutate.
%% @returns the modified state.
%% @end
%%--------------------------------------------------------------------
-spec add_to_nout(Extras, CurrentState) -> NewState when
      Extras       :: [neighbour()],
      CurrentState :: state(),
      NewState     :: state().

add_to_nout(Extras, CurrentState) -> 
    #state{outlist = CurrentNout} = CurrentState,
    CurrentState#state{outlist = lists:append(CurrentNout, Extras)}.

%%--------------------------------------------------------------------
%% @doc Adds the neighbours in the Extras list to the Nin list of the 
%% vertex.
%% @param Extras the neighbours to add to the inlist.
%% @param CurrentState the current state record containing the inlist
%% to mutate.
%% @returns the modified state.
%% @end
%%--------------------------------------------------------------------
-spec add_to_nin(Extras, CurrentState) -> NewState when
      Extras       :: [neighbour()],
      CurrentState :: state(),
      NewState     :: state().

add_to_nin(Extras, CurrentState) -> 
    #state{inlist = CurrentNin} = CurrentState, 
    CurrentState#state{inlist = lists:append(CurrentNin, Extras)}.
    
%%--------------------------------------------------------------------
%% @doc Removes the neighbours in the Victims list from the Nin list of
%% the vertex.
%% @param Victims the neighbours to remove from the inlist.
%% @param CurrentState the current state record containing the inlist to 
%% mutate.
%% @returns the modified state.
%% @end
%%--------------------------------------------------------------------
-spec sub_from_nin(Victims, CurrentState) -> NewState when
      Victims      :: [neighbour()],
      CurrentState :: state(),
      NewState     :: state().

sub_from_nin([], CurrentState) -> CurrentState;
sub_from_nin([H|T], CurrentState) ->
    sub_from_nin(T, sub_from_nin1(H, CurrentState)).
 
sub_from_nin1(Neighbour, CurrentState) ->
    #state{inlist = CurrentNin} = CurrentState,
    CurrentState#state{inlist = lists:keydelete(Neighbour, 1, CurrentNin)}.

%%--------------------------------------------------------------------
%% @doc Removes the neighbours in the Victims list from the Nout list of
%% the vertex.
%% @param Victims the neighbours to remove from the outlist.
%% @param CurrentState the current state record containing the outlist to 
%% mutate.
%% @returns the modified state.
%% @end
%%--------------------------------------------------------------------
-spec sub_from_nout(Victims, CurrentState) -> NewState when
      Victims      :: [neighbour()],
      CurrentState :: state(),
      NewState     :: state().

sub_from_nout([], CurrentState) -> CurrentState;
sub_from_nout([H|T], CurrentState) ->
    sub_from_nout(T, sub_from_nout1(H, CurrentState)).
 
sub_from_nout1(Neighbour, CurrentState) ->
    #state{outlist = CurrentNout} = CurrentState,
    CurrentState#state{outlist = lists:keydelete(Neighbour, 1, CurrentNout)}.

%%--------------------------------------------------------------------
get_pid_from_name(Name, AddressList) ->
    {Name, Pid} = lists:keyfind(Name, 1, AddressList),
    Pid.

send_msg_to_names(Msg, NameList, AddressList) ->
    lists:foreach(fun(X)-> send_msg_to_name(Msg, X, AddressList) end, NameList).

send_msg_to_name(Msg, Name, AddressList) ->
    Pid = get_pid_from_name(Name, AddressList),
    Pid ! Msg.
