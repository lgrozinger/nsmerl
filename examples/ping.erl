-module(ping).

-export([start_ping/0]).

-include("santoro.hrl").

start_ping() ->
    Forms = dslc:file_to_forms("ping_behaviour.nsm"),
    merl:compile_and_load(Forms),
    S0 = example_state(),
    S1 = nsmops:add_to_nout([{master, self()}], S0),
    spawn(ping_behaviour, listen, [S1]).
    
example_state() ->
    State = #state{name=example, status=red}.
    
