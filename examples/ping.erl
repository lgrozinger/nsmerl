-module(ping).

-export([start_ping/1, ping/1, red_to_green/1]).

-include("santoro.hrl").

start_ping(Target) ->
    code:add_pathsz(["../ebin"]),
    Forms = dslc:file_to_forms("ping_behaviour.nsm"),
    merl:compile_and_load(Forms),
    S0 = example_state(),
    S1 = nsmops:add_to_nout([{master, self()}], S0),

    load_deps(Target),

    %% remote load the binaries
    {ok, M1, B1} = merl:compile(Forms),
    {module, ping_behaviour} = rpc:call(Target, code, load_binary, [M1, "", B1]),
    spawn(Target, ping_behaviour, listen, [S1]).

ping(Pid) ->
    Pid ! {msg, {master, erlang:system_time(microsecond)}},
    receive {msg, Reply} ->
	    io:format("reply: ~w~n", [Reply])
    after 3000 ->
	    io:format("reply not received within timeout.~n")
    end.

red_to_green(Pid) ->
    Pid ! {god, {start}}.
	
example_state() ->
    #state{name=example, status=red}.
    
load_deps(Target) ->
    ObjList = lists:map(fun get_dep_object/1, deps()),
    [rpc:call(Target, code, load_binary, [M, "", B]) || {M, B} <- ObjList].

deps() ->
    [list_misc, nsmops].

get_dep_object(Module) ->
    {M, Bin, _F} = code:get_object_code(Module),
    {M, Bin}.
    
