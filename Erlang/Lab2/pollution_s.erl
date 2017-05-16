-module(pollution_s).
-behaviour(gen_server).
-version('1.0').

-export([start_link/1, init/1, send/1, handle_call/3]).

start_link(_) ->
    gen_server:start_link({local, pollution_s}, pollution_s, null, []).

init(_) ->
    {ok, pollution:createMonitor()}.

send(Msg) ->
    gen_server:call(pollution_s, Msg).

handle_call({addStation, Name, Coords}, _, Monitor) ->
    case pollution:addStation(Name, Coords, Monitor) of
        {error, Msg} -> {reply, Msg, Monitor};
        NewMonitor -> {reply, ok, NewMonitor}
    end.