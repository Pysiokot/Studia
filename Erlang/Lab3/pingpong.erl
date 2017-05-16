-module(pingpong).

-export([ping_loop/0, pong_loop/0, start/0, stop/0, play/1]).

start() ->
    register(ping, spawn(pingpong, ping_loop, [])),
    register(pong, spawn(pingpong, pong_loop, [])).

ping_loop() ->
    receive
        stop -> ok;
        0 -> timer:sleep(1000), ping_loop();
        N -> pong ! N-1, timer:sleep(1000), io:format("Ping ~n", []), ping_loop()
    after 
        20000 -> ok
    end.

pong_loop() ->
    receive
        stop -> ok;
        0 -> timer:sleep(1000), pong_loop();
        N -> ping ! N-1, timer:sleep(1000), io:format("Pong ~n", []), pong_loop()
    after
        20000 -> ok
    end.

stop() ->
    ping ! stop,
    pong ! stop.

play(N) -> ping ! N.