%%%-------------------------------------------------------------------
%%% @author javierroman
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Aug 2018 8:15 AM
%%%-------------------------------------------------------------------
-module(registered).
-author("javierroman").

%% API
-export([start2/0, ping/1, pong/0]).

ping(0) ->
  pong ! finished,
  io:format("ping finished~n", []);
ping(N) ->
  pong ! {ping, self()},
  receive
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N - 1).

pong() ->
  receive
    finished ->
      io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
      io:format("Pong received ping~n", []),
      Ping_PID ! pong,
      pong()
  end.

start2() ->
  register(pong, spawn(registered, pong, [])),
  spawn(registered, ping, [3]).
