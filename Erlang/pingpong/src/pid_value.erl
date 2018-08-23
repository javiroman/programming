%%%-------------------------------------------------------------------
%%% @author javierroman
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Aug 2018 8:15 AM
%%%-------------------------------------------------------------------
-module(pid_value).
-author("javierroman").

%% API
-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
  Pong_PID ! finished,
  io:format("ping finished~n", []);
ping(N, Pong_PID) ->
  Pong_PID ! {ping, self()},
  receive
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N - 1, Pong_PID).

pong() ->
  receive
    finished ->
      io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
      io:format("Pong received ping~n", []),
      Ping_PID ! pong,
      pong()
  end.

start() ->
  Pong_PID = spawn(pid_value, pong, []),
  spawn(pid_value, ping, [3, Pong_PID]).
