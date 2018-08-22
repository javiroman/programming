-module(process).

%% API exports
-export([main/0, call/2, cuadrado/0]).

%%====================================================================
%% API functions
%%====================================================================
main() ->
  test_01(),
  test_02().

%%====================================================================
%% Internal functions
%%====================================================================

%% Test 01
test_01() ->
  do_spawn(),
  timer:sleep(1000),
  io:fwrite("Test 01 bye.\n").

call(Arg1, Arg2) ->
  io:format("~p ~p~n", [Arg1, Arg2]).

do_spawn() ->
  %% Equivalent to SpawnProcess:call("hello", "process"),
  %% The function must be exported in this module (?MODULE).
  Pid = spawn(?MODULE, call, ["hello", "process"]),
  Pid.

%% Test 02
test_02() ->
  Pid = spawn(?MODULE, cuadrado, []),
  Pid ! 2,
  Pid ! 5,
  timer:sleep(1000),
  io:fwrite("Test 02 bye.\n").

cuadrado() ->
  io:format("waiting for messages ...\n"),
  receive
    N ->
      io:format("cuadrado de ~B: ~B\n", [N,N*N]),
      cuadrado()
  end.

