-module(pingpong).

%% API exports
-export([main/0]).
-import(pid_value, [start/0]).
-import(registered, [start2/0]).

%%====================================================================
%% API functions
%%====================================================================

main() ->
  pid_value:start(),
  registered:start2().

%%====================================================================
%% Internal functions
%%====================================================================
%% If we want to send a message to a process, then we need to
%% know its PID, but when a process is created, only the parent
%% process knows the PID. No other process in the system knows
%% about the process. Erlang has a method for publishing a process
%% identifier so that any process in the system can communicate
%% with this process. Such a process is called a registered process.
%% The act of giving a name to a process allows you to replace the
%% unpredictable pid stored in a variable by an atom.

%% http://erlang.org/doc/getting_started/conc_prog.html