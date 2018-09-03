-module(server).
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).
-export([format_status/2]).

start() ->
    gen_server:start_link({local, frequency}, frequency, [], []).

stop() ->
    gen_server:cast(frequency, stop).

init(_Args) ->
    Frequencies = {get_frequencies(), []},
    {ok, Frequencies}.

get_frequencies() -> [10,11,12,13,14, 15].

handle_call({allocate, Pid}, _From, Frequencies) ->
    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
    {reply, Reply, NewFrequencies}.

handle_cast({deallocate, Freq}, Frequencies) ->
    NewFrequencies = deallocate(Frequencies, Freq),
    {noreply, NewFrequencies};

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_info(_Msg, LoopData) ->
    {noreply, LoopData}.

terminate(_Reason, _LoopData) ->
    ok.

format_status(_Opt, [_ProcDict, {Available, Allocated}]) ->
    {data, [{"State", {{available, Available}, {allocated, Allocated}}}]}.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Res|Resources], Allocated}, Pid) ->
    {{Resources, [{Res, Pid}|Allocated]}, {ok, Res}}.

deallocate({Free, Allocated}, Res) ->
    NewAllocated = lists:keydelete(Res, 1, Allocated),
    {[Res|Free],  NewAllocated}.
