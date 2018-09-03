-module(client).

-export([allocate/0, deallocate/1]).

allocate() ->
    gen_server:call(frequency, {allocate, self()}).

deallocate(Frequency) ->
    gen_server:cast(frequency, {deallocate, Frequency}).
