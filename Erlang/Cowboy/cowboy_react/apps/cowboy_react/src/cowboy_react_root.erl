%%%-------------------------------------------------------------------
%%% @author javierroman
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Sep 2018 11:04 AM
%%%-------------------------------------------------------------------
-module(cowboy_react_root).
-author("javierroman").

%% API
-export([init/2]).

init(Req, Opts) ->
    Req2 = cowboy_req:reply(200,
        %%[{<<"content-type">>, <<"text/plain">>}],
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello Erlang!">>,
        Req),
    {ok, Req2, Opts}.
