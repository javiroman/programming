%%%-------------------------------------------------------------------
%% @doc cowboy_react public API
%% @end
%%%-------------------------------------------------------------------

-module(cowboy_react_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
    io:format("Application listening at: http://localhost:8080 ~n"),
    {ok, Pid} = 'cowboy_react_sup':start_link(),
    Routes = [ {
        '_',
        [
         {"/", cowboy_static, {priv_file, cowboy_react, "build/index.html"}},
         {"/static/[...]", cowboy_static, {priv_dir, cowboy_react, "build/static",
           [{mimetypes, cow_mimetypes, all}]}}
        ]
    } ],
    Dispatch = cowboy_router:compile(Routes),

    TransOpts = [ {ip, {0,0,0,0}}, {port, 8080} ],
    ProtoOpts = #{env => #{dispatch => Dispatch}},

    {ok, _} = cowboy:start_clear(chicken_poo_poo, TransOpts, ProtoOpts),

    {ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
