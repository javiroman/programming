%%%-------------------------------------------------------------------
%% @doc cowboy_hello public API
%% @end
%%%-------------------------------------------------------------------

-module(cowboy_hello_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
 {ok, Pid} = 'cowboy_hello_sup':start_link(),
    Routes = [ {
        '_',
        [
            {"/", cowboy_hello_root, []}
        ]
    } ],
    Dispatch = cowboy_router:compile(Routes),

    TransOpts = [ {ip, {0,0,0,0}}, {port, 2938} ],
    ProtoOpts = #{env => #{dispatch => Dispatch}},

    {ok, _} = cowboy:start_clear(chicken_poo_poo, TransOpts, ProtoOpts),

    {ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
