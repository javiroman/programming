%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Drew Kerrigan All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(riak_mesos_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-include("riak_mesos.hrl").


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
    SCHEDULER = {riak_mesos_scheduler,
        {scheduler, start_link, [riak_mesos_scheduler, scheduler_config()]},
         permanent, brutal_kill, worker, []},
    REQUEST_SERVER = {riak_mesos_request_server,
        {riak_mesos_request_server, start_link, [[]]},
         permanent, 5000, worker, [riak_mesos_request_server]},
    METADATA_SERVER = {riak_mesos_metadata_server,
        {riak_mesos_metadata_server, start_link, [[]]},
         permanent, 5000, worker, [riak_mesos_metadata_server]},
    WEB = {webmachine_mochiweb,
        {webmachine_mochiweb, start, [web_config()]},
         permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [REQUEST_SERVER, METADATA_SERVER, WEB, SCHEDULER],

    {ok, { {one_for_one, 10, 10}, Processes} }.

%%%===================================================================
%%% Private
%%%===================================================================

web_config() ->
    {Ip, Port} = riak_mesos:web_host_port(),
    [
        {ip, Ip},
        {port, Port},
        {nodelay, true},
        {log_dir, "log"},
        {dispatch, dispatch()}
    ].

scheduler_config() ->
    "127.0.1.1:5050".

dispatch() ->
    Resources = [
        riak_mesos_wm_resource:dispatch()
    ],
    lists:flatten(Resources).
