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

-module(riak_mesos).
-behaviour(application).

-export([start/2, stop/1]).
-export([web_host_port/0]).
-export([run_command/2, run_docker_container/3]).

-include("riak_mesos.hrl").

%%%===================================================================
%%% API
%%%===================================================================

web_host_port() ->
    case application:get_env(riak_mesos, listenter_web_http) of
        {ok, {_, _} = HostPort} -> HostPort;
        undefined -> {"0.0.0.0", 9000}
    end.

run_command(Name, Command) ->
    Request = #provision_request{ name=Name, start_command=Command, type= <<"command">> },
    {ok, Identifier} = riak_mesos_request_server:new_request(Request),
    {ok, Identifier}.

run_docker_container(Name, ImageUri, Command) ->
	Request = #provision_request{ name=Name, start_command=Command, type= <<"docker">>, docker_image=ImageUri },
    {ok, Identifier} = riak_mesos_request_server:new_request(Request),
    {ok, Identifier}.

%%%===================================================================
%%% Callbacks
%%%===================================================================

start(_Type, _StartArgs) ->
    {ok, Pid} = riak_mesos_sup:start_link(),
    riak_mesos_cli:load_schema(),
    riak_mesos_cli:register(),
    {ok, Pid}.

stop(_State) ->
    ok.
