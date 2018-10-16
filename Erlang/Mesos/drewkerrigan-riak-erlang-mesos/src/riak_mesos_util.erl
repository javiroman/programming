-module(riak_mesos_util).

-export([generate_uuid/0]).

generate_uuid() ->
   uuid:uuid_to_string(uuid:get_v4(weak), binary_standard).
