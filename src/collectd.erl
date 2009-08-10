-module(collectd).

-behaviour(application).

-export([add_server/1, add_server/2, add_server/3,
	 set_gauge/3, inc_counter/3, set_counter/3]).

%% Application callbacks
-export([start/2, stop/1]).

add_server(Interval) ->
    %% ff18::efc0:4a42
    add_server(Interval, {65304,0,0,0,0,0,61376,19010}).

add_server(Interval, Host) ->
    add_server(Interval, Host, 25826).

add_server(Interval, Host, Port) ->
    collectd_sup:add_server(Interval, Host, Port).

set_gauge(Type, TypeInstance, Values) ->
    collectd_sup:cast_all({set_gauge, Type, TypeInstance, Values}).

inc_counter(Type, TypeInstance, Values) ->
    collectd_sup:cast_all({inc_counter, Type, TypeInstance, Values}).

set_counter(Type, TypeInstance, Values) ->
    collectd_sup:cast_all({set_counter, Type, TypeInstance, Values}).

%%====================================================================
%% Application callbacks
%%====================================================================
start(_Type, _StartArgs) ->
    case collectd_sup:start_link() of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
