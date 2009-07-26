-module(collectd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_server(Interval, Host, Port) when is_list(Host) ->
    case inet:getaddrs(Host, inet6) of
	{ok, Addr} -> add_server(Addr, Port);
	{error, _} -> case inet:getaddrs(Host, inet) of
			  {ok, Addr} -> add_server(Addr, Port);
			  {error, Reason} -> {error, Reason}
		      end
    end;
add_server(Interval, Host, Port) ->
    supervisor:start_child({local, ?SERVER},
			   {{server, Host, Port},
			    {collectd_server, start_link, [Interval, Host, Port]},
			    permanent, 1000000, worker, [collectd_server]}).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    {ok,{{one_for_one,1,1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
