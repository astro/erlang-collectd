-module(collectd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_server/3, cast_all/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_server(Interval, Host, Port) when is_list(Host) ->
    case inet:getaddr(Host, inet6) of
	{ok, Addr} -> add_server(Interval, Addr, Port);
	{error, _} -> case inet:getaddr(Host, inet) of
			  {ok, Addr} -> add_server(Interval, Addr, Port);
			  {error, Reason} -> {error, Reason}
		      end
    end;
add_server(Interval, Host, Port) ->
    supervisor:start_child(?SERVER,
			   {{server, Host, Port},
			    {collectd_server, start_link, [Interval, Host, Port]},
			    permanent, 1000000, worker, [collectd_server]}).

cast_all(Msg) ->
    Children = supervisor:which_children(?SERVER),
    lists:foreach(fun({_Id, Pid, _Type, _Modules}) ->
			  gen_server:cast(Pid, Msg)
		  end, Children).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    {ok,{{one_for_one,1,1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
