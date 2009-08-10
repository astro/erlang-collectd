-module(collectd_server).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {sock, interval, host, port, values}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Interval, Host, Port) ->
    gen_server:start_link(?MODULE, [Interval, Host, Port], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Interval, Host, Port]) ->
    AF = case Host of
	     {_, _, _, _} -> inet;
	     {_, _, _, _, _, _, _, _} -> inet6
	 end,
    {ok, Sock} = gen_udp:open(0, [AF]),
    I = self(),
    Timeout = trunc(Interval * 1000),
    spawn_link(fun() ->
		       timer(I, Timeout)
	       end),
    {ok, #state{sock = Sock, interval = Interval,
		host = Host, port = Port,
		values = collectd_values:new()}, Interval}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(timer, #state{sock = Sock,
			  host = Host, port = Port,
			  interval = Interval,
			  values = Values} = State) ->
    send_packet(Sock, Host, Port, Interval, Values),
    Values2 = collectd_values:forget_gauges(Values),
    {noreply, State#state{values = Values2}};

handle_cast({set_gauge, Type, TypeInstance, Values},
	    #state{values = Values1} = State) ->
    Values2 = collectd_values:set_gauge(Values1, Type, TypeInstance, Values),
    {noreply, State#state{values = Values2}};

handle_cast({inc_counter, Type, TypeInstance, Values},
	    #state{values = Values1} = State) ->
    Values2 = collectd_values:inc_counter(Values1, Type, TypeInstance, Values),
    {noreply, State#state{values = Values2}};

handle_cast({set_counter, Type, TypeInstance, Values},
	    #state{values = Values1} = State) ->
    Values2 = collectd_values:set_counter(Values1, Type, TypeInstance, Values),
    {noreply, State#state{values = Values2}}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{sock = Socket}) ->
    ok = gen_udp:close(Socket).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

send_packet(Sock, Host, Port, Interval, Values) ->
    {MS, S, _} = erlang:now(),
    Time = MS * 1000000 + S,
    [Name, Hostname | _] = string:tokens(atom_to_list(node()), "@"),
    Parts = [collectd_pkt:pack_plugin("erlang"),
	     collectd_pkt:pack_plugin_instance(Name)
	     | lists:map(fun({type, Type}) ->
				 collectd_pkt:pack_type(Type);
			    ({type_instance, TypeInstance}) ->
				 collectd_pkt:pack_type_instance(TypeInstance);
			    ({values, ValuesType, Values1}) ->
				 Values2 = [{ValuesType, Value}
					    || Value <- Values1],
				 collectd_pkt:pack_values(Values2)
			 end, collectd_values:to_list(Values))],
    Pkt = collectd_pkt:pack(Hostname, Time, Interval, Parts),
    ok = gen_udp:send(Sock, Host, Port, Pkt).


timer(Pid, Timeout) ->
    gen_server:cast(Pid, timer),
    receive
	after Timeout ->
		timer(Pid, Timeout)
	end.
