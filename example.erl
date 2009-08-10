-module(example).

-export([start/0, loop/0]).


start() ->
    application:start(collectd),
    collectd:add_server(1),
    loop().

loop() ->
    collectd:set_gauge(ps_count, erlang,
		       [erlang:system_info(process_count), erlang:system_info(schedulers_online)]),
    collectd:inc_counter(counter, inc, [1]),
    lists:foreach(fun({Type, Size}) ->
			  collectd:set_gauge(memory, Type, [Size])
		  end, erlang:memory()),
    {RunTime, _} = erlang:statistics(runtime),
    collectd:set_counter(cpu, runtime, [RunTime]),
    {Reductions, _} = erlang:statistics(reductions),
    collectd:set_counter(counter, reductions, [Reductions]),
    collectd:set_gauge(queue_length, run_queue, [erlang:statistics(run_queue)]),
    sleep(1000),
    ?MODULE:loop().

sleep(I) ->
    receive
	after I ->
		ok
	end.
