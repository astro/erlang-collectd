-module(collectd_values).

-export([new/0, set_gauge/3, inc_counter/3, set_counter/3, make_parts/1]).

new() ->
    [].

set_val(Values, [], NewValue) ->
    [NewValue | Values];
set_val(Values, [K | R], NewValue) ->
    case lists:keysearch(K, 1, Values) of
	false ->
	    [set_val1([], R, NewValue) | Values];
	{true, {K, SubValues1}} ->
	    SubValues2 = set_val(SubValues1, R, NewValue),
	    lists:keyreplace(K, 1, Values, {K, SubValues2})
    end.

map_vals(Values, F) ->
    map_vals1(Values, F, []).

map_vals1(Values, F, PPTT) when size(PPTT) == 4 ->
    F(lists:reverse(PPTT), Values);
map_vals1(Values, F, PPTT) ->
    lists:map(fun({K, SubValues}) ->
		      {K, map_vals(SubValues, F, [K | PPTT])}
	      end, Values).

set_gauge(Values, PPTT, Gauges) ->
    set_val(Values, PPTT, {gauge, Gauges}).

inc_counter(Values, PPTT, Increment) ->
    NewValues = map_vals(Values, fun(PPTT, {counter, OldValues}) ->
					 {counter,
					  zipwith(fun(Old, New) ->
							  Old + New
						  end, OldValues, NewValues)}
				 end),
    if
	NewValues == Values ->
	    set_val(Values, PPTT, Increment);
	true ->
	    incremented
    end.

set_counter(Values, PPTT
