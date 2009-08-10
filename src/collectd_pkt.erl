-module(collectd_pkt).

-export([pack/4,
	 pack_plugin/1, pack_plugin_instance/1,
	 pack_type/1, pack_type_instance/1,
	 pack_values/1]).

pack(Hostname, Time, Interval, Parts) ->
    list_to_binary(
      [pack_hostname(Hostname),
       pack_time(Time),
       pack_interval(Interval),
       Parts]).


pack_part(Type, Part) ->
    PartSize = size(Part) + 4,
    <<Type:16/big, PartSize:16/big, Part/binary>>.

pack_string(S) when is_atom(S) ->
    pack_string(atom_to_list(S));
pack_string(S) when is_list(S) ->
    pack_string(list_to_binary(S));
pack_string(S) ->
    <<S/binary, 0>>.

pack_integer(I) ->
    <<I:64/big>>.

pack_hostname(Hostname) ->
    pack_part(0, pack_string(Hostname)).

pack_time(Time) ->
    pack_part(1, pack_integer(Time)).

pack_interval(Interval) ->
    pack_part(7, pack_integer(Interval)).

pack_plugin(Plugin) ->
    pack_part(2, pack_string(Plugin)).

pack_plugin_instance(PluginInstance) ->
    pack_part(3, pack_string(PluginInstance)).

pack_type(Type) ->
    pack_part(4, pack_string(Type)).

pack_type_instance(TypeInstance) ->
    pack_part(5, pack_string(TypeInstance)).

-define(TYPE_COUNTER, 0).
-define(TYPE_GAUGE, 1).

pack_values(TypesValues) ->
    ValuesSize = length(TypesValues),
    Content = list_to_binary([<<ValuesSize:16/big>>,
			      lists:map(fun({counter, _}) ->
						<<?TYPE_COUNTER:8>>;
					   ({gauge, _}) ->
						<<?TYPE_GAUGE:8>>
					end, TypesValues),
			      lists:map(fun({counter, Value}) ->
						<<Value:64/big>>;
					   ({gauge, Value}) ->
						htond(Value)
					end, TypesValues)
			     ]),
    pack_part(6, Content).

htond(Float) ->
    <<_:16, B/binary>> = term_to_binary(Float, [{minor_version, 1}]),
    list_to_binary(
      lists:reverse(
	binary_to_list(B))).
