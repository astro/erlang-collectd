-module(collectd_pkt).

-export([pack/4]).

pack(Hostname, Time, Interval, Parts) ->
    list_to_binary(
      [pack_hostname(Hostname),
       pack_time(Time),
       pack_interval(Interval),
       lists:map(
	 fun({Plugin, PluginInstances}) ->
		 [pack_plugin(Plugin),
		  lists:map(
		    fun({PluginInstance, Types}) ->
			    [pack_plugin_instance(PluginInstance),
			     lists:map(
			       fun({Type, TypeInstances}) ->
				       [pack_type(Type),
					lists:map(
					  fun({TypeInstance, Values}) ->
						  pack_values(Values)
					  end, TypeInstances)]
			       end, Types)]
		    end, PluginInstances)]
	 end, Parts)]).


pack_part(Type, Part) ->
    PartSize = size(Part) + 4,
    <<Type:16/big, PartSize:16/big, Part>>.

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
    pack_string(Plugin).

pack_plugin_instance(PluginInstance) ->
    pack_string(PluginInstance).

pack_type(Type) ->
    pack_string(Type).

pack_type_instance(TypeInstance) ->
    pack_string_instance(TypeInstance).

-define(TYPE_COUNTER, 0).
-define(TYPE_GAUGE, 1).

pack_values(TypesValues) ->
    ValuesSize = length(TypesValues),
    Content = list_to_binary([<<TypesValues:16/big>>,
			      lists:map(fun({Type, _}) ->
						<<Type:8>>
					end, TypesValues),
			      lists:map(fun({?TYPE_COUNTER, Value}) ->
						<<Value:64/big>>;
					   ({?TYPE_GAUGE, Value}) ->
						htond(Value)
					end, TypesValues)
			     ]),
    pack_part(6, Content).

htond(Float) ->
    <<_:16, B/binary>> = term_to_binary(Float, [{minor_version, 1}]),
    B.
