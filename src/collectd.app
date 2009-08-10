{application, collectd,
 [{description, "collectd client"},
  {vsn, "0.0.1"},
  {modules, [

  ]},
  {registered, [collectd_sup]},
  {mod, {collectd, [collectd, collectd_pkt, collectd_server,
		    collectd_sup, collectd_values]}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
