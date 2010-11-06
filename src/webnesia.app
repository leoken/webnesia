{application, webnesia,
 [{description, "webnesia"},
  {vsn, "0.01"},
  {modules, [
    webnesia,
    webnesia_app,
    webnesia_sup,
    webnesia_web,
    webnesia_deps,
	webnesia_db,
	webnesia_response
  ]},
  {registered, []},
  {mod, {webnesia_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
