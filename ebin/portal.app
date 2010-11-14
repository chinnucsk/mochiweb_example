{application, portal,
 [{description, "portal"},
  {vsn, "0.01"},
  {modules, [
    portal,
    portal_app,
    portal_sup,
    portal_web,
    portal_deps
  ]},
  {registered, []},
  {mod, {portal_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
