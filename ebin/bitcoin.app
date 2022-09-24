{
  application, bitcoin,
  [
    {vsn, "1.0.0"},
    {description, "Mine Bitcoins by leveraging the actor model in Erlang. Project for the class of DOSP Fall 2022 at University of Florida"},
    {modules, [app, mine]},
    {env, [{gatorid, "mfasih;"}, {actorcount, 16}, {workunit, 1000000}, {performanceinterval, 10}]}
  ]
}.