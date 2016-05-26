{application, connscale,
 [{vsn, "1.0.0"},
  %% {modules, [connscale, serv, serv_sup, client, client_sup]},
  {modules, [connscale, serv, serv_sup]},
  {registered, []},
  {env, [
         {listen_port, 8989},
         {acceptors, 20},
         {server, {127,0,0,1}},
         {client1, 'hostname_of_client1'},
         {client2, 'hostname_of_client2'},
         {cookie, yakenlassie},
         {ebin_dir, "/path/to/ebin/dir"}
        ]
  },
  {mod, {connscale, []}}
 ]}.
