{application, server,
 [{vsn, "1.0.0"},
  {modules, [server, server_sup, rclient]},
  {registered, []},
  {env, [   % see example.config
         {listen_port, 8989},
         {acceptors, 20},
         {server, {127,0,0,1}},
         {client1, 'hostname_of_client1'},
         {client2, 'hostname_of_client2'},
         {cookie, yakenlassie},
         {ebin_dir, "/path/to/ebin/dir"}
        ]
  },
  {mod, {server, []}}
 ]}.
