{application, client,
 [{vsn, "1.0.0"},
  %% {modules, [connscale, serv, serv_sup, client, client_sup]},
  {modules, [client, client_sup]},
  {registered, []},
  {env, [
         {listen_port, 8989},
         {server, {127,0,0,1}},
         {interval, 5000},
         {timeout, 1000}
        ]
  },
  {mod, {client, []}}
 ]}.
