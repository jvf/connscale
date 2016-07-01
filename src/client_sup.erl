-module(client_sup).
-behaviour(supervisor).

-export([start/0, start_link/0]).
-export([init/1]).

start() ->
    supervisor:start({local, ?MODULE}, ?MODULE, []).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 60, 3600},
    %% {ok, {{one_for_one, 60, 3600},
          [{client,         % ChildId
            {client, start_link, []}, % Start func, pass the socket
            temporary,      % Restart strategy
            1000,           % Shutdown timeout
            worker,         % Type
            [client]}   % Modules
          ]}}.



