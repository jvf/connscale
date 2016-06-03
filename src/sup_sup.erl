-module(sup_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% {ok, {{simple_one_for_one, 60, 3600},
    {ok, {{one_for_one, 2, 3600},
          [{client_sup,         % ChildId
            {client_sup, start_link, []}, % Start func, pass the socket
            transient,      % Restart strategy
            1000,           % Shutdown timeout
            supervisor,         % Type
            [client_sup]}   % Modules
          ]}}.

