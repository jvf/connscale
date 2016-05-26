-module(serv_sup).
-behaviour(supervisor).

-export([start_link/1, start_acceptor/0]).
-export([init/1]).

start_link(ListenSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenSocket]).

init([ListenSocket]) ->
    {ok, {{simple_one_for_one, 60, 3600},
          [{socket,         % ChildId
            {serv, start_link, [ListenSocket]}, % Start func, pass the socket
            temporary,      % Restart strategy
            1000,           % Shutdown timeout
            worker,         % Type
            [serv]}   % Modules
          ]}}.

start_acceptor() ->
    supervisor:start_child(?MODULE, []).
