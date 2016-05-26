-module(serv_sup).
-behaviour(supervisor).

-export([start_link/1, start_acceptor/0]).
-export([init/1]).

start_link(ListenSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenSocket]).

init([ListenSocket]) ->
    %% Port = 8091,
    %% {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {keepalive,true}]),
    %% spawn_link(fun initial_acceptors/0),
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

%% start 20 initial acceptors to allow accepting multiple connections in parallel
%% initial_acceptors() ->
%%     [start_acceptor() || _ <- lists:seq(1,1)],
%%     ok.
