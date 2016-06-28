-module(client_sup).
-behaviour(supervisor).

-export([start/0, start_link/0, start_clients/1, stop_clients/1]).
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

start_clients(NoOfClients) ->
    do_start_clients(NoOfClients, []).

do_start_clients(0, Acc) ->
    Acc;
do_start_clients(NoOfClients, Acc) ->
    {ok, ClientPid} = supervisor:start_child(?MODULE, [NoOfClients]),
    do_start_clients(NoOfClients-1, [ClientPid|Acc]).


stop_clients(NoOfClients) ->
    ChildDetails = supervisor:which_children(?MODULE),
    Children = lists:map(fun({_Id, Pid, _Type, _Module}) -> Pid end, ChildDetails),
    do_stop_clients(NoOfClients, Children).

do_stop_clients(0, _) ->
    ok;
do_stop_clients(_, []) ->
    io:format("less children than requested to stop");
do_stop_clients(NoOfClients, [C|Children]) ->
    ok = supervisor:terminate_child(?MODULE, C),
    do_stop_clients(NoOfClients-1, Children).

