-module(connscale).

-export([start/0]).

% application behaviour
-export([start/2, stop/1]).

% Client API
-export([client_start/1, client_stop/1, connections_start/2, connections_stop/2]).

%% to use with erl -run/-s
start() ->
    io:format("starting ~w application~n", [?MODULE]),
    application:start(?MODULE).

%% Start the connscale app. Opens a listen port at the port defined in the env
%% and opens Acceptors number of concurrent acceptors.
start(normal, _Args) ->
    {ok, Port} = application:get_env(listen_port),
    {ok, Acceptors} = application:get_env(acceptors),
    {ok, Cookie} = application:get_env(cookie),
    erlang:set_cookie(node(), Cookie),
    io:format("listening on port ~w~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {reuseaddr, true}]),
    {ok, Sup} = serv_sup:start_link(ListenSocket),
    _ = [ supervisor:start_child(Sup, []) || _ <- lists:seq(1,Acceptors)],
    {ok, Sup, ListenSocket}.

%% Stop the connscale app (closes the listen socket)
stop(ListenSocket) ->
    ok = gen_tcp:close(ListenSocket),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Client Interface %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Starts a remote node and client application at the client host defined as
%% ClientId in the env. ClientId must be a key in the env of the application
%% indicating the hostname of machine to host the client.
client_start(ClientId) ->
    {ok, HostName} = application:get_env(?MODULE, ClientId),
    {ok, Cookie} = application:get_env(?MODULE, cookie),
    {ok, EbinDir} = application:get_env(?MODULE, ebin_dir),
    {ok, ListenPort} = application:get_env(?MODULE, listen_port),
    {ok, Server} = application:get_env(?MODULE, server),
    ClientEnv = io_lib:format("-client listen_port ~w server \"'~w'\"",
                             [ListenPort, Server]),
    Args = io_lib:format("-setcookie ~s -pa ~s ~s", [Cookie, EbinDir, ClientEnv]),
    {ok, ClientName} = slave:start(HostName, client, Args),
    ok = rpc:call(ClientName, application, start, [client]),
    ClientName.

%% Stops the remote node (which also stops the remote client application).
client_stop(Client) ->
    ok = slave:stop(Client).

%% Start a number of connections at the given client. Client needs to be a node
%% in the form name@host.
connections_start(NoOfConnections, Client) ->
    do_start_connections(NoOfConnections, Client, []).

do_start_connections(0, _Client, Acc) ->
    Acc;
do_start_connections(NoOfConnections, Client, Acc) ->
    {ok, Pid} = supervisor:start_child({client_sup, Client}, []),
    do_start_connections(NoOfConnections-1, Client, [Pid|Acc]).

%% Stops a given number connections by terminating the respective process
%% Process are terminated by pid, from smalles (oldest) to largest
connections_stop(NoOfConnections, Client) ->
    Children = supervisor:which_children({client_sup, Client}),
    ChildPids = lists:map(fun({_, Pid, _, _}) -> Pid end, Children),
    SChildPids = lists:sort(ChildPids),
    {FirstN, _Rest} = lists:split(NoOfConnections, SChildPids),
    lists:foreach(fun(Pid) ->
                          ok = supervisor:terminate_child({client_sup, Client}, Pid)
                  end, FirstN).

