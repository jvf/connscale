-module(rclient).

% Client API
-export([start/1, stop/1, connections_start/2, connections_stop/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Remote Client Interface %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: fix different escapings depending on the platform

%% Starts a remote node and client application at the client host defined as
%% ClientId in the env. ClientId must be a key in the env of the application
%% indicating the hostname of machine to host the client.
start(ClientId) ->
    {ok, HostName} = application:get_env(connscale, ClientId),
    {ok, Cookie} = application:get_env(connscale, cookie),
    {ok, EbinDir} = application:get_env(connscale, ebin_dir),
    {ok, ListenPort} = application:get_env(connscale, listen_port),
    {ok, Server} = application:get_env(connscale, server),
    %% ClientEnv = io_lib:format("-client listen_port ~w server \"'~w'\"", % this escaping works on Linux
    %%                          [ListenPort, Server]),
    ClientEnv = io_lib:format("-client listen_port ~w server '~w'", % this escaping works on Mac OS X
                             [ListenPort, Server]),
    Args = io_lib:format("-setcookie ~s -pa ~s ~s", [Cookie, EbinDir, ClientEnv]),
    {ok, ClientName} = slave:start(HostName, client, Args),
    ok = rpc:call(ClientName, application, start, [client]),
    ClientName.

%% Stops the remote node (which also stops the remote client application).
stop(Client) ->
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

