-module(connscale).

-export([start/0]).

% application behaviour
-export([start/2, stop/1]).

%% to use with erl -run/-s
start() ->
    io:format("starting ~w application~n", [?MODULE]),
    application:start(?MODULE).

%% Start the connscale app. Opens a listen port at the port defined in the env
%% and opens Acceptors number of concurrent acceptors.
start(normal, _Args) ->
    {ok, Port} = application:get_env(listen_port),
    {ok, Acceptors} = application:get_env(acceptors),
    {ok, Backlog} = application:get_env(backlog),
    {ok, Cookie} = application:get_env(cookie),
    erlang:set_cookie(node(), Cookie),
    io:format("listening on port ~w with backlog of ~w~n", [Port, Backlog]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {reuseaddr, true}, {backlog, Backlog}]),
    {ok, Sup} = server_sup:start_link(ListenSocket),
    _ = [ supervisor:start_child(Sup, []) || _ <- lists:seq(1,Acceptors)],
    {ok, Sup, ListenSocket}.

%% Stop the connscale app (closes the listen socket)
stop(ListenSocket) ->
    ok = gen_tcp:close(ListenSocket),
    ok.
