-module(serv).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    process_flag(trap_exit, true),

    %% start acceptor
    gen_server:cast(self(), accept),
    {ok, Socket}.

handle_call(get_socket, _From, Socket) ->
    {reply, Socket, Socket};

handle_call(_E, _From, State) ->
    {noreply, State}.

%% this constitutes the acceptor
%% blocks until an connection is established (i.e. might need a brutal_kill)
handle_cast(accept, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket} ->
            {ok, {ServerIp, ServerPort}} = inet:sockname(AcceptSocket),
            {ok, {ClientIp, ClientPort}} = inet:peername(AcceptSocket),
            io:format("serv ~w accepted connection <Server ~w:~w> <Client ~w:~w>~n",
                      [self(), ServerIp, ServerPort, ClientIp, ClientPort]),
            serv_sup:start_acceptor(), % start a new acceptor
            {noreply, AcceptSocket};
        {error, closed} ->
            %% ListenSocket was closed
            {stop, normal, ListenSocket};
        {error, Reason} ->
            io:format("~w gen_tcp:accept caught an error: ~p~n", [self(), Reason]),
            %% serv_sup:start_acceptor(), % start a new acceptor
            {stop, {error, Reason}, ListenSocket}
    end.

%% handle_info({tcp, _Socket, "quit"++_}, Socket) ->
%%     io:format("received quit msg~n", []),
%%     gen_tcp:close(Socket),
%%     {stop, normal, Socket};

handle_info({tcp, _Socket, "Ping"}, Socket) ->
    %% io:format("serv ~w received "Ping"~n", [self()]),
    ok = gen_tcp:send(Socket, "Pong"),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, Socket};

handle_info({tcp, _Socket, Str}, Socket) ->
    io:format("Received message: ~s~n", [Str]),
    {noreply, Socket};

handle_info({tcp_closed, _Socket}, S) ->
    %% io:format("serv ~w: tcp_closed~n", [self()]),
    {stop, {shutdown, tcp_closed}, S};
handle_info({tcp_error, _Socket, Error}, S) ->
    io:format("tcp_error: ~p~n", [Error]),
    {stop, {tcp_error, Error}, S};
handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, Socket) ->
    ok = gen_tcp:close(Socket),
    io:format("serv ~w terminated with reason: ~p~n", [self(), Reason]).


