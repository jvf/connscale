-module(client).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Client application
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-export([start/2, stop/1]).

start(normal, _Args) ->
    {ok, Sup} = client_sup:start_link(),
    {ok, Sup, []}.

stop(_State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Client Connections (gen_server)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init(_Args) ->
    process_flag(trap_exit, true),

    % use crypto to seed random
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),

    {ok, Server} = application:get_env(client, server),
    {ok, Port} = application:get_env(client, listen_port),
    {ok, Interval} = application:get_env(client, interval),
    {ok, Socket} = gen_tcp:connect(Server, Port, [{active, false}]),

    % initialize periodic trigger
    erlang:send_after(Interval, self(), trigger),

    {ok, Socket}.

handle_call(get_socket, _From, Socket) ->
    {reply, Socket, Socket}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, _OtherSocket, _Msg}, Socket) ->
    io:format("client ~w received msg: ~s~n", [self(), _Msg]),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, Socket};

% receiving in passive mode with timeout
handle_info(trigger, Socket) ->
    %% io:format("~w was triggered~n", [self()]),
    {ok, Timeout} = application:get_env(client, timeout),
    case gen_tcp:send(Socket, "Ping") of
        ok ->
            case gen_tcp:recv(Socket, 0, Timeout) of
                {ok, "Pong"} ->
                    {ok, Interval} = application:get_env(client, interval),
                    erlang:send_after(Interval, self(), trigger),
                    {noreply, Socket};
                {error, timeout} ->
                    % timeout from gen_tcp:recv/3
                    {stop, no_answer_from_server, Socket};
                {error, Reason} ->
                    % other reasons, mainly tcp_closed
                    {stop, Reason, Socket}
            end;
        {error, Reason} ->
            io:format("~w terminating in gen_tcp:send/2 due to ~w~n", [self(), Reason]),
            {stop, normal, Socket}
    end;

handle_info(Info, State) ->
    io:format("unexpected: ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, Socket) ->
    io:format("client conn ~w terminated due to reason: ~p~n", [self(), Reason]),
    ok = gen_tcp:close(Socket).

