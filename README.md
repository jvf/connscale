# Connscale

A little Erlang application to test how well the number of TCP connections scales on a given hardware using `gen_tcp`. The app consists of a main (server) component (`connscale`, `serv` and `serv_sup`) and a client component (`client`, `client_sup`).

## Server

The server component opens a listen socket and a number of concurrent acceptors, waiting for connection requests. If a clients connects, one of the acceptors takes over the connection, spawns a new acceptor and goes into `{active, once}` mode. Each acceptor/connection is managed by its own process, implemented as a `gen_server`. All the server processes are supervised by `serv_sup`. Each connection waits for "Ping" messages and responds with a "Pong".

## Client

The client component consists of a supervisor through which we start client connections. Each connection is managed by its own process, implemented as a `gen_server`. Once a connection is started the process sends a "Ping" message every `INTERVAL` seconds and waiting for a "Pong" reply in passive mode. If no pong message is received with 1 second, the process is terminated with a `no_answer_from_server` error.

# How to use

## Prerequisites

* The application assumes, that the whole application (source code, binaries, app files etc) is available at all nodes, e.g. through NFS or rsync.
* passwordless ssh access to remote nodes
* Erlang needs to be installed on all nodes

## Configuration

Configuration parameters like the server IP address, the listening port, the client hostnames etc need to be added either to the `connscale.app` file (in `ebin`) or (preferably) through a configuration file, see the  `example.config`.

## Usage

Compile the sources with `erl -make`. Start the server application with
```
erl -rsh ssh -name master -pa ebin -s connscale -config connscale.config
```
(assuming a configuration file `connscale.config`). This starts the server application, which starts the supervisor and the configured number of acceptors.
From the console, start a client application on a remote node with
```
Client1 = connscale:client_start(client1).
```
`client1` needs to a key in the configuration of the `connscale` application (the `env` part in the `.app` file), indicating the hostname of the remote node designated to run the client. This starts a remote node using the Erlang `slave` module and starts the client application on that node. Multiple client applications can be started if more hosts are available.

We can now start client connections with `connscale:connections_start(10, Client1).` (this starts 10 client connetions).

To stop connetions, use `connscale:connections_stop/2`, e.g. `connections_stop(5, Client1)`.



# Findings

## `EADDRINUSE` error

Re-Opening a listen socket with

`{ok, ListenSocket} = gen_tcp:listen(Port, [{active,true}])`

might result in an `{error,eaddrinuse}`, even if the socket was closed properly with

`gen_tcp:close(ListenSocket)`.

The reason is, that the `gen_tcp:close/1` returns before the OS actually closed the socket. The socket might go into `TIME_WAIT`, which is related to not-empty send buffers (cp.  [here](http://stackoverflow.com/a/14388707/2973513)). I'm not 100% clear on how this is connected to listen sockets, but it seems to occur if a there are still connections attempt on the listen socket. The problem is reproducible with the following sequence:

More sources on the topic:
* http://erlang.org/pipermail/erlang-questions/2013-September/075271.html
* http://stackoverflow.com/questions/23786265/erlang-otp-supervisor-gen-tcp-error-eaddrinuse



### How to fix it

Add the option `{reuseaddr, true} ` when opening the listen socket. This only works if the listen socket is bound to the wildcard 0.0.0.0 IP (which is the default for gen_tcp).

# TODO
- [ ] make `INTERVAL` configurable
- [ ] make client timeout configurable
- [ ] collect connection statistics
