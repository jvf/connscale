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

## Test on two 2 GB RAM Digital Ocean instances

### Setup

Two Digital Ocean instances with 2 GB RAM / 2 CPU's and SSDs for storage with Ubuntu 14.04.4 x64 as OS.

### Tests

After increasing the limit on file descriptors, we beginn starting tcp connections. One might need to increase the timeout at the client. We used a value of 5 seconds (i.e. if a client does not receive an answer within 5 seconds an `no_answer_from_server` error is thrown.)

Starting large numbers of connections gets pretty slow (like > 1h for 28 000 connections slow). This raises the question what the bottleneck of the setup is. CPU and RAM are always candidates, but so far ()~24 000 connections) this does not seem to be the problem. Network traffic is around 3.2 MBit/sec (In) / 5.2 Mbit/sec (Out) on the client (around 24 000). On the server, it is around 5.3 MBit/sec (In) / 3.5 Mbit/sec out. PPS on the server is around 10 K (In) / 6 K (Out), on the client it is the outher way around. My suspicion is, that the PPS is the bottleneck here.

At 28233 connections we get `eaddrinuse` errors. This corresponds exactly to range of ephemeral ports:

```
server:~# cat  /proc/sys/net/ipv4/ip_local_port_range
32768   61000

```

This means we successfully started and maintained the maximum number of TCP connections possible (using only one port at the server).

The full error message from the client:
```
=ERROR REPORT==== 1-Jun-2016::11:05:44 ===
Error in process <0.20073.0> with exit value:
{{badmatch,{error,{{badmatch,{error,eaddrinuse}},
                   [{client,init,1,[{file,"src/client.erl"},{line,41}]},
                    {gen_server,init_it,6,
                                [{file,"gen_server.erl"},{line,328}]},
                    {proc_lib,init_p_do_apply,3,
                              [{file,"proc_lib.erl"},{line,240}]}]}}},
 [{client_sup,do_start_clients,2,[{file,"src/client_sup.erl"},{line,30}]}]}
 ```



 TODO
 - [ ] try to increase interval, as the network is already pretty heavy utilized at 28 000 connections (without starting new connections)

# TODO
- [x] make `INTERVAL` configurable
- [x] make client timeout configurable
- [ ] collect connection statistics
