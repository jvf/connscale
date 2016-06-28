# Connscale

A little Erlang application to test how well the number of TCP connections scales on a given hardware using `gen_tcp`. The app consists of a main (server) component (`connscale`, `server` and `server_sup`) and a client component (`client`, `client_sup`).

## Server

The server component opens a listen socket and a number of concurrent acceptors, waiting for connection requests. If a clients connects, one of the acceptors takes over the connection, spawns a new acceptor and goes into `{active, once}` mode. Each acceptor/connection is managed by its own process, implemented as a `gen_server`. All the server processes are supervised by `server_sup`. Each connection waits for "Ping" messages and responds with a "Pong".

## Client

The client component consists of a supervisor through which we start client connections. Each connection is managed by its own process, implemented as a `gen_server`. Once a connection is started the process sends a "Ping" message every `INTERVAL` seconds and waiting for a "Pong" reply in passive mode. If no pong message is received with 1 second, the process is terminated with a `no_answer_from_server` error.

# How to use

## Prerequisites

* The application assumes, that the whole application (source code, binaries, app files etc) is available at all nodes, e.g. through NFS or rsync.
* passwordless ssh access to remote nodes
* Erlang needs to be installed on all nodes

## Configuration

Configuration parameters like the server IP address, the listening port, the client hostnames etc need to be added either to the `connscale.app` file (in `ebin`) or (preferably) through a configuration file, see the	`example.config`.

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

The reason is, that the `gen_tcp:close/1` returns before the OS actually closed the socket. The socket might go into `TIME_WAIT`, which is related to not-empty send buffers (cp.	[here](http://stackoverflow.com/a/14388707/2973513)). I'm not 100% clear on how this is connected to listen sockets, but it seems to occur if a there are still connections attempt on the listen socket. The problem is reproducible with the following sequence:

More sources on the topic:
* http://erlang.org/pipermail/erlang-questions/2013-September/075271.html
* http://stackoverflow.com/questions/23786265/erlang-otp-supervisor-gen-tcp-error-eaddrinuse



### How to fix it

Add the option `{reuseaddr, true} ` when opening the listen socket. This only works if the listen socket is bound to the wildcard 0.0.0.0 IP (which is the default for gen_tcp).

# Error `CLOSED` when starting connections concurrently

The scenario is a server listening to a port with multiple concurrent acceptors. From a client I establish a connection by calling `gen_tcp:connect/3`, afterwards I send a "Ping" message to the server and wait in passive mode for a "Pong" response. Normally this all works fine. The problem occurs when trying to establish a lot of connections in parallel. While most of the connections still get established, some connections fail with a `closed` error in `gen_tcp:receiv/3`. The weird thing is, that these connections did not fail before, the calls to `gen_tcp:connect/3` and `gen_tcp:send/2` were both successful
 (i.e. returned `ok`). On the server side I don't see a matching connection for these "weird" connections, i.e. no returning `gen_tcp:accept/1`.

## Explanation

```
TCP 3-way handshake
        Client         Server

  connect()│──┐          │listen()
           │  └──┐       │
           │      SYN    │
           │        └──┐ │
           │           └▶│   STATE
           │          ┌──│SYN-RECEIVED
           │       ┌──┘  │
           │   SYN-ACK   │
           │ ┌──┘        │
   STATE   │◀┘           │
ESTABLISHED│──┐          │
           │  └──┐       │
           │     └ACK    │
           │        └──┐ │   STATE
           │           └▶│ESTABLISHED
           ▽             ▽
```

The problem lies with the finer details of the 3-way handshake for establishing a TCP connection and the queue for incoming connections at the listen socket. See this [excellent article](http://veithen.github.io/2014/01/01/how-tcp-backlog-works-in-linux.html) for details, much of the following explanation was informed by this article.

In Linux there are actually two queues for incoming connections. When the server receives a connection request (`SYN` packet) and transitions to the state `SYN-RECEIVED`, this connection is placed in the `SYN` queue. If a corresponding `ACK` is received, the connections is placed in the accept queue for the application to consume. The `{backlog, N}` (default: 5) option to `gen_tcp:listen/2`  determines the length of the access queue.

When the server receives an `ACK` while the accept queue is full the `ACK` is basically ignored and no `RST` is sent to the client. There is a timeout associated with the `SYN-RECEIVED` state: if no `ACK` is received (or ignored, as is the case here), the server will resend the `SYN-ACK`. The client then resends the `ACK`. If the application consumes an entry from accept queue before the maximum number of `SYN-ACK` retries has been reached, the server will eventually process one of the duplicate `ACKs` and transition to state `ESTABLISHED`. If the maximum number of retries has been reached the server will send a `RST` to the the client to reset the connection.

Coming back to the behavior observed when starting lots of connections in parallel. The explanation is, that the accept queue at the server fills up faster than our application consumes the accepted connections. The `gen_tcp:connect/3` calls on the client side return successfully as soon as the receive the first `SYN-ACK`. The connections do not get reset immediately because the server retries the `SYN-ACK`. The server does not report these connections as successful, because they are still in state `SYN-RECEIVED`.

On BSD derived system (including Mac OS X) the queue for incoming connections works a bit different, see the above mentioned [article](http://veithen.github.io/2014/01/01/how-tcp-backlog-works-in-linux.html).

- [ ] research why sending the "Ping" message succeeds at the client and only the call to `gen_tcp:recv/3` fails
- [ ] test if connections get reset eventually if no send is attempted


## How to fix it

Increase the size of the accept queue with the `{backlog, N}` option to `gen_tcp:listen/2`.


## Test on two 2 GB RAM Digital Ocean instances

### Setup

Two Digital Ocean instances with 2 GB RAM / 2 CPU's and SSDs for storage with Ubuntu 14.04.4 x64 as OS.

### Tests

After increasing the limit on file descriptors, we beginn starting tcp connections. One might need to increase the timeout at the client. We used a value of 5 seconds (i.e. if a client does not receive an answer within 5 seconds an `no_answer_from_server` error is thrown.)

Starting large numbers of connections gets pretty slow (like > 1h for 28 000 connections slow). This raises the question what the bottleneck of the setup is. CPU and RAM are always candidates, but so far ()~24 000 connections) this does not seem to be the problem. Network traffic is around 3.2 MBit/sec (In) / 5.2 Mbit/sec (Out) on the client (around 24 000). On the server, it is around 5.3 MBit/sec (In) / 3.5 Mbit/sec out. PPS on the server is around 10 K (In) / 6 K (Out), on the client it is the outher way around. My suspicion is, that the PPS is the bottleneck here.

At 28233 connections we get `eaddrinuse` errors. This corresponds exactly to range of ephemeral ports:

```
server:~# cat	/proc/sys/net/ipv4/ip_local_port_range
32768	 61000

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
