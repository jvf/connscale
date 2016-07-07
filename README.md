# Connscale

A little Erlang application to test how well the number of TCP connections scales on a given hardware using `gen_tcp`. This package  consists of two applications, the server (`server` and `server_sup`) and a client (`client`, `client_sup`).

## Server

The server component opens a listen socket and a number of concurrent acceptors, waiting for connection requests. If a clients connects, one of the acceptors takes over the connection, spawns a new acceptor and goes into `{active, once}` mode. Each acceptor/connection is managed by its own process, implemented as a `gen_server`. All the server processes are supervised by `server_sup`. If a server process receives a "Ping" message it  responds with a "Pong" message.

## Client

The client component consists of a supervisor through which we start client connections. Each connection is managed by its own process, implemented as a `gen_server`. Once a connection is started the process sends a "Ping" message every `interval` seconds and waiting for a "Pong" reply in passive mode. If no pong message is received with `timeout` microsecond, the process is terminated with a `timeout` error.


# How to use

Compile with `erl -make`. Start the server with

```
erl -pa ebin -s server -config connscale.config
```

and the client app with

```
erl -pa ebin -s client
```

Start 10 connections from the console of the client with

```
client:connections_start(10)
```

Get the count of connections at the server:
```
server:count_connections().
```

Get the count of connections at the server:
```
client:count_connections().
```

To stop connections, use `client:connections_stop/2`, e.g.
```
client:connections_stop(5).
```

## Configuration

Configuration parameters like the server IP address, the listening port, the client hostnames etc need to be added either to the `connscale.app` file (in `ebin`) or (preferably) through a configuration file, see the	`example.config`.


# Remote Client

An (experimental) remote client (`rclient`) is also available, which uses Erlang's `slave` module and to start a remote node and the client application on that node. This way it is possible to controll a server and multiple clients from one Erlang console.

This setup assumes
* that the whole application (source code, binaries, app files etc) is available at all nodes, e.g. through NFS or rsync.
* passwordless ssh access to remote nodes
* that Erlang is installed on all nodes
* hostnames are configured correctly



## Usage

Compile the sources with `erl -make`. Start the server application with
```
erl -rsh ssh -name master -pa ebin -s server -config connscale.config
```
(assuming a configuration file `connscale.config`). This starts the server application, which starts the supervisor and the configured number of acceptors.
From the console, start a client application on a remote node with
```
Client1 = rclient:start(client1).
```
`client1` needs to a key in the configuration of the `connscale` application (the `env` part in the `.app` file), indicating the hostname of the remote node designated to run the client. This starts a remote node using the Erlang's `slave` module and starts the client application on that node. Multiple client applications can be started if more hosts are available.

To start for example 10 connections from `Client1` use:
```
rclient:connections_start(10, Client1).
```

To stop connections, use `connscale:connections_stop/2`, e.g.
```
rclient:connections_stop(5, Client1).
```


# Findings

## `EADDRINUSE` error

Re-Opening a listen socket with

`{ok, ListenSocket} = gen_tcp:listen(Port, [{active,true}])`

might result in an `{error,eaddrinuse}`, even if the socket was closed properly with

`gen_tcp:close(ListenSocket)`.

The reason is, that the `gen_tcp:close/1` returns before the OS actually closed the socket. The socket might go into `TIME_WAIT`, which is related to not-empty send buffers (cp.	[here](http://stackoverflow.com/a/14388707/2973513)). I'm not 100% clear on how this is connected to listen sockets, but it seems to occur if a there are still connections attempt on the listen socket.

More sources on the topic:
* http://erlang.org/pipermail/erlang-questions/2013-September/075271.html
* http://stackoverflow.com/questions/23786265/erlang-otp-supervisor-gen-tcp-error-eaddrinuse



### How to fix it

Add the option `{reuseaddr, true} ` when opening the listen socket. This only works if the listen socket is bound to the wildcard 0.0.0.0 IP (which is the default for `gen_tcp`).

## Error `closed` when starting connections concurrently

When starting a *lot* of connections concurrently I ran into a weird problem. While most of the connections still get established, some connections fail with a `closed` error in `gen_tcp:receiv/3`. The weird thing is, that these connections did not fail before, the calls to `gen_tcp:connect/3` and `gen_tcp:send/2` were both successful
 (i.e. returned `ok`). On the server side I don't see a matching connection for these "weird" connections, i.e. no returning `gen_tcp:accept/1`.

### Explanation

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

### Connect

Coming back to the behavior observed when starting lots of connections in parallel:  
The explanation is that the accept queue at the server fills up faster than our application consumes the accepted connections. The `gen_tcp:connect/3` calls on the client side return successfully as soon as the client receives the first `SYN-ACK`. The connections do not get reset immediately because the server retries the `SYN-ACK`. The server does not report these connections as successful, because they are still in state `SYN-RECEIVED`.

On BSD derived system (including Mac OS X) the queue for incoming connections works a bit different, see the above mentioned [article](http://veithen.github.io/2014/01/01/how-tcp-backlog-works-in-linux.html).

### Send vs Receive

We now know, why `gen_tcp:connect/3` returns even if the connection is only half established on the server side (on the client side, the connection is fully established). This still leaves the question, why `close` error only occurs in the call to `gen_tcp:recv/3`, not `gen_tcp:send/2`.

To investigate, I performed more experiments:

#### 1. `connect()` -> wait -> `send()` -> `receive()`

This is the setup described above, the starting point of the investigation. On a half-established connection this sequence fails, as described, with a `closed` error in `gen_tcp:recv`. There was never an error during the `gen_tcp"send` on a half-established connection.

To determine if this is due to timing issues or due to some special semantics I introduced a delay of 240 seconds (the delay through `ACK-SYN` retries can be up to 180 seconds with default value of 5 retries) between the `gen_tcp:connect/3` and the calls to `gen_tcp:send/2`. Still, the calls to `gen_tcp:send/2` all succeed, still only the the `gen_tcp:recv` fails.


#### 2. `connect()` -> long wait -> `send()`

To see, if I can get errors while sending data on a half-established connection I waited for 4 minutes before sending data. The 4 minutes should cover all timeouts and retries associated with the half-established connection. Sending data was still possible, i.e. `send()` returned without error.

#### 3. `connect()` -> `receive()`

Next I tested what happens if I only call `receive()` with a very long timeout (5 minutes). My expectation was to get an `closed` error for the half-established connections, as in the original experiments. Alas, nothing happened, no error was thrown and the receive eventually timed out.

#### 4. `gen_tcp:connect`  -> `gen_tcp:send` -> short wait -> `gen_tcp:send`

Motivated by [this answer](http://stackoverflow.com/a/37998348/2973513) I tried this sequence. Here the second `gen_tcp:send` fails with error `closed` on a half-established connection.

#### Explanation

(Based on [this answer](http://stackoverflow.com/a/37998348/2973513) and some knowledge about TCP. The next step would be to confirm the explanation with something like Wireshark.)

The (first) `gen_tcp:send` (in 1, 2 and 4) is successful because from the clients point of view the connection is fully established. The `send()` system call returns when the data is copied into the respective kernel buffers, no `ACK` from the far side is necessary.

Nevertheless, the `gen_tcp:send` does generate the error, as it forces the server to decide over the half-established connection. As long as no data was send the server can still hope to fully establish the connection (through `SYN-ACK` retries), this is no longer possible when the server is asked to acknowledge data on that connection. There the server resets the connection by sending an `RST` to the client. As the `gen_tcp:send` has already returned, the reset of the connection is only noticed by a subsequent call, either to `gen_tcp:recv` (as in 1) or to `gen_tcp:send` (as in 4).

### How to fix it

Increase the size of the accept queue with the `{backlog, N}` option to `gen_tcp:listen/2`. Also of interest might be the `tcp_max_syn_backlog` kernel parameter (defaults to 512 on Ubuntu 14.04), this sets the length of the syn queue (in contrast to the accept queue, see above).

The `tcp_abort_on_overflow` kernel parameter can be used to force the resetting of the connection if the accept queue overflows. I used this to confirm that the problem described above is indeed due to overflows in the connect queue.

The number of `SYN-ACK` retries can be controlled by the `tcp_synack_retries` kernel parameter.

All the kernel parameters can be set through `procfs`.

# Limits

## File Descriptors

The first limit to hit are usually the number of open files per process. Change with `ulimit -n` or through `/etc/security/limits.conf`. A good overview on [setting file descriptors](http://www.cyberciti.biz/faq/linux-increase-the-maximum-number-of-open-files/).

On the client the fd limit manifests when calling `gen_tcp:connect`, which returns `{error, emfile}` if no file descriptor is available.

On the server, if the last available file descriptor is consumed, one acceptor returns with `{error, emfile}` from `gen_tcp:accept`. All the other active acceptors return with with `{error, closed}`.

## Port Numbers

In TCP, four fields are used to match packets to connections (e.g. file descriptors). These four fields are:

```
<Client                 > <Server                           >
<Source-IP> <Source-Port> <Destination-IP> <Destination-Port>
```

In a scenario with a server listening on one port all the accepted connections share this port and the IP of the server. Connections can therefore only be distinguished by the Port and IP of the client, i.e. *per client* the only variable factor is the port number.

The port numbers are divided into three ranges: the well-known ports (0 through 1023), the registered ports (1024 through 49151), and the dynamic or private ports (officially 49152 through 65535). Only the dynamic ports are assigned to `gen_tcp` through the OS.

In Linux, the default range for dynamically assigned ports is (in violation of the standard) 32768 through 61000, which results in 28233 useable ports. This range can be read and set through `/proc/sys/net/ipv4/ip_local_port_range`.

The number of useable ports constitutes a second limit. This limit only applies *per client*.

(Sources: [Stack Overflow](http://stackoverflow.com/a/2332756/2973513))

## Hardware

With the hardware available for testing the final limit was the hardware. Even with very large timeout values the connections timed out eventually (see the section on tests).

# Conclusions for Scalaris

This tool was written to test if the number of connections maintained by Scalaris are or can become a bottleneck for scalability. A second objective was to test how the errors from the OS are propagated through the Erlang VM to `gen_tcp`.



# Experiments

## Test on two 2 GB RAM Digital Ocean instances

### Setup

Digital Ocean instances with 2 GB RAM / 2 CPU's and SSDs for storage with Ubuntu 14.04.4 x64 as OS. `bmon` and `nload` where used to monitor the network traffic.

### One client

After increasing the limit on file descriptors, we begin starting TCP connections. One might need to increase the timeout at the client. We used a value of 5 seconds (i.e. if a client does not receive an answer within 5 seconds an `timeout` error is thrown.)

At 28233 connections we get `eaddrinuse` errors. This corresponds exactly to range of ephemeral ports:

```
server:~# cat /proc/sys/net/ipv4/ip_local_port_range
32768	 61000

```

This means we successfully started and maintained the maximum number of TCP connections possible with one client and only one socket at the server. The range of dynammically assignabale ports can be increased with the `ip_local_port_range` setting, in later experiments started the range at 5000 to get more connections per client.

The `connscale` version used for this test started the connections sequentially, which was really slow (like > 1h for 28 000 connections slow). This led to parallelizing the the setup of connections by moving the call to `gen_tcp:connect` from the initialization of the (connection) `gen_server` process to a cast. This in turn led to the problems of half-established connections discussed above.



### Multiple Clients

With 2-3 clients (one client seemed to be a bit wonky) I managed to establish up to ~ 75000 connections, with ~ 2 MiB/Sec In + 2 MiB/Sec Out and ~ 20-40K pps (in) + 20-40K pps (in). Server machine not yet fully exhausted hardware wise.

```
$ ulimit -n
130000

cat /proc/sys/net/ipv4/ip_local_port_range
5000	 61000
```

### 8 GB / 4 CPUs Digital Ocean

* ~ 95.000 connections with one server, two clients (interval 5 s, timeout 10 s, backlog 256, 128 acceptors)
* ~ 100.000 connections with one server, two clients (interval 5 s, timeout 20 s, backlog 256, 128 acceptors)
* with both timeout settings, there are still occasional timeouts
* CPU / RAM not fully utilized, I suspect the network interface is the bottleneck (pps, not bps) here (1 Gbps Ethernet, shared by multiple Droplets on the host)
* RX: ~ 2.5 MiB/s, ~ 38.000 pps
* TX: ~ 2.8 MiB/s, ~ 45.000 pps

### New

* re-tested with reduced load (i.e. increased interval between the pings from the client connections)
* up to ~ 200000 connections work (interval 20 s, timeout 20 s, backlog 512, 256 acceptors),  (four client hosts)
* confirms, that the hardware

Ubuntu 14.04.4 LTS


```
$ lscpu
Architecture:          x86_64
CPU op-mode(s):        32-bit, 64-bit
Byte Order:            Little Endian
CPU(s):                4
On-line CPU(s) list:   0-3
Thread(s) per core:    1
Core(s) per socket:    1
Socket(s):             4
NUMA node(s):          1
Vendor ID:             GenuineIntel
CPU family:            6
Model:                 63
Stepping:              2
CPU MHz:               1799.998
BogoMIPS:              3599.99
Virtualization:        VT-x
Hypervisor vendor:     KVM
Virtualization type:   full
L1d cache:             32K
L1i cache:             32K
L2 cache:              256K
L3 cache:              30720K
NUMA node0 CPU(s):     0-3
```


# TODO
- [ ] collect connection statistics
