# Chat Server with Cloud Haskell

An implementation of a Chat Server with [Cloud Haskell](http://haskell-distributed.github.io/)


# Installation

Clone this repository and run `stack install` in its root folder.


# Run server

To run the server, you must provide the following arguments:

 - `host`: Address of the host machine for the chat server.
 - `port`: Port where the server is to be served.
 - `room`: Name for this chat room.

For example:

```
chatServer --host 192.168.0.33 --port 8080 --room myChat
```

As soon as the server is launched it will prompt its `EndPointAddress`, which is needed for clients to establish a connection.
The prompt log will look like the following:

```
Server launched at: 192.168.0.33:8080:0
```

# Run client

To run the client, you must provide the following arguments:

- `address`: [EndPointAddress](http://haskell-distributed.github.io/tutorials/tutorial-NT2.html) of the remote (or local) machine serving the chat room.
- `host`: Address of the host machine for the chat client.
- `port`: Port where the client is to be served.
- `room`: Name of the chat room this client will connect to.


For example:

```
chatClient --address 192.168.0.33:8080:0 --host 127.0.0.1 --port 8880 --room myChat
```

Note that the `EndPointAddress` you must provide is the value provided by the chat server once it has been launched.