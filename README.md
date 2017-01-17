## About Lichat-TCP-Server
This is a simple, threaded, TCP-based client for the [Lichat protocol](https://shirakumo.org/projects/lichat-protocol).

## How To
Create a new client instance and pass it whatever settings you would like.

```
(defvar *client* (make-instance 'lichat-tcp-client:client))
```

Notable initargs of potential interest are:

* `:username` The name the client goes by on the network. Defaults to `(machine-instance)`.
* `:password` The password of the user profile, if any. Defaults to `NIL`.
* `:hostname` The hostname to which the TCP socket should connect. The default is `localhost`.
* `:port` The port the TCP socket should connect to. The default is `1111`.

Once a client exists, it can be started to attempt a connection to the server.

```
(lichat-tcp-client:open-connection *client*)
```

The client logs information via [Verbose](http://shinmera.github.io/verbose/). If you set the REPL level to `:trace` you should see a bunch of status messages being printed every now and again.

Once you're done with the client, you can shut it down again.

```
(lichat-tcp-client:close-connection *client*)
```

Naturally this client doesn't really do much on its own. You can send updates to the server with `s`:

```
(lichat-tcp-client:s *client* 'create :channel "test")
(lichat-tcp-client:s *client* 'message :channel "test" :text "Hey.")
```

If you would like to respond to updates that the server sends back, you can define a method on `process` to do so.

```
(defmethod lichat-tcp-client:process ((update lichat-protocol:join) (client lichat-tcp-client:client))
  (lichat-tcp-client:s client 'message
                       :channel (lichat-protocol:channel update)
                       :text "What's up everyone?"))
```

See the [Lichat protocol](https://shirakumo.org/projects/lichat-protocol) for more information on the available updates, their arguments, and their behaviour.

## Also See

* [lichat-protocol](https://shirakumo.github.io/lichat-protocol) The Lichat protocol specification.
* [lichat-serverlib](https://shirakumo.github.io/lichat-serverlib) An agnostic implementation of the server-side protocol.
* [lichat-tcp-server](https://shirakumo.github.io/lichat-tcp-server) A basic, threaded, TCP-based implementation of a Lichat server.
* [LionChat](https://github.com/Shirakumo/lionchat) A Qt GUI client for a TCP server.
