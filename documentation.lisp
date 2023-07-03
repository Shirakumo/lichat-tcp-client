(in-package #:org.shirakumo.lichat.tcp-client)

(docs:define-docs
  (variable *default-port*
    "The default TCP port on which to connect. Should be 1111.")

  (type client
    "Class to represent a Lichat client.

See USERNAME
See PASSWORD
See HOSTNAME
See PORT
See SOCKET
See THREAD")

  (function username
    "Accessor to the username of the client.

See CLIENT
See LICHAT-PROTOCOL:USERNAME")

  (function password
    "Accessor to the password of the client, if any.

See CLIENT
See LICHAT-PROTOCOL:PASSWORD")

  (function hostname
    "Accessor to the TCP hostname the client is connecting to.

See CLIENT")

  (function port
    "Accessor to the TCP port the client is connecting over.

See CLIENT")

  (function socket
    "Accessor to the USOCKET:SOCKET instance that connects to the server.

See CLIENT")

  (function thread
    "Accessor to the background processing thread of the client.

See CLIENT")

  (function socket-stream
    "Returns the stream of the socket of the client, if such is available.

See CLIENT")

  (function read-message
    "Reads wireables from the client's stream until one can be read successfully.

This is to say, it will repeatedly try to read a message,
ignoring all LICHAT-PROTOCOL:WIRE-CONDITIONs until one can
be read without such an error. The message is returned.

See LICHAT-PROTOCOL:WIREABLE
See LICHAT-PROTOCOL:FROM-WIRE
See SOCKET-STREAM")

  (function call-with-response
    "Calls the function once a suitable response update has appeared on the client's stream.

A suitable response is either an UPDATE with the same ID
as the update returned by INIT, or an UPDATE-FAILURE with
the same UPDATE-ID as the ID of the updated returned by
INIT. If another message is found that does not match, it
is simply ignored.

See READ-MESSAGE
See LICHAT-PROTOCOL:UPDATE-FAILURE
See LICHAT-PROTOCOL:UPDATE
See LICHAT-PROTOCOL:ID
See LICHAT-PROTOCOL:UPDATE-ID")

  (function with-response
    "Shorthand macro to await a response from the server that matches the update generated in the INIT form.

See CALL-WITH-RESPONSE")

  (function with-eresponse
    "Shorthand like WITH-RESPONSE that signals an error if the matching update is not of type LICHAT-PROTOCOL:UPDATE or of type LICHAT-PROTOCOL:FAILURE.

See WITH-RESPONSE
See LICHAT-PROTOCOL:FAILURE
See LICHAT-PROTOCOL:UPDATE")

  (function open-connection
    "Initiate the connection from the client to the connection.

Implements the proper connection procedure as mandated by the
protocol in §4.1. If the connection fails, for whatever reason,
an error is signalled. If the connection succeeds, a background
thread is started that calls HANDLE-CONNECTION.

See HANDLE-CONNECTION")

  (function close-connection
    "Closes the connection to the server.

Attempts to be graceful by first sending a 
LICHAT-PROTOCOL:DISCONNECT update to the server, if it can.

See LICHAT-PROTOCOL:DISCONNECT")

  (function send
    "Send an update over the client connection to the server.

See LICHAT-PROTOCOL:TO-WIRE
See SOCKET-STREAM")

  (function s
    "Shorthand to construct an update to the server.

TYPE is coerced to a symbol from the LICHAT-PROTOCOL package.
It is then used together with the initargs to create an update
instance. A :FROM initarg using the client's USERNAME is
automatically added.

See SEND")

  (function handle-connection
    "Handles the incoming updates from the server.

This repeatedly (while the stream is open) calls READ-MESSAGE
and then passes that message on to PROCESS. If a connection
stability error occurs during this (timout, shutdown, reset, etc)
then HANDLE-FATAL-ERROR is called.

A restart called CLOSE-CONNECTION is active during this, which
allows you to gracefully exit and close the connection.

See PROCESS
See HANDLE-FATAL-ERROR")

  (function handle-fatal-error
    "This function is used to handle a fatal, aborting connection error.

You can use this to either simply close and exit, or to try
and reestablish a new connection.

See HANDLE-CONNECTION")

  (function process
    "This method is intended to be extended by user code and is called with any update received from the server.

Default methods exist for the following update types:
 * LICHAT-PROTOCOL:PING       -- Automatically replies to the server
                                 with a LICHAT-PROTOCOL:PONG update.
 * LICHAT-PROTOCOL:DISCONNECT -- Invoke the CLOSE-CONNECTION restart.
 * T                          -- Print the object via Verbose.

See HANDLE-CONNECTION
See LICHAT-PROTOCOL:PING
See LICHAT-PROTOCOL:DISCONNECT"))
