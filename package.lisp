(in-package #:cl-user)
(defpackage #:lichat-tcp-client
  (:nicknames #:org.shirakumo.lichat.tcp-client)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:use #:cl)
  (:export
   #:*default-port*
   #:client
   #:username
   #:password
   #:socket
   #:hostname
   #:port
   #:thread
   #:url
   #:read-message
   #:call-with-response
   #:with-response
   #:open-connection
   #:close-connection
   #:connection-open-p
   #:handle-fatal-error
   #:send
   #:s
   #:s-data
   #:handle-connection
   #:process
   #:mini-client))
