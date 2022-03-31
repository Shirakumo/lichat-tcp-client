#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:lichat-tcp-client
  (:nicknames #:org.shirakumo.lichat.tcp-client)
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
   #:read-message
   #:call-with-response
   #:with-response
   #:open-connection
   #:close-connection
   #:connection-open-p
   #:handle-fatal-error
   #:send
   #:s
   #:handle-connection
   #:process))
