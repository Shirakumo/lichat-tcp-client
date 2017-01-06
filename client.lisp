#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat)

(defclass client ()
  ((name :initarg :name :accessor name)
   (password :initarg :password :accessor password)
   (socket :initarg :socket :accessor socket)
   (hostname :initarg :hostname :accessor hostname)
   (port :initarg :port :accessor port))
  (:default-initargs
   :name (machine-instance)
   :password NIL
   :hostname "localhost"
   :port *default-port*))

(defmethod socket-stream ((client client))
  (usocket:socket-stream (socket client)))

(defmethod read-message ((client client))
  (loop for message = (handler-case (lichat-protocol:from-wire (socket-stream client))
                        (lichat-protocol:wire-condition (err)
                          (v:info :lichat.client "~a: error on wire: ~a" client err)))
        do (when message (return message))))

(defmethod call-with-response (function client send-event)
  (send send-event client)
  (loop for message = (read-message client)
        do (if (typecase message
                 (lichat-protocol:update-failure
                  (= (lichat-protocol:update-id message)
                     (lichat-protocol:id send-event)))
                 (lichat-protocol:update
                  (= (lichat-protocol:id message)
                     (lichat-protocol:id send-event))))
               (return (funcall function message))
               (v:warn :lichat.client "~a: got message while waiting for response: ~a"
                       client message))))

(defmacro with-response ((message client) send-event &body body)
  `(call-with-response (lambda (,message) ,@body) ,client ,send-event))

(defmethod open-connection ((client client))
  (setf (socket client) (usocket:socket-connect (hostname client) (port client)))
  (with-response (message client)
                 (make-instance 'lichat-protocol:connect :from (name client)
                                                         :password (password client))
    (unless (typep message 'lichat-protocol:connect)
      (close-connection client)
      (error "Connection failed: ~a" message)))
  client)

(defmethod close-connection ((client client))
  (v:info :lichat.client "~a: Closing." client)
  (ignore-errors (usocket:socket-close (socket client)))
  (setf (socket client) NIL)
  client)

(defmethod send ((object lichat-protocol:wire-object) (client client))
  (lichat-protocol:to-wire object (usocket:socket-stream (socket client)))
  object)
