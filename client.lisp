(in-package #:org.shirakumo.lichat.tcp-client)

(defvar *default-port* 1111)

(defclass client ()
  ((username :initarg :username :accessor username)
   (password :initarg :password :accessor password)
   (hostname :initarg :hostname :accessor hostname)
   (port :initarg :port :accessor port)
   (socket :initarg :socket :accessor socket)
   (thread :initarg :thread :accessor thread))
  (:default-initargs
   :username (machine-instance)
   :password NIL
   :hostname "localhost"
   :port *default-port*
   :socket NIL
   :thread NIL))

(defmethod url ((client client))
  (format NIL "lichat://~a@~a~@[:~a~]/"
          (username client) (hostname client)
          (unless (= *default-port* (port client)) (port client))))

(defmethod socket-stream ((client client))
  (let ((socket (socket client)))
    (when socket
      (usocket:socket-stream socket))))

(defmethod read-message ((client client))
  (loop for message = (handler-case (lichat-protocol:from-wire* (socket-stream client))
                        (lichat-protocol:wire-condition (err)
                          (v:warn :lichat.client "~a: error on wire: ~a" client err)))
        do (when message (return message))))

(defmethod call-with-response (function client init)
  (let ((send-event (funcall init)))
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
                         client message)))))

(defmacro with-response ((message client) init &body body)
  `(call-with-response (lambda (,message) ,@body) ,client (lambda () ,init)))

(defmacro with-eresponse ((message client) init &body body)
  `(with-response (,message ,client) ,init
     (etypecase ,message
       (lichat-protocol:failure
        (error "Failed: ~a" (lichat-protocol:text ,message)))
       (lichat-protocol:update
        ,@body))))

(defmethod open-connection ((client client))
  (setf (socket client) (usocket:socket-connect (hostname client) (port client)))
  (with-response (message client)
                 (s client 'connect
                    :version "2.0"
                    :password (unless (equal "" (password client)) (password client))
                    :extensions '("shirakumo-backfill" "shirakumo-data"))
    (cond ((typep message 'lichat-protocol:connect)
           (process message client))
          (T
           (close-connection client)
           (error "Connection failed: ~a" message))))
  (unless (thread client)
    (setf (thread client) (bt:make-thread (lambda () (unwind-protect
                                                          (handle-connection client)
                                                       (setf (thread client) NIL)))
                                          :initial-bindings `((*standard-output* . ,*standard-output*)))))
  client)

(defmethod close-connection ((client client))
  (v:info :lichat.client "~a: Closing." client)
  (ignore-errors (s client 'disconnect))
  (ignore-errors (usocket:socket-close (socket client)))
  (setf (socket client) NIL)
  (unless (eql :local (thread client))
    (setf (thread client) NIL))
  client)

(defmethod connection-open-p ((client client))
  (let ((stream (socket-stream client)))
    (and stream (open-stream-p stream))))

(defmethod send ((object lichat-protocol:object) (client client))
  (lichat-protocol:to-wire object (usocket:socket-stream (socket client)))
  object)

(defun s (client type &rest args)
  (send (apply #'make-instance (find-symbol (string type) :lichat-protocol)
               :from (username client)
               :id (lichat-protocol:next-id)
               :clock (get-universal-time)
               args)
        client))

(defun s-data (client channel file)
  (let ((data (with-open-file (stream file :element-type '(unsigned-byte 8))
                (let ((arr (make-array (file-length stream) :element-type '(unsigned-byte 8))))
                  (read-sequence arr stream)
                  arr))))
    (s client 'data
       :channel channel
       :content-type (trivial-mimes:mime file)
       :filename (file-namestring file)
       :payload (cl-base64:usb8-array-to-base64-string data))))

(define-compiler-macro s (&whole whole &environment env client type &rest args)
  (if (constantp type env)
      (let ((clientg (gensym "CLIENT")))
        `(let ((,clientg ,client))
           (send (make-instance (load-time-value (find-symbol (string ,type) :lichat-protocol))
                                :from (username ,clientg)
                                :id (lichat-protocol:next-id)
                                :clock (get-universal-time)
                                ,@args)
                 ,clientg)))
      whole))

(defmethod handle-connection ((client client))
  (let ((stream (usocket:socket-stream (socket client))))
    (restart-case
        (handler-case
            (loop while (open-stream-p stream)
                  do (v:trace :lichat.client "~a: Waiting for message..." client)
                     (with-simple-restart (continue "Give up processing the update.")
                       (process (read-message client) client)))
          ((or usocket:ns-try-again-condition
            usocket:timeout-error
            usocket:shutdown-error
            usocket:connection-reset-error
            usocket:connection-aborted-error
            cl:end-of-file
            cl:stream-error) (err)
            (v:warn :lichat.client "~a: Encountered fatal error: ~a" client err)
            (handle-fatal-error client)))
      (close-connection ()
        (close-connection client)))))

(defmethod handle-fatal-error ((client client))
  (invoke-restart 'close-connection))

(defmethod process (object (client client))
  (v:info :lichat.client "~a: received ~a" client object))

(defmethod process ((update lichat-protocol:ping) (client client))
  (s client 'pong))

(defmethod process ((update lichat-protocol:connect) (client client))
  (setf (username client) (lichat-protocol:from update)))

(defmethod process ((update lichat-protocol:disconnect) (client client))
  (v:info :lichat.client "~a: received disconnect, exiting." client)
  (invoke-restart 'close-connection))
