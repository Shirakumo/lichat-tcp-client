(in-package #:org.shirakumo.lichat.tcp-client)

(defclass mini-client (client)
  ((current-channel :initform NIL :accessor current-channel)))

(defun format-mini-line (update format &rest args)
  (multiple-value-bind (s m h) (decode-universal-time (lichat-protocol:clock update))
    (declare (ignore s))
    (format T "~&~2,'0d:~2,'0d <~12a> ~?~%" h m (lichat-protocol:from update) format args)))

(defmethod process (update (client mini-client))
  (format-mini-line update "** Sent ~s" (type-of update)))

(defmethod process ((update lichat-protocol:channel-update) (client mini-client))
  (when (string-equal (current-channel client) (lichat-protocol:channel update))
    (format-mini-line update "** Sent ~s" (type-of update))))

(defmethod process ((update lichat-protocol:text-update) (client mini-client))
  (format-mini-line update "~a" (lichat-protocol:text update)))

(defmethod process ((update lichat-protocol:message) (client mini-client))
  (when (string-equal (current-channel client) (lichat-protocol:channel update))
    (format-mini-line update "~a" (lichat-protocol:text update))))

(defmethod process ((update lichat-protocol:join) (client mini-client))
  (cond ((string-equal (lichat-protocol:from update) (username client))
         (setf (current-channel client) (lichat-protocol:channel update))
         (format T "~& Switched to ~a~%" (current-channel client)))
        (T
         (when (string-equal (current-channel client) (lichat-protocol:channel update))
           (format-mini-line update "** Joined ~a" (lichat-protocol:channel update))))))

(defmethod handle-command ((client mini-client) cmd args)
  (format T "~& Error: No such command ~a~%" cmd))

(defmethod handle-command ((client mini-client) (cmd (eql :quit)) args)
  (close-connection client))

(defmethod handle-command ((client mini-client) (cmd (eql :join)) args)
  (s client 'join :channel args))

(defmethod handle-command ((client mini-client) (cmd (eql :leave)) args)
  (s client 'leave :channel (if (string= args "") (current-channel client) args)))

(defmethod handle-input ((client mini-client) (line string))
  (when (< 0 (length line))
    (if (char= #\/ (char line 0))
        (let* ((end (or (position #\Space line) (length line))))
          (handle-command client (intern (string-upcase (subseq line 1 end)) "KEYWORD")
                          (subseq line (min (length line) (1+ end)))))
        (s client 'message :channel (current-channel client)
                           :text line))))

(defun mini-client (&key username
                         password
                         (hostname "chat.tymoon.eu")
                         (port *default-port*))
  (let ((client (make-instance 'mini-client
                               :username username
                               :password password
                               :hostname hostname
                               :port port)))
    (unwind-protect
         (progn (open-connection client)
                (loop while (connection-open-p client)
                      for line = (read-line)
                      do (handle-input client line)))
      (close-connection client))))
