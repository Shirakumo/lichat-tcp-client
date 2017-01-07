#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.tcp-client)

(defvar *repl-client* NIL)
(defvar *repl-channel* NIL)

(defclass repl-client (client)
  ())

(defmethod open-connection :after ((client repl-client))
  (start-repl-client-loop client))

(defun status (format &rest args)
  (format T "~&! ~?~%" format args))

(defmethod process ((update lichat-protocol:message) (client repl-client))
  (if (string= (lichat-protocol:channel update) *repl-channel*)
      (format T "~&~a> ~a~%"
              (lichat-protocol:from update)
              (lichat-protocol:text update))))

(defmethod process ((update lichat-protocol:join) (client repl-client))
  (let ((from (lichat-protocol:from update))
        (chan (lichat-protocol:channel update)))
    (cond ((string= from (name client))
           (status "Switched channel to ~a."
                   chan)
           (setf *repl-channel* chan))
          ((string= chan *repl-channel*)
           (status "~a has joined ~a."
                   from chan)))))

(defmethod process ((update lichat-protocol:leave) (client repl-client))
  (let ((from (lichat-protocol:from update))
        (chan (lichat-protocol:channel update)))
    (cond ((string= from (name client))
           (status "Left channel ~a."
                   chan)
           (setf *repl-channel* NIL))
          ((string= chan *repl-channel*)
           (status "~a has left ~a."
                   from chan)))))

(defmethod process ((update lichat-protocol:channels) (client repl-client))
  (status "Found channels: ~a"
          (lichat-protocol:channels update)))

(defmethod process ((update lichat-protocol:users) (client repl-client))
  (status "Users in ~a: ~a"
          (lichat-protocol:channel update)
          (lichat-protocol:users update)))

(defun start-repl-client-loop (client)
  (setf *repl-client* client)
  (let ((*package* #.*package*))
    (format T "~%> ")
    (loop for cmd = (read-line)
          do (with-simple-restart (abort "Continue the repl.")
               (cond ((string= "" cmd))
                     ((string= ":" cmd :end2 1)
                      (eval (read-from-string
                             (format NIL "(~a)" (subseq cmd 1)))))
                     (T
                      (m cmd *repl-channel*))))
             (format T "~&> "))))

(defun j (channel)
  (s *repl-client* 'join :channel channel))

(defun l (&optional (channel *repl-channel*))
  (s *repl-client* 'leave :channel channel))

(defun c (&optional channel)
  (s *repl-client* 'create :channel channel))

(defun m (text &optional (channel *repl-channel*))
  (s *repl-client* 'message :channel channel :text text))

(defun k (user &optional (channel *repl-channel*))
  (s *repl-client* 'kick  :channel channel :target user))

(defun p (user &optional (channel *repl-channel*))
  (s *repl-client* 'pull :channel channel :target user))

(defun lu (&optional (channel *repl-channel*))
  (s *repl-client* 'users :channel channel))

(defun lc ()
  (s *repl-client* 'channels))
