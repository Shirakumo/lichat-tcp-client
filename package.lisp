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
   #:*default-port*))
