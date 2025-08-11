(asdf:defsystem lichat-tcp-client
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A simple TCP client implementation for lichat"
  :homepage "https://shirakumo.org/docs/lichat-tcp-client/"
  :bug-tracker "https://shirakumo.org/project/lichat-tcp-client/issues"
  :source-control (:git "https://shirakumo.org/project/lichat-tcp-client.git")
  :serial T
  :components ((:file "package")
               (:file "client")
               (:file "mini")
               (:file "documentation"))
  :depends-on (:lichat-protocol
               :usocket
               :bordeaux-threads
               :documentation-utils
               :verbose
               :cl-base64
               :trivial-mimes))
