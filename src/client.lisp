(in-package :cl-format-server)

(defun send-request (source-code  &key formatter port)
  "Send a request to the server and return the response."
  (let ((p (or port *default-server-port*)))
    (log-debug "Sending request to port ~A" p)
    (usocket:with-client-socket (socket stream "localhost" p)
      (let ((request (list formatter source-code)))
        ;; (log-debug "Sending request: ~A" request)
        (write request :stream stream)
        ;; (log-debug "Finished writing request")
        (finish-output stream)
        ;; (log-debug "Finished output")
        (usocket:socket-shutdown socket :output)
        (read stream :eof t)
        ))))

(defun simple-req (str port)
  (send-request str :formatter :trivial-formatter :port port))
