(in-package :cl-format-server)


(defun send-request (source-code  &key formatter linter port)
  "Send a request to the server and return the response."
  (let ((p (or port *default-server-port*)))
    (log-debug "Sending request to port ~A" p)
    (usocket:with-client-socket (socket stream "localhost" p)
      (let ((request (list :source-code source-code :formatter formatter :linter linter)))
        ;; (log-debug "Sending request: ~A" request)
        (write request :stream stream)
        ;; (log-debug "Finished writing request")
        (finish-output stream)
        ;; (log-debug "Finished output")
        (usocket:socket-shutdown socket :output)
        (read stream :eof t)
        ))))
;; (usocket:with-client-socket (socket stream "localhost" port)
;;   (let ((request (list :source-code source-code :formatter formatter :linter linter)))
;;     (log-debug "Sending request: ~A" request)
;;     (write request :stream stream)
;;     (finish-output stream)
;;     (read stream)))

;; (defun format (source-code &key formatter linter port)
;;   "Format the source-code using the formatter and linter."
;;   (let ((response (send-request source-code :formatter formatter :linter linter :port port)))
;;     (log-debug "Received response: ~A" response)
;;     (getf response :formatted-code)))



;; (send-request "(defvar a 2) (defvar b 3)" :formatter :trivial-formatter :linter :cl-lint :port 8080)
;; (send-request "(defvar a 2)" :port 8080)
