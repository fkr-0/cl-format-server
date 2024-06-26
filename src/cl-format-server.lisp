(in-package :cl-format-server)

(defun help ()
  (format t "~&Usage:

  cl-format-server --serve [port] \n --client-stdin [port]~&"))

(defun ctrl-c-handler (signal code scp)
  (declare (ignore signal code scp))
  (log:info "~%Caught Ctrl-C, stopping...~%")
  (when *server-instance*
    (log:info "Stopping server: ~a" *server-instance*)
    (stop-server *server-instance*))
  (uiop:quit))

;; parse the cli args
(defun %main (argv)
  "Parse CLI args."
  (log:info "Starting cl-format-server with args: ~a" argv)
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (cond
    ((member "cli" argv :test #'equal)
      (progn
        (format t "~A~A~%" #\NewLine (handle (second argv) (third argv)))))
    ((member "--sample-request" argv :test #'equal)
      (send-request "( defun dudu (x) (do-some-risky business))" :formatter "trivial-formatter" :port 8080))
    ((member "--client-stdin" argv :test #'equal)
      (simple-req (read-line) (parse-integer (second argv) :junk-allowed t) ))
    ((or (member "--unix-sock" argv :test #'equal ) (member "-u" argv :test #'equal))
      (progn
        (sb-sys:enable-interrupt sb-unix:sigint #'ctrl-c-handler)
        (log:info "Starting server on unix socket: ~a"
          (or (and (second argv)(parse-integer
                                  (second argv) :junk-allowed t))*default-server-sock*))
        (setf *server-instance*
          (start-unix-socket-server
            (or (and (second argv)
                  (parse-integer (second argv)
                    :junk-allowed t)) *default-server-sock*) ))))
    (t;(member "--server" argv :test #'equal)
      (progn
        (sb-sys:enable-interrupt sb-unix:sigint #'ctrl-c-handler)
        (log:info "Starting server on port: ~a"
          (or (and (second argv)(parse-integer
                                  (second argv) :junk-allowed t))*default-server-port*))
        (setf *server-instance*
          (start-network-server
            (or (and (second argv)
                  (parse-integer (second argv)
                    :junk-allowed t))
              *default-server-port*)))))))


(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments))
  ;; Return 0 to indicate success.
  ;; (format t "Exit.~%" )
  (sb-ext:exit :code 0))
