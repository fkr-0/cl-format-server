(in-package :cl-format-server)


(defun help ()
  (format t "~&Usage:

  cl-format-server --serve [port] \n --client-stdin [port]~&"))
(defun start-server-by-type (server-type port)
  "Start server based on the specified type and port."
  (sb-sys:enable-interrupt sb-unix:sigint #'ctrl-c-handler)
  (log:info "Starting ~A server on port/socket: ~A" server-type port)
  (let ((s (if (string= server-type "unix")
             (make-instance 'lisp-unix-socket-server :socket-path port)
             (make-instance 'lisp-network-server :port port))))
    (setf *server-instance* s)
    (start-server s)
    (log:info "Server started: ~A" s)))

(defun parse-port-or-default (args default)
  "Parse port from arguments or return default if none is specified."
  (let ((port-arg (second args)))
    (or (and port-arg (parse-integer port-arg :junk-allowed t)) default)))


(defun ctrl-c-handler (signal code scp)
  (declare (ignore signal code scp))
  (log:info "~%Caught Ctrl-C, stopping...~%")
  (when *server-instance*
    (log:info "Stopping server: ~a" *server-instance*)
    (stop-server *server-instance*))
  (uiop:quit))


(defun %main (argv)
  "Parse CLI args and manage server based on commands."
  (log:info "Starting cl-format-server with args: ~A" argv)
  (cond
    ((member "-h" argv :test #'equal)
      (help)
      (uiop:quit))
    ((member "--sample-request" argv :test #'equal)
      (send-request "( defun dudu (x) (do-some-risky business))" :formatter "trivial-formatter" :port 8080))
    ((member "--client-stdin" argv :test #'equal)
      (simple-req (read-line) (parse-port-or-default argv *default-server-port*)))
    ((member "--unix-sock" argv :test #'equal)
      (start-server-by-type "unix" (parse-port-or-default argv *default-server-sock*)))
    ((member "--serve" argv :test #'equal)
      (start-server-by-type "network" (parse-port-or-default argv *default-server-port*)))
    (t
      (help)
      (log:error "Invalid arguments."))))

;; parse the cli args
;; (defun %main (argv)
;;   "Parse CLI args."
;;   (log:info "Starting cl-format-server with args: ~a" argv)
;;   (when (member "-h" argv :test #'equal)
;;     ;; To properly parse command line arguments, use a third-party library such as
;;     ;; clingon, unix-opts, defmain, adopt… when needed.
;;     (help)
;;     (uiop:quit))
;;   (cond
;;     ((member "cli" argv :test #'equal)
;;       (format t "~A~A~%" #\NewLine (handle (second argv) (third argv))))
;;     ((member "--sample-request" argv :test #'equal)
;;       (send-request "( defun dudu (x) (do-some-risky business))" :formatter "trivial-formatter" :port 8080))
;;     ((member "--client-stdin" argv :test #'equal)
;;       (simple-req (read-line) (parse-integer (second argv) :junk-allowed t) ))
;;     ((or (member "--unix-sock" argv :test #'equal ) (member "-u" argv :test #'equal))
;;       (sb-sys:enable-interrupt sb-unix:sigint #'ctrl-c-handler)
;;       (log:info "Starting server on unix socket: ~a"
;;         (or (and (second argv)(parse-integer
;;                                 (second argv) :junk-allowed t))*default-server-sock*))
;;       (setf *server-instance*
;;         (start-unix-socket-server
;;           (or (and (second argv)
;;                 (parse-integer (second argv)
;;                   :junk-allowed t)) *default-server-sock*) )))
;;     (t;(member "--server" argv :test #'equal)
;;       (sb-sys:enable-interrupt sb-unix:sigint #'ctrl-c-handler)
;;       (log:info "Starting server on port: ~a"
;;         (or (and (second argv)(parse-integer
;;                                 (second argv) :junk-allowed t))*default-server-port*))
;;       (setf *server-instance*
;;         (start-network-server
;;           (or (and (second argv)
;;                 (parse-integer (second argv)
;;                   :junk-allowed t))
;;             *default-server-port*))))))


(defun main ()
  "Entry point for the executable, reads command line arguments."
  (%main (uiop:command-line-arguments))
  (sb-ext:exit :code 0))
