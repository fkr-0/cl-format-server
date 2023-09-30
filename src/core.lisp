(in-package :cl-format-server)
;;;; server

(defparameter *default-server-port* 8080
  "The default port on which the server listens")
(defparameter *default-handler* :trivial-formatter
  "Request handler that is used when no handler is specified in the request")
(defvar *server-instance* nil "The server instance that is currently running.
  This variable is set by the server instance itself when it is started.")

(defclass lisp-server ()
  ((linters :initform nil :accessor linters)
    (port :initform 8080 :accessor port)
    (formatters :initform nil :accessor formatters)
    (server-thread :initform nil :accessor server-thread)
    (server-socket :initform nil :accessor server-socket)
    (running :initform nil :accessor running)))

(defmethod initialize-instance :after ((server lisp-server) &key)
  (setf *server-instance* server))

(defun server-loop (server)
  (let ((server-socket (server-socket server)))
    ;; (start-time (get-internal-real-time))
    (loop while (running server) do
      (let ((client-socket (usocket:socket-accept server-socket)))
        (setf (server-thread server)
          (bordeaux-threads:make-thread
            (lambda ()
              (unwind-protect
                ;; (with-time-measurement
                (with-open-stream
                  (client-stream (usocket:socket-stream client-socket))
                  ;; (log:info (trivial-formatter:read-as-code client-stream))
                  (log:info "Client connected: ~A" client-socket)
                  ;; read multiple requests from the same client
                  ;; (let ((request (read client-stream )))
                  (let ((request (read-request client-stream)))
                    (log:info "Request: ~A" request)
                    (let ((response (handle-request request)))
                      (log:info "Response: ~A" response)
                      (format client-stream "~A~%" response) ;; former "write-response"
                      (finish-output client-stream))));)
                (usocket:socket-close client-socket)))))))))

(defun start-server (p);(&optional (port 8080))
  (let ((server (make-instance 'lisp-server)))
    ;; (log:info "Starting server on port ~A" port)
    (setf (server-socket server) (usocket:socket-listen usocket:*wildcard-host* p))
    (setf (running server) t)
    ;; (setf (server-thread server) (bordeaux-threads:make-thread (lambda () (server-loop server))))
    (unwind-protect
      (server-loop
        server)
      (stop-server server))))

(defmacro with-time-measurement (&body body)
  `(let ((start-time (get-internal-real-time)))
     (unwind-protect
       (progn ,@body)
       (let ((end-time (get-internal-real-time)))
         (log:info "Elapsed time: ~As" (/ (- end-time start-time) 1000000.0))))))

(defun stop-server (server)
  (setf (running server) nil)
  (usocket:socket-close (server-socket server))
  ;; (usocket:socket-shutdown (server-socket server) :IO)
  (bordeaux-threads:join-thread (server-thread server))
  (log:info "Server stopped"))

(defmacro with-temporary-asdf-project ((temp-dir) &body body)
  `(let ((,temp-dir (create-temporary-asdf-project)))
     (unwind-protect
       (progn ,@body)
       (delete-temporary-asdf-project ,temp-dir))))


(defun read-request (stream)
  (log:info "reading request")
  (let (;; (read-delimited-lis t)
         (lines (read-line stream nil nil)))
    (log:info "read line ~A" lines)
    lines))
(defun read-str-request (stream)
  (log:info "reading request")
  (let (;; (read-delimited-lis t)
         (lines (read-line stream nil nil)))
    (log:info "read line ~A" lines)
    lines))

(defun handle-request (request)

  (log:info "handling request ~A" request)
  ;; regex for (:<formatter-name> <source-code>)
  (destructuring-bind (handler source-code)
    (let ((groups (second
                    (multiple-value-list
                      (ppcre:scan-to-strings ":([^ ]+) (.*)\\)"
                        request)))))
      (list (aref groups 0) (aref groups 1)))
    (log:info "handler: ~A" handler)
    (log:info "source-code: ~A" source-code)
    (if (null handler)
      (setf handler *default-handler*)
      (handle (intern (string-upcase handler) 'keyword)
        source-code))))

(defun handle-request-sym (request)
  (log:info "handling request ~A" request)
  (let* ((handler (when (typep (car request) 'symbol)
                    (car request)))
          (source-code (if (null handler)
                         (car request)
                         (cadr request))))
    (if (null handler)
      (setf handler *default-handler*)
      (handle handler source-code))))
