(in-package :cl-format-server)

;;;; Server Configuration and Global Variables

;; Defines the default port for the server
(defparameter *default-server-port* 8080
  "The default port on which the server listens.")
(defvar *default-server-fock* "/tmp/cl-format-server.sock"
  "The default port on which the server listens.")
(defparameter *default-server-sock* "/tmp/cl-format-server.sock"
  "The default port on which the server listens.")

;; Specifies the default request handler
(defparameter *default-handler* :trivial-formatter
  "Request handler used when no specific handler is specified in the request.")

;; Holds the instance of the currently running server
(defvar *server-instance* nil
  "The server instance that is currently running. Set when the server starts.")


;;;; Server Class Definition

;; Defines the lisp-server class with essential properties
(defclass lisp-server ()
  ((linters :initform nil :accessor linters)
    (formatters :initform nil :accessor formatters)
    (server-thread :initform nil :accessor server-thread)
    (server-socket :initform nil :accessor server-socket)
    (running :initform nil :accessor running))
  (:documentation "Class representing the Lisp server with configuration and state.

  Attributes:
    linters: A list of linters to use for linting requests.
    port: The port on which the server listens.
    formatters: A list of formatters to use for formatting requests.
    server-thread: The thread on which the server is running.
    server-socket: The socket on which the server is listening.
    running: Whether the server is currently running."))

(defclass lisp-network-server (lisp-server)
  ((port :initform 8080 :accessor port-of))
  (:documentation "Class representing the Lisp server with configuration and state."))

(defclass lisp-unix-socket-server (lisp-server)
  ((socket-path :initform nil :accessor socket-path-of))(:documentation "Class representing the Lisp server with configuration and state."))

;;;; Server Initialization



;; Initializes the server instance after creation
(defmethod initialize-instance :after ((server lisp-server) &key)
  "Initializes the server instance after creation.

  Args:
    server: The server instance to initialize."
  (setf *server-instance* server))

;;;; Server Loop and Request Handling

;; Main loop for handling incoming requests
(defmethod server-loop ((server lisp-network-server) )
  "Main server loop. Continuously accepts and handles client requests.

  Args:
    server: The server instance to run.
  Returns:
    None.
  Example:
    (server-loop server)"
  (let ((server-socket (server-socket server)))
    (loop while (running server) do
      (let ((client-socket (usocket:socket-accept server-socket)))
        (setf (server-thread server)
          (bordeaux-threads:make-thread
            (lambda ()
              (unwind-protect
                (handle-client  server client-socket)
                (usocket:socket-close client-socket)))))))))

(defmethod server-loop ((server lisp-unix-socket-server) )
  "Main server loop. Continuously accepts and handles client requests.

  Args:
    server: The server instance to run.
  Returns:
    None.
  Example:
    (server-loop server)"
  (let ((server-socket (server-socket server)))
    (loop while (running server) do
      (let ((client-socket (unix-sockets:accept-unix-socket server-socket)))
        (setf (server-thread server)
          (bordeaux-threads:make-thread
            (lambda ()
              (unwind-protect
                (handle-client server client-socket)
                (unix-sockets:close-unix-socket client-socket)))))))))


(defmethod handle-client ((server lisp-network-server) client-socket )
  "Handles a client connection, reading and responding to requests.

  Args:
    client-socket: The client socket to handle.
    server: The server instance.
  Returns:
    None.
  Example:
    (handle-client client-socket server)"
  (declare (ignore server))
  (with-open-stream (client-stream (usocket:socket-stream client-socket))
    (log:info "Client connected: ~A" client-socket)
    (loop for request = (read-request client-stream)
      while request do
      (process-request  client-stream request))))

(defmethod handle-client ((server lisp-unix-socket-server) client-socket )
  "Handles a client connection, reading and responding to requests.

  Args:
    client-socket: The client socket to handle.
    server: The server instance.
  Returns:
    None.
  Example:
    (handle-client client-socket server)"
  (declare (ignore server))
  (with-open-stream (client-stream (unix-sockets:unix-socket-stream client-socket))
    (log:info "Client connected: ~A" client-socket)
    (loop for request = (read-request client-stream)
      while request do
      (process-request client-stream request ))))

;; (handle-request ":trivial-formatter (format t \"Hello, world!\")")
;; (stop-server *server-instance*)

(defun handle-request (request)
  "Handles an incoming request by parsing and executing it.
  Args:
    request: The incoming request stringkku.
  Returns:
    The response string after processing the request.
  "
  (log:info "Handling request ~A" request)
  (handler-case
    (let* ((parsed-request (parse-request request))
            (handler (car parsed-request))
            (source-code (cadr parsed-request)))
      (when (null handler)
        (setf handler *default-handler*))
      (handle (or (find-symbol (string-upcase handler) :keyword) *default-handler*)
        source-code))
    (error (e)
      (log:error "Error handling request ~A: ~A" request e)
      "Error processing request")))

(defun parse-request (request)
  "Parses the incoming request string.
  Args:
    request: The incoming request string.
  Returns:
    A list containing the handler and source code from the request, if any.
  Example:
    (parse-request \":trivial-formatter (format t \"Hello, world!\")\")
    => (:trivial-formatter \"(format t \"Hello, world!\")\")"
  (let ((groups (second
                  (multiple-value-list
                    ;; regex for (:<formatter-name> <source-code>)
                    (ppcre:scan-to-strings ":([^ ]+) (.*)\\)"
                      request)))))
    (list (aref groups 0) (aref groups 1))))

;; Reads a request from the client stream
(defun read-request (stream)
  "Reads a single request from the client stream.
  Args:
    stream: The client stream from which to read the request.
  Returns:
    The request string.
  Example:
    (read-request client-stream)
    => \":trivial-formatter (format t \"Hello, world!\")\""
  (log:info "Reading request")
  (read-line stream nil nil))

;; Processes a single request and sends a response
(defmethod process-request ((client-stream usocket:stream-usocket) request )
  "Processes a single request and sends the corresponding response."
  (let ((response (handle-request request )))
    (log:info "UsockResponse: ~A" response)
    (format client-stream "~A~%" response)
    (finish-output client-stream)))

(defmethod process-request ((client-stream flexi-streams:flexi-io-stream) request )
  (let ((response (handle-request request )))
    (log:info "FlexiResponse: ~A" response)
    (dotimes (x (length response))
      (write-byte (char-int (char response x)) client-stream))
    (finish-output client-stream)))
;; (stop-server *server-instance*)
;;;; Server Management Functions

;; Starts the server on the specified port
(defun start-network-server (&optional (port *default-server-port*))
  "Starts the server on the specified port.

  Args:
    port: The port on which to start the server.
  Returns:
    None.
  Example:
    (start-server 8080)"
  (let ((server (make-instance 'lisp-network-server)))
    ;; (log:info "Starting server on port ~A" port)
    (setf (port-of server) port)
    (setf (server-socket server) (usocket:socket-listen usocket:*wildcard-host* port))
    (setf (running server) t)
    ;; (setf (server-thread server) (bordeaux-threads:make-thread (lambda () (server-loop server))))
    (unwind-protect
      (server-loop
        server)
      (stop-server server))))

;; Starts the server on the specified port
(defun start-unix-socket-server (&optional (socket-path *default-server-sock*))
  "Starts the server on the specified port.

  Args:
    port: The port on which to start the server.
  Returns:
    None.
  Example:
    (start-server 8080)"
  (let ((server (make-instance 'lisp-unix-socket-server)))
    (when (probe-file socket-path)
      (delete-file socket-path))
    (setf (socket-path-of server) socket-path)
    (log:info "Starting server on socket ~A" socket-path)

    (setf (server-socket server) (unix-sockets:make-unix-socket
                                   socket-path))
    (setf (running server) t)
    ;; (setf (server-thread server) (bordeaux-threads:make-thread (lambda () (server-loop server))))
    (unwind-protect
      (server-loop
        server))
    (stop-server server)))

;; Stops the server gracefully
(defmethod stop-server ((server lisp-network-server))
  "Stops the running server instance."
  (setf (running server) nil)
  (usocket:socket-close (server-socket server))
  (bordeaux-threads:join-thread (server-thread server))
  (log:info "Server stopped"))

(defmethod stop-server ((server lisp-unix-socket-server))
  "Stops the running server instance."
  (setf (running server) nil)
  (unix-sockets:close-unix-socket (server-socket server))
  (bordeaux-threads:join-thread (server-thread server))
  (log:info "Server stopped"))
