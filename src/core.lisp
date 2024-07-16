(in-package :cl-format-server)

;;;; Server Configuration and Global Variables

;; Defines the default port for the server
(defparameter *default-server-port* 8080
  "The default port on which the server listens.")

(defparameter *default-server-sock* "/tmp/cl-format-server.sock"
  "The default port on which the server listens.")

;; Specifies the default request handler
(defparameter *default-handler* :trivial-formatter
  "Request handler used when no specific handler is specified in the request.")

;; Holds the instance of the currently running server
(defvar *server-instance* nil
  "The server instance that is currently running. Set when the server starts.")

;; Defines the lisp-server class with essential properties
(defclass lisp-server ()
  ((server-thread :initform nil :accessor server-thread)
    (server-socket :initform nil :accessor server-socket)
    (running :initform nil :accessor running))
  (:documentation "Class representing the generic Lisp server with configuration and state."))

(defclass lisp-network-server (lisp-server)
  ((port :initform 8080 :accessor port-of :initarg :port))
  (:documentation "Lisp server using network sockets."))

(defclass lisp-unix-socket-server (lisp-server)
  ((socket-path :initform "/tmp/cl-format-server.sock" :accessor socket-path-of :initarg :socket-path))
  (:documentation "Lisp server using Unix sockets."))

;; Base class method to start the server
(defun start-server (server)
  (initialize-server-socket server)
  (setf (running server) t)
  (server-loop server))

;; Abstract method for initializing server socket
(defgeneric initialize-server-socket (server))

;; Concrete implementations for each server type
(defmethod initialize-server-socket ((server lisp-network-server))
  (setf (server-socket server) (usocket:socket-listen usocket:*wildcard-host* (port-of server))))

(defmethod initialize-server-socket ((server lisp-unix-socket-server))
  (setf (server-socket server) (unix-sockets:make-unix-socket (socket-path-of server))))

;;;; Server Initialization



;; Initializes the server instance after creation
(defmethod initialize-instance :after ((server lisp-server) &key)
  "Initializes the server instance after creation.

  Args:
    server: The server instance to initialize."
  (setf *server-instance* server))

(defmethod server-loop ((server lisp-server))
  "Main server loop that listens for incoming connections and handles them."

  (log:info "Server started on ~A" server)
  (unwind-protect

    (loop while (running server) do
      (handler-case
        (let ((client-socket (accept-connection server)))
          (handle-client server client-socket))
        (error (e)
          (log:error "Failed to handle client due to error: ~A" e)))))
  (log:info "Server stopped")
  (close-server-socket (server-socket server)))

(defun respond-with-error (client-socket server error-message)
  (with-open-stream (stream (socket-stream server client-socket))
    (format stream "HTTP/1.1 500 Internal Server Error~%Content-Type: text/plain~%~%~A" error-message)
    (finish-output stream)))

(defmethod socket-stream ((server lisp-network-server) client-socket)
  (usocket:socket-stream client-socket))

(defmethod socket-stream ((server lisp-unix-socket-server) client-socket)
  (unix-sockets:unix-socket-stream client-socket))

(defmethod handle-client ((server lisp-server) client-socket )
  "Handles a client connection, reading and responding to requests.

  Args:
    client-socket: The client socket to handle.
    server: The server instance.
  Returns:
    None.
  Example:
    (handle-client client-socket server)"
  (with-open-stream (client-stream (socket-stream server client-socket))
    (log:info "Client connected: ~A" client-socket)
    (handler-case
      (loop for request = (read-request client-stream)
        while request do
        (respond client-stream (handle-request request)))
      (error (e)
        (log:error "Error during client request processing: ~A" e)
        (respond-with-error client-socket server (format nil "Internal Server Error ~A~%" e))))))



;; Abstract accept connection method
(defgeneric accept-connection (server))

;; Concrete implementations for accepting connections
(defmethod accept-connection ((server lisp-network-server))
  (usocket:socket-accept (server-socket server)))

(defmethod accept-connection ((server lisp-unix-socket-server))
  (unix-sockets:accept-unix-socket (server-socket server)))

;; Generic stop server method
(defmethod stop-server ((server lisp-server))
  (setf (running server) nil)
  (close-server-socket (server-socket server))
  (log:info "Server stopped")
  (bordeaux-threads:join-thread (server-thread server)))

;; Abstract close server socket method
(defgeneric close-server-socket (socket))

;; Implementations for closing server sockets
(defmethod close-server-socket ((socket usocket:usocket))
  (usocket:socket-close socket))

(defmethod close-server-socket ((socket unix-sockets::unix-socket))
  (unix-sockets:close-unix-socket socket)
  ;; Remove the socket file
  (uiop:delete-file-if-exists (socket-path-of *server-instance*)))

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

(defgeneric respond (client-stream response)
  (:method ((client-stream usocket:stream-usocket) response)
    (log:info "UsockResponse: ~A" response)
    (format client-stream "~A~%" response)
    (finish-output client-stream))
  (:method ((client-stream flexi-streams:flexi-io-stream) response)
    (log:info "FlexiResponse: ~A" response)
    (dotimes (x (length response))
      (write-byte (char-int (char response x)) client-stream))
    (finish-output client-stream))
  (:documentation "Responds to a client request with the given response.

  Args:
    client-stream: The client stream to respond to.
    response: The response to send.
  Returns:
    None.
  Example:
    (respond client-stream \"Hello, world!\")"))


