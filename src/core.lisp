(in-package :cl-format-server)
;;;; server

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
        (bordeaux-threads:make-thread
          (lambda ()
            (unwind-protect
              (with-time-measurement
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
                      (format stream "~A~%" response) ;; former "write-response"
                      (finish-output client-stream))))
                (usocket:socket-close client-socket)))))))))

(defun start-server (p);(&optional (port 8080))
  (let ((server (make-instance 'lisp-server)))
    ;; (log:info "Starting server on port ~A" port)
    (setf (server-socket server) (usocket:socket-listen usocket:*wildcard-host* p))
    (setf (running server) t)
    ;; (setf (server-thread server) (bordeaux-threads:make-thread (lambda () (server-loop server))))
    (server-loop
      server)))

(defmacro with-time-measurement (&body body)
  `(let ((start-time (get-internal-real-time)))
     (unwind-protect
       (progn ,@body)
       (let ((end-time (get-internal-real-time)))
         (log:info "Elapsed time: ~As" (/ (- end-time start-time) 1000000.0))))))

(defun stop-server (server)
  (setf (running server) nil)
  (usocket:socket-close (server-socket server))
  (bordeaux-threads:join-thread (server-thread server))
  (log:info "Server stopped"))

(defmacro with-temporary-asdf-project ((temp-dir) &body body)
  `(let ((,temp-dir (create-temporary-asdf-project)))
     (unwind-protect
       (progn ,@body)
       (delete-temporary-asdf-project ,temp-dir))))

;; (defun create-temporary-asdf-project ())
;; (defun add-code-to-temporary-asdf-project (source-code asdf-project))
;; (defun delete-temporary-asdf-project (asdf-project))

(defun write-response (stream response)
  )


(defun read-request (stream)
  (log:info "reading request")
  (let (;; (read-delimited-lis t)
         (lines (read stream nil nil)))
    (log:info "read line ~A" lines)
    lines))

(defun handle-request (request)
  (log:info "handling request ~A" request)
  (let* ((handler (when (typep (car request) 'symbol)
                    (car request)))
          (source-code (if (null handler)
                         (car request)
                         (cadr request))))
    (if (null handler)
      (setf handler *default-handler*)
      (handle handler source-code))))



;;
;; read request from stream as string:
;; (defun read-request (stream)
;;  (let ((request (read stream)))
;;  (log:info "Request: ~A" request)
;;
;; (defun read-response (stream)
;;  (let ((response (read stream)))
;;  (log:info "Response: ~A" response)
;;  response))
;; (defun write-response (stream response)
;;  (with-output-to-string (s)
;;  (format s "~A" response)))
;;  (format stream "~A" response)))
;;  read stream: 
;; (defun handle-client (client-stream)
;;   (let ((request (get-output-stream-string client-stream )
;;           ))

;;     (log:info "RRequest: ~A" request)
;;     (let ((response (handle-request request)))
;;       (log:info "Response: ~A" response)
;;       (write-response client-stream response)
;;       (finish-output client-stream))))
;; (let ((request  (read client-stream )))
;;   (log:info "RequestinHandler: ~A" request)
;;   (let ((response (handle-request request)))
;;     (log:info "Response: ~A" response)
;;     (write-response client-stream response))))
;; (defun handle-client (client)
;;   (let ((buffer (make-array 4096 :element-type 'character :fill-pointer 0))
;;          (data nil))
;;     (loop
;;       (setf data (usocket:client- buffer :dont-wait t))
;;       (when data
;;         (let ((request (parse-request data)))
;;           (when request
;;             (let ((response (handle-request request)))
;;               (usocket:socket-send client response)
;;               (usocket:socket-close client)
;;               (return))))))))
;; (defun listen-for-requests (port)
;;   (let ((p (or port *default-server-port*)))
;;     (log:info "Listening for requests on port ~A" p)
;;     ;; (usocket:socket-listen usocket:*wildcard-host* p)
;;     (let ((server-socket (usocket:socket-listen usocket:*wildcard-host* p :reuse-address t))
;;            (client-socket nil)
;;            (client-stream nil))


;;       (loop
;;         (setf client-socket (usocket:socket-accept server-socket))
;;         (setf client-stream (usocket:socket-stream client-socket))
;;         (handler-case

;;           (handle-client client-stream)
;;           (error (e)
;;             (log:info "Error handling client: ~A" e)))
;;         (usocket:socket-close client-socket)))))
;; (listen-for-requests 8080)

;;     (usocket:with-server-socket
;;       (server-socket
;;         (usocket:socket-listen "localhost" 8080 :reuse-address t  ) )

;;     ;; :reuseaddress t
;;     )
;;   ;; (loop
;;   ;;   (usocket:with-client-socket (client-socket client-stream server-socket)
;;   ;;     (let* ((request (read client-stream))
;;   ;;             (response (handle-request request)))
;;   ;;       (write response :stream client-stream)
;;   ;;       (finish-output client-stream)))))


;;   (loop
;;     (usocket:with-client-socket (client-socket client-stream );JKserver-socket)
;;       (let* ((request (read client-stream))
;;               (response (handle-request request)))
;;         (write response :stream client-stream)
;;         (finish-output client-stream))))))
;; (listen-for-requests 8080)
;; (listen-for-requests 8080)

;; (defun handle-request (request)
;;   (let* ((source-code (getf request :source-code))
;;          (requested-formatter (getf request :formatter))
;;          (requested-linter (getf request :linter))
;;          (available-formatters '(:trivial-indent :cl-formatter))
;;          ;; (available-linters '(:sb-lint))
;;           (available-linters '())
;;          (formatter (or (find requested-formatter available-formatters) (first available-formatters)))
;;          (linter (or (find requested-linter available-linters) (first available-linters))))
;;     (handler-case
;;         ;; (with-temporary-asdf-project (temp-dir)
;;         ;;   (add-code-to-temporary-asdf-project temp-dir source-code)
;;         ;;   (multiple-value-bind (linted-code formatted-code)
;;         ;;       (values
;;         ;;        (if requested-linter (lint-source-code linter source-code) source-code)
;;         ;;        (if requested-formatter (format-source-code formatter source-code) source-code))
;;         ;;     (if requested-linter
;;         ;;         (if requested-formatter
;;         ;;             formatted-code
;;         ;;             linted-code)
;;         ;;         formatted-code)))
;;         (format-code :trivial-formatter source-code)
;;       (error (e)
;;         (format t "Error: ~A~%" e)
;;         (format nil "HTTP/1.1 500 Internal Server Error~%")))))

;; (defun handle-request (request)
;;   (log:info "handling request ~A" request)
;;   (let* ((source-code (request-source-code request))
;;           (linter (request-linter request))
;;           (formatter (request-formatter request))
;;           (asdf-project (create-temporary-asdf-project)))
;;     (unwind-protect
;;       (progn
;;         (log:info "Linting ~A" source-code)
;;         (add-code-to-temporary-asdf-project source-code asdf-project)
;;         (let ((linted-code (lint linter source-code))
;;                (formatted-code (format formatter source-code)))
;;           (values linted-code formatted-code)))
;;       (delete-temporary-asdf-project asdf-project))))

