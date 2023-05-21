(in-package :cl-format-server)


;; (defparameter *default-handler :trivial-formatter)
;; (defgeneric handle-code (handler source-code))

;; (defmethod handle-code ((handler (eql :trivial-formatter)) source-code)
;;   (log:info "trivial-formatter on:\n ~A" source-code)
;;   (let ((stream (make-string-output-stream ))
;;          (code-obj (if (listp source-code) source-code(trivial-formatter:read-as-code (make-string-input-stream source-code)))))
;;     (trivial-formatter:print-as-code
;;       code-obj
;;       stream)
;;     (get-output-stream-string stream)    ))

;; respond with list of handle-code methods
;; (defmethod handle-code ((handler (eql :list-handlers)) source-code)
;;   (log:info "list-handlers on:\n ~A" source-code)
;;   (let ((stream (make-string-output-stream )))
;;     (loop
;;        :for method :in (generic-function-methods #'handle-code)
;;        :do (format stream "~A~%" (method-function method)))
;;     (get-output-stream-string stream)    ))


;; (defun request-source-code (request)
;;   (log:info "Request: ~A" request)
;;   (or (getf request :source-code)request))


;; (defun request-formatter (request)
;;   (or (getf request :handler)))

;; (handle-code *default-handler '(defun foo (x) (print                   x)))
;; (handle-code *default-handler '(ls    a
;;                                  d (defun foo (x) (print                   x))))
;; (handle-code *default-handler '((dd)ls    a
;;                                  d (defun foo (x) (print                   x))))
;; (handle-code *default-handler "(dd)(ls    a)
;;                         (d) (defun foo (x) (print                   x))")

;; (log:info
;;   (trivial-formatter:read-as-code (make-string-input-stream "(dd)(ls    a)
;;                         (d) (defun foo (x) (print                   x))(") t 'dud
;;     )
;;   )
;; (read-sequence (make-array 100 :element-type 'character )
;;   (make-string-input-stream "(dd)(ls    a)
;;                         (d) (defun foo (x) (print                   x))(")
;;   :start 0 :end 100)
;; eof-value: if the end of file is reached, return this value. If eof-error-p is true, eof-value is ignored.
;; eof-error-p: if true, an end of file error is signaled if the end of file is reached.
;; recursive-p: if true, read-as-code-r is called recursively to read the next form.
;; (read-as-code-r (make-string-input-stream "(dd)(ls    a)
;;                        (d) (defun foo (x) (print                   x))(") t 'dud
;;   )

;; (str-to-file (let ((s (file-content-as-str "cl-format-server.lisp"))
;;        (stream (make-string-output-stream)))
;;   (trivial-formatter:print-as-code (read-sexps-from-string s) stream )
;;   (get-output-stream-string  stream  )) "cl-format-server_ref.lisp")
;; (read-sexps-from-string (file-content-as-str "cl-format-server.lisp"))
;; recursive read operation
;; (defun read-as-code-r (stream &optional eof-error-p eof-value recursive-p)
;;   (let ((*readtable* (copy-readtable *readtable*)))
;;     (set-macro-character #\# #'(lambda (stream char)
;;                                  (declare (ignore char))
;;                                  (read-as-code-r stream nil nil t)) nil *readtable*)
;;     (trivial-formatter:read-as-code stream eof-error-p eof-value recursive-p)))


;; (with-output-to-string (stream)
;; (sblint:run-lint-file "aa.lisp" stream ))

;; (with-output-to-string (stream)
;; (indentify:indentify (make-string-input-stream "(def a A)") stream ))
;; ;; (ql:quickload :cl-indentify)
;; (with-output-to-string (stream)
;; (lisp-critic:critique-file "aa.lisp" stream ))

(defhandler :cl-indentify code
 (with-output-to-string (stream)
   (indentify:indentify (make-string-input-stream code) stream)))

(defhandler :trivial-formatter code
  (let* ((sexps (if (listp code)
                  code
                  (read-sexps-from-string code
                    :read-fun #'trivial-formatter:read-as-code)))
          (_ (log:info "trivial-formatter on:\n ~A" sexps))
         (formatted (with-output-to-string (stream)
                      (loop
                        :for sexp :in sexps
                        :do (progn (trivial-formatter:print-as-code
                              sexp stream)
                              (format stream "~%~%"))))))
                      ;; (mapcar (lambda (x) (trivial-formatter:print-as-code
                      ;;   x stream)) sexps)

    formatted)
  )



;; (handle :trivial-formatter "(dd)(ls    a)
;;                        (d) (defun foo (x) (print                   x))")
;; (handle :trivial-formatter-file "cl-format-server.lisp")

;; (test simple-test
;;   (let ((s (file-content-as-str "cl-format-server.lisp"))
;;          (stream (make-string-output-stream)))
;;     (trivial-formatter:print-as-code (read-sexps-from-string s) stream )
;;     (get-output-stream-string  stream  )))

;; apply read-sexps to print-as-code
(defun print-sexps-as-code (input-string)
  (with-input-from-string (stream input-string)
    (loop
      :for sexp := (read stream nil :eof)
      :until (eq sexp :eof)
      :do (trivial-formatter:print-as-code sexp)
      :collect sexp)))
