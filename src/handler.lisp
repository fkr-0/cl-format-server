(in-package :cl-format-server)



(defhandler :cl-indentify code
  (with-output-to-string (stream)
    (indentify:indentify (make-string-input-stream code) stream)))

(defhandler :trivial-formatter code
  (let* ((sexps (if (listp code)
                  code
                  (read-sexps-from-string code
                    :read-fun #'trivial-formatter:read-as-code)))
          (formatted
            (with-output-to-string (stream)
              ;; (log:info "trivial-formatter on:\n ~A" sexps)
              (loop
                :for sexp :in sexps
                :do (progn (trivial-formatter:print-as-code
                             sexp stream)
                      (format stream "~%~%"))))))
    ;; (mapcar (lambda (x) (trivial-formatter:print-as-code
    ;;   x stream)) sexps)
    formatted))

(defhandler :lisp-critic cd
  (with-output-to-string (str)
    (with-content-tmp-file
      ("/tmp/cl.format.server.XXXXXX" fn fn-stream cd)
      (lisp-critic:critique-file fn str))))

(defhandler :sblint cd
  (with-output-to-string (str)
    (with-content-tmp-file
      ("/tmp/cl.format.server.XXXXXX" fn fn-stream cd)
      (sblint:run-lint-file fn str))))


    formatted)
  )



;; apply read-sexps to print-as-code
(defun print-sexps-as-code (input-string)
  (with-input-from-string (stream input-string)
    (loop
      :for sexp := (read stream nil :eof)
      :until (eq sexp :eof)
      :do (trivial-formatter:print-as-code sexp)
      :collect sexp)))
