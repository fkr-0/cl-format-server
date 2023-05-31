(in-package :cl-format-server)



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



;; apply read-sexps to print-as-code
(defun print-sexps-as-code (input-string)
  (with-input-from-string (stream input-string)
    (loop
      :for sexp := (read stream nil :eof)
      :until (eq sexp :eof)
      :do (trivial-formatter:print-as-code sexp)
      :collect sexp)))
