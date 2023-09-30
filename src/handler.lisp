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


(defhandler :triv-asdf cd
  (with-code-in-temporary-asdf-system
    cd my-temp-system tmp-code-file
    (newline-str (file-content-as-str tmp-code-file))))
