(in-package :cl-format-server)

(defhandler :cl-indentify code
  "Handler for indentifying Lisp code. It formats the code with proper indentation.
  Args:
    code: A string containing the Lisp code to be formatted.
  Returns:
    A string containing the formatted Lisp code.
  "
  (handler-case
    (with-output-to-string (stream)
      (indentify:indentify (make-string-input-stream code) stream))
    (error (e)
      (log:error "Error in :cl-indentify handler: ~A" e)
      "Error formatting code")))


(defhandler :trivial-formatter code
  "Trivial formatter for Lisp code. Formats code without significant structural changes.
  Args:
    code: A string containing the Lisp code to be formatted or a list of s-expressions.
  Returns:
    A string containing the formatted Lisp code.
  "
  (handler-case
    (let* ((sexps (if (listp code)
                    code
                    (read-sexps-from-string code
                      :read-fun #'trivial-formatter:read-as-code)))
            (formatted
              (with-output-to-string (stream)
                (loop
                  :for sexp :in sexps
                  :do (progn (trivial-formatter:print-as-code
                               sexp stream)
                        (format stream "~%~%"))))))
      formatted)
    (error (e)
      (log:error "Error in :trivial-formatter handler: ~A" e)
      "Error formatting code")))


(defhandler :lisp-critic code
  "Handler for critiquing Lisp code using lisp-critic. It provides suggestions for improvements.
  Args:
    code: A string containing the Lisp code to be critiqued.
  Returns:
    A string containing suggestions and critiques of the Lisp code.
  "
  (handler-case
    (with-output-to-string (str)
      (with-content-tmp-file
        ("/tmp/cl.format.server.XXXXXX" fn fn-stream code)
        (lisp-critic:critique-file fn str)))
    (error (e)
      (log:error "Error in :lisp-critic handler: ~A" e)
      "Error processing code critique")))


(defhandler :sblint code
  "Handler for linting Lisp code using sblint. It checks for common issues and coding standards.
  Args:
    code: A string containing the Lisp code to be linted.
  Returns:
    A string containing linting results and suggestions.
  "
  (handler-case
    (with-output-to-string (str)
      (with-content-tmp-file
        ("/tmp/cl.format.server.XXXXXX" fn fn-stream code)
        (sblint:run-lint-file fn str)))
    (error (e)
      (log:error "Error in :sblint handler: ~A" e)
      "Error linting code")))



(defhandler :triv-asdf code
  "Handler for processing Lisp code within a temporary ASDF system.
  Args:
    code: A string containing the Lisp code to be processed.
  Returns:
    A string representation of the processed code.
  "
  (handler-case
    (with-code-in-temporary-asdf-system
      code my-temp-system tmp-code-file
      (newline-str (file-content-as-str tmp-code-file)))
    (error (e)
      (log:error "Error in :triv-asdf handler: ~A" e)
      "Error processing code in temporary ASDF system")))
