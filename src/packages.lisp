(ql:quickload :log4cl)
(ql:quickload :usocket)
(ql:quickload :cl-indentify)
(ql:quickload :lisp-critic)
(ql:quickload :trivial-formatter)
(ql:quickload :sblint)

(defpackage :cl-format-server
  (:use :cl :log4cl)
  (:export :main
    :send-request
    :*DEFAULT-SERVER-PORT*
    :*SERVER-INSTANCE*
    :keywordify
    :str-to-file
    :read-sexps-from-string
    :keywordify
    :str-to-file
    :file-content-as-str
    :list-handlers
    :file-exists-p
    :newline-str
    :with-content-tmp-file
    :with-calc-tmp-file
    :with-file-content
    :with-replacing-file
    :handle
    :defhandler
    ))
