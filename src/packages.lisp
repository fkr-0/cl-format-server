(ql:quickload :log4cl)
(ql:quickload :usocket)
(ql:quickload :cl-indentify)
(ql:quickload :lisp-critic)
(ql:quickload :trivial-formatter)
(ql:quickload :sblint)

(defpackage :cl-format-server
  (:use :cl :log4cl)
  (:export :main :send-request :*DEFAULT-SERVER-PORT* :*SERVER-INSTANCE*))
