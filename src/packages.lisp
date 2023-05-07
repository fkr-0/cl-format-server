(ql:quickload :log4cl)
(ql:quickload :usocket)
(ql:quickload :sblint)
(ql:quickload :cl-indentify)
(ql:quickload :lisp-critic)
(ql:quickload :trivial-formatter)

(defpackage :cl-format-server
  (:use :cl :log4cl)
  (:export :main))
