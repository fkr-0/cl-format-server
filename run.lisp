"
Usage:

rlwrap sbcl --load run.lisp

This loads the project's asd, loads the quicklisp dependencies, and
calls the main function.

Then, we are given the lisp prompt.

If you don't want to land in the REPL, you can (quit) below or call lisp with the --non-interactive flag.

Another solution to run the app is to build and run a binary (see README).
"

;(asdf:load-asd "./cl-format-server")
;()
(asdf:load-system "cl-format-server")
(load "src/packages.lisp")
(load "src/core.lisp")
(load "src/client.lisp")
(load "src/util.lisp")
(load "src/handler.lisp")
(load "src/cl-format-server.lisp")
;;(ql:quickload "cl-format-server")
(ql:quickload "sblint")
(ql:quickload :log4cl)
(ql:quickload :usocket)
(ql:quickload :cl-indentify)
(ql:quickload :lisp-critic)
(ql:quickload "trivial-formatter")


(in-package :cl-format-server)
;; (handler-case
    (main)
  ;; (error (c)
  ;;   (format *error-output* "~&An error occured: ~a~&" c)
  ;;   (uiop:quit 1)))
