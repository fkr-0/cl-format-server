
(load "cl-format-server.asd")
(load "cl-format-server-tests.asd")

(ql:quickload "cl-format-server-tests")

(in-package :cl-format-server-tests)

(uiop:quit (if (run-all-tests) 0 1))
