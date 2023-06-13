(in-package :asdf-user)
(defsystem "cl-format-server-tests"
  :description "Test suite for the cl-format-server system"
  :author "cbadger <cbadger@systemli.org>"
  :version "0.0.1"
  :depends-on (:cl-format-server
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-cl-format-server")
                                      (:file "test-utils"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
