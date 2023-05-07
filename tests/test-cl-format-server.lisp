(in-package :cl-format-server-tests)

;; Define your project tests here...

(def-suite testmain
    :description "test suite 1")

(in-suite testmain)

(test test1
  (is (= (+ 1 1)
         3)))
(ert-deftest test-handle-request ()
  ;; Add test cases for handle-request
  )

(ert-deftest test-create-temporary-asdf-project ()
  ;; Add test cases for create-temporary-asdf-project
  )

(ert-deftest test-add-code-to-temporary-asdf-project ()
  ;; Add test cases for add-code-to-temporary-asdf-project
  )

(ert-deftest test-delete-temporary-asdf-project ()
  ;; Add test cases for delete-temporary-asdf-project
  )

(ert-deftest test-linters-and-formatters ()
  ;; Add test cases for lint and format functions
  )
