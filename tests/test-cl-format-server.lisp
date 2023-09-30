(in-package :cl-format-server-tests)

(def-suite cl-format-server-tests
  :description "test suite 1")

(in-suite cl-format-server-tests)

(test basic
  (is (equal :HELLO (keywordify "hello"))))

(test with-suffix
  (is (equal :HELLO-WORLD (keywordify "hello" "world"))))

(test multiple-suffixes
  (is (equal :HELLO-WORLD-AGAIN (keywordify "hello" "world" "again"))))

(test trim-colons
  (is (equal :HELLO (keywordify ":hello:")))
  (is (equal :HELLO-WORLD (keywordify ":hello:" "world:")))
  (is (equal :HELLO-WORLD (keywordify ":hello:" ":world:")))
  (is (equal :HELLO-WORLD-AGAIN (keywordify ":hello:" "world:" "again:"))))

(test uppercase
  (is (equal :HELLO (keywordify "HeLlO"))))

;; (test with-spaces
;;     (is (equal :HELLO-WORLD (keywordify "hello world"))))

;; (def-suite str-to-file-test :in cl-server-tests)

;; (in-suite str-to-file-test)

(defparameter *test-file-path* "/tmp/cl-format-server-test-file.txt")

(defmacro with-test-file ((file-path str-symbol) &body body)
  "Creates a temporary file with the content of `str-symbol` and binds the
file path to `file-path` in the body. The file is deleted after the body has
been evaluated."
  (let ((file-path-symbol (gensym)))
    `(let ((,str-symbol (gensym)))
       (with-open-file (,str-symbol (make-pathname :name "test-file" :type "txt")
                         :direction :output :if-exists :supersede)
         (write-line "This is a test file." ,str-symbol))
       (let ((,file-path-symbol (make-pathname :name "test-file" :type "txt")))
         ,@body)
       (delete-file ,file-path-symbol))))
(test basic
  (with-test-file (file-path str)
    (str-to-file "Hello" file-path)
    (is (equal "Hello" (file-content-as-str file-path)))
    (delete-file file-path)))

(test overwrite
  (let ((file-path "test.txt"))
    (str-to-file "Hello" file-path)
    (str-to-file "World" file-path)
    (is (equal "World" (file-content-as-str file-path)))
    (delete-file file-path)))

(test append
  (let ((file-path "test.txt"))
    (str-to-file "Hello" file-path)
    (str-to-file "World" file-path :if-exists :append)
    (is (equal "HelloWorld" (file-content-as-str file-path)))
    (delete-file file-path)))

;; (def-suite with-file-content-test :in cl-server-tests)

;; (in-suite with-file-content-test)

(test basic
  (let ((file-path "test.txt")
         (str))
    (str-to-file "Hello" file-path)
    (with-file-content (file-path str)
      (is (equal "Hello" str)))
    (delete-file file-path)))

(test error
  (let ((file-path 1)
         (str))
    (signals error
      (with-file-content (file-path str)
        (is (equal "Hello" str))))))
