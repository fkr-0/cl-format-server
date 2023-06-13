(in-package :cl-format-server-tests)

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

(test test-read-sexps-from-string
  (let ((input-string "(+ 1 2)\n(- 3 4)\n(/ 5 6)\n(* 7 8)\n"))
    (is (equal (read-sexps-from-string input-string) '(+ 1 2 - 3 4 / 5 6 * 7 8)))))

(test test-keywordify
  (is (equal (keywordify "my-fun" "file" "replace") :MY-FUN-FILE-REPLACE))
  (is (equal (keywordify "my-fun") :MY-FUN)))

(test test-str-to-file
  (let ((test-str "This is a test string"))
    (str-to-file test-str *test-file-path*)
    (is (equal (file-content-as-str *test-file-path*) test-str))))

(test test-file-content-as-str
  (let ((test-str "This is a test string"))
    (str-to-file test-str *test-file-path*)
    (is (equal (file-content-as-str *test-file-path*) test-str))))

(test test-list-handlers
  (defun test-handler-1 (source-code-str) "Handler 1")
  (defun test-handler-2 (source-code-str) "Handler 2")
  (defun test-handler-3 (source-code-str) "Handler 3")
  (defgeneric test-handle (handler source-code-str))
  (defmethod test-handle :test-handler-1 ((handler symbol) source-code-str) (test-handler-1 source-code-str))
  (defmethod test-handle :test-handler-2 ((handler symbol) source-code-str) (test-handler-2 source-code-str))
  (defmethod test-handle :test-handler-3 ((handler symbol) source-code-str) (test-handler-3 source-code-str))
  (is (equal (list-handlers 'test-handle) '(test-handler-1 test-handler-2 test-handler-3))))

(test test-file-exists-p
  (let ((test-str "This is a test string"))
    (str-to-file test-str *test-file-path*)
    (is (file-exists-p *test-file-path*))
    (delete-file *test-file-path*)
    (is (not (file-exists-p *test-file-path*)))))

(test test-newline-str
  (let ((test-str "This is a test string\nwith multiple lines\n"))
    (is (equal (newline-str test-str) "This is a test string\nwith multiple lines\n\n"))))

(test test-with-file-content
  (let ((test-str "This is a test string"))
    (str-to-file test-str *test-file-path*)
    (with-file-content (*test-file-path* str)
      (is (equal str test-str)))))



(test test-file-content-as-str
  (is (equal "This is a test file." (file-content-as-str "test/test-file.txt"))))

(test test-file-exists-p
  (is (file-exists-p "test/test-file.txt"))
  (is (not (file-exists-p "test/non-existent-file.txt"))))

(test test-newline-str
  (is (equal "This is a test file.\n" (newline-str "This is a test file.")))
  (is (equal "This is a test file.\n" (newline-str "This is a test file.\n")))
  (is (equal "This is a test file.\n" (newline-str "This is a test file.\r\n"))))

(test test-with-file-content
  (with-test-file (file-path str)
    (with-file-content (file-path str)
      (is (equal "This is a test file." str)))))

(test test-with-replacing-file
  (with-test-file (file-path str)
    (with-replacing-file (file-path str)
      (is (equal "This is a test file." (file-content-as-str file-path))))))

(test test-defhandler
  (defhandler :test-handler code-str
    (concatenate 'string "Test handler: " code-str))
  (is (equal "Test handler: Some data" (handle :test-handler "Some data")))
  (with-test-file (file-path str)
    (is (equal "Test handler: This is a test file."
          (handle :test-handler-file file-path)))))
