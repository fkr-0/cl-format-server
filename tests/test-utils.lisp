(in-package :cl-format-server-tests)

(def-suite cl-server-utils-tests
  :in cl-format-server-tests)

(in-suite cl-server-utils-tests)

(test test-list-handlers
  (let ((result (list-handlers)))
    (is (typep result 'list))
    (dolist (handler result)
      (is (typep handler 'symbol)))))

(test test-read-sexps-from-string
  (let ((input-string "(+
2
3)(+ 1 2
(- 3 4)
(/ 5 6)
(* 7 8)

)
"))
    (is (equal (read-sexps-from-string input-string)
          '((+ 2 3)(+ 1 2 (- 3 4) ( / 5 6 ) (* 7 8)))))))

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

(test test-file-exists-p
  (let ((test-str "This is a test string"))
    (str-to-file test-str *test-file-path*)
    (is (file-exists-p *test-file-path*))
    (delete-file *test-file-path*)
    (is (not (file-exists-p *test-file-path*)))))

(test test-with-file-content
  (let ((test-str "This is a test string"))
    (str-to-file test-str *test-file-path*)
    (with-file-content (*test-file-path* str)
      (is (equal str test-str)))))

(test test-file-content-as-str
  (with-content-tmp-file ("/tmp/cl-format-server-test.XXXXXX"
                           test-fn test-fh "This is a test file.")
    (is (equal "This is a test file." (file-content-as-str test-fn)))))

(test test-file-exists-p
  (with-content-tmp-file ("/tmp/cl-format-server-test.XXXXXX" test-fn test-fh ".")
    (is (file-exists-p test-fn))
    (is (not (file-exists-p "test/non-existent-file.txt")))))

(test test-newline-str
  (is (string= "This is a test file.
" (newline-str "This is a test file.
")))
  (is (string= (newline-str "This is a
test file.") (newline-str "This is a
test file.
")))
  (is (string= "This is a test file.
" (newline-str "This is a test file.
"))))

(test test-with-file-content
  (with-content-tmp-file ("/tmp/cl-format-server-test.XXXXXX"
                           test-fn test-fh "This is a test file.")
    (with-file-content (test-fn str)
      (is (string= "This is a test file." str)))))

(test test-with-replacing-file
  (with-content-tmp-file ("/tmp/cl-format-server-test.XXXXXX"
                           test-fn test-fh "This is a test file.")
    (with-replacing-file (test-fn str)
      "This was a test file, but now it is still a
test file, but with a different content.")
    (is (equal "This was a test file, but now it is still a
test file, but with a different content." (file-content-as-str test-fn)))))

(test test-defhandler
  (defhandler :test-handler code-str
    (concatenate 'string "Test handler: " code-str))
  (is (equal "Test handler: Some data
" (handle :test-handler "Some data")))
  (with-content-tmp-file ("/tmp/cl-format-server-test.XXXXXX"
                           test-fn test-fh "This is a test file.")
    (is (equal "Test handler: This is a test file.
" (handle :test-handler-file test-fn)))))
