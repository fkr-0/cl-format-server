(in-package :cl-format-server)

;;;;; Define some utility functions

(defun read-sexps-from-string (input-string &optional &key (read-fun #'read))
  "Reads sexps from a string and returns a list of the sexps read. The default
read function is `read` but you can pass any function that takes a stream as
first argument and returns a sexp."
  (with-input-from-string (stream input-string)
    (loop
      :for sexp := (funcall read-fun stream nil :eof)
      :until (eq sexp :eof)
      :collect sexp)))

(defun keywordify (ele &rest suffixes)
  "Converts a string to a keyword. The string is converted to uppercase and all
characters in the string are converted to keywords. The suffixes are also
converted to keywords and concatenated to the string with a \"-\" in between.
For example (keywordify \"my-fun\" \"file\" \"replace\") would return the
keyword :MY-FUN-FILE-REPLACE"
  (let* ((clean-str (lambda (str) (string-upcase (string-trim '(#\: #\-) (string str)))))
          (ele (funcall clean-str ele))
          (suffixes (mapcar clean-str suffixes))
          (suffixes (if suffixes (reduce #'(lambda (a b) (concatenate 'string a "-" b))suffixes) nil)))
    (intern (string-upcase (concatenate 'string "" ele (if suffixes (concatenate 'string "-" suffixes) "") )) 'keyword)))

(defun str-to-file (str file-path &key (if-exists :supersede))
  "Writes a string to a file. If the file exists it is overwritten by default"
  (with-open-file (stream file-path :direction :output :if-exists if-exists)
    (write-sequence str stream)))

(defun file-content-as-str (file-path)
  "Returns the content of a file as a string"
  (with-open-file (stream file-path)
    (let ((str (make-string (file-length stream))))
      (read-sequence str stream)
      str)))

(defun list-handlers (&optional &key (generic #'handle))
  "Returns a list of all handlers defined for the generic function `handle`"
  (loop
    :for name :in (sb-mop:generic-function-methods generic)
    :collect (slot-value (car
                           (sb-mop:method-specializers name) )'sb-pcl::object) ))

(defun file-exists-p (file-path)
  "Returns true if file exists else nil"
  (ignore-errors
    (open file-path :direction :input :if-does-not-exist nil)))

(defun newline-str (in-str)
  "Converts a string to a string with newlines at the end of each line"
  (concatenate 'string (string-trim '(#\  #\NewLine) in-str) (string #\Newline)))

(defmacro with-file-content ((file-path str-symbol) &body body)
  "Opens a file and binds the content of the file to the symbol `str-symbol` in
the body. The file is closed after the body has been evaluated."
  (let ((file-path-symbol (gensym)))
    ;; (str-symbol (gensym)))
    `(let (
            (,file-path-symbol ,file-path))
       (when (not (stringp ,file-path-symbol))
         (error "file-path must be a string"))
       (let ((,str-symbol (file-content-as-str ,file-path-symbol)))
         ,@body))))

(defmacro with-replacing-file ((file-path str-symbol) &body body)
  "Opens a file and binds the content of the file to the symbol `str-symbol` in
the body. The file is closed after the body has been evaluated. If the file
does not exist it is created. If the file exists it is overwritten."
  (let ((file-path-symbol (gensym)))
    `(let (
            (,file-path-symbol ,file-path))
       (when (not (stringp ,file-path-symbol))
         (error "file-path must be a string"))
       (let ((,str-symbol (file-content-as-str ,file-path-symbol)))
         (let ((rs (progn ,@body)))
           (str-to-file rs ,file-path-symbol))))))

;; (defmacro replacing-file (file-path &body body)
;;   "Opens a file and binds the content of the file to the symbol `str-symbol` in
;; the body. The file is closed after the body has been evaluated. If the file
;; does not exist it is created. If the file exists it is renamed to
;; <filename>.bak and the new file is written to the file-path."
;;   (let ((file-path-symbol (gensym)))
;;     `(let ((,file-path-symbol ,file-path))
;;        (when (not (stringp ,file-path))
;;          (error "file-path must be a string"))
;;        (unwind-protect
;;          (if (not (file-exists-p ,file-path))
;;            (str-to-file (newline-str ,@body) ,file-path)
;;            (progn
;;              (rename-file ,file-path-symbol (concatenate 'string ,file-path-symbol ".bak"))
;;              (str-to-file (newline-str ,@body) ,file-path)))
;;          (delete-file (concatenate 'string ,file-path-symbol ".bak"))))))


;;;;; Define the generic function `handle` and a macro `defhandler` adding implementations to `handle`

;; Define the generic function `handle`
(defgeneric handle (handler source-code-str)
  (:documentation "Handles the source-code-str with the handler"))

;; Define the `defhandler` macro
(defmacro defhandler (handler-name code-str &body body)
  "Defines a handler for the generic function `handle`. The handler is defined
as a method dispatching on the keyword `handler-name`. The handler is also
defined as a method dispatching on the keyword `handler-name-file` which
expects the `code-str` to be a file path. The handler is also defined as a
method dispatching on the keyword `handler-name-replace` which expects the
`code-str` to be a file path. The handler will replace the file with the
result of the body."
  (let ((handler-name-symbol (keywordify handler-name))
         (handler-file-symbol (keywordify handler-name "file"))
         (handler-replace-symbol (keywordify handler-name "replace")))
    `(progn
       ;; Define the method dispatching on :<handlername>
       (defmethod handle ((handler (eql ',handler-name-symbol)) ,code-str)
         (newline-str
           ,@body))
       ;; Define the method dispatching on :<handlername>-file
       (defmethod handle ((key (eql ,handler-file-symbol)) ,code-str)
         (when (listp ,code-str)
           (setf ,code-str (first ,code-str)))
         (if (not (file-exists-p ,code-str))
           (error "File ~a does not exist" ,code-str))
         (handle ,handler-name-symbol (file-content-as-str ,code-str)))
       ;; Define the method dispatching on :<handlername>-replace
       (defmethod handle ((key (eql ,handler-replace-symbol)) ,code-str)
         (when (listp ,code-str)
           (setf ,code-str (first ,code-str)))
         (if (not (file-exists-p ,code-str))
           (error "File ~a does not exist" ,code-str)
           (with-replacing-file (,code-str code)
             (handle ,handler-name-symbol code)))))))


;; Define a handler using the `defhandler` macro
;; (defhandler :example arg
;;   (concatenate 'string "Arg: " (string arg)))
;; Test the handler
;; (handle :EXAMPLE "Some data")
;; (handle :EXAMPLE-FILE "oho.test")
;; (handle :EXAMPLE-REPLACE "oho.test")
;; (list-handlers)
