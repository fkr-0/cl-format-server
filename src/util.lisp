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

(defun str-replace (str old new)
  "Replaces all occurrences of `old` with `new` in `str`
  Arguments:
    str: The string in which to replace
    old: The string to replace
    new: The string to replace `old` with
  Returns:
    The string with all occurrences of `old` replaced with `new`
  Example:
    (str-replace \"Hello World!\" \"World\" \"Universe\")
    => \"Hello Universe!\""
  (with-output-to-string (out)
    (loop
      :with old-len := (length old)
      :for i :from 0
      :while (< i (length str))
      :if (string= old (subseq str i (+ i old-len)))
      :do (progn
            (write-string new out)
            (incf i (1- old-len)))
      :else
      :do (write-char (char str i) out))))

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
  "Adds a newline to the end of a string if it doesn't already have one,
otherwise returns the string unchanged. Before adding the newline all
whitespace characters are truncated from the end of the string."
  (concatenate 'string (string-trim '(#\  #\NewLine) in-str) (string #\Newline)))

(defmacro with-file-content ((file-path str-symbol) &body body)
  "Opens a file and binds the content of the file to the symbol `str-symbol` in
the body. The file is closed after the body has been evaluated."
  (let ((file-path-symbol (gensym)))
    ;; (str-symbol (gensym)))
    `(let ((,file-path-symbol ,file-path))
       (when (not (stringp ,file-path-symbol))
         (error "file-path must be a string"))
       (let ((,str-symbol (file-content-as-str ,file-path-symbol)))
         ,@body))))

(defmacro with-replacing-file ((file-path str-symbol) &body body)
  "Opens a file and binds the content of the file to the symbol `str-symbol` in
the body. The file is closed after the body has been evaluated. If the file
does not exist it is created. If the file exists it is overwritten. Content
of the new or overwritten file is the result of the body.
  Arguments:
    file-path: The path to the file to be opened
    str-symbol: The symbol to which the content of the file is bound in the body
  Example:
    (with-replacing-file (\"/tmp/test.txt\" str)
      \"Hello World!\")"
  (let ((file-path-symbol (gensym)))
    `(let ((,file-path-symbol ,file-path))
       (when (not (stringp ,file-path-symbol))
         (error "file-path must be a string"))
       (let ((,str-symbol (file-content-as-str ,file-path-symbol)))
         (let ((rs (progn ,@body)))
           (str-to-file rs ,file-path-symbol))))))

;;; The `with-tmp-file` macro creates a temporary file using a template string.
;;; The filename and file stream handler are bound to variables within the body of the macro.
;;; The temporary file is automatically deleted after the body executes.

;; (defmacro with-tmp-file ((template-string file-name-variable file-handler-variable) &body body)
;;   `(let* ((,file-name-variable (sb-posix:mktemp ,template-string)))
;;      (unwind-protect
;;           (progn
;;             (with-open-file (,file-handler-variable ,file-name-variable :direction :output :if-exists :supersede)
;;               ,@body))
;;        (if nil
;;          (delete-file ,file-name-variable)
;;          (log-info "File ~a not deleted" ,file-name-variable)))))
;;; Yes, you can use the `values` function to return multiple values.
;;; Updated the macro to return both the filename and the value(s) returned by the body expression when `delete-p` is nil.

(defmacro with-tmp-file-base ((template-string file-name-variable file-handler-variable &key (content nil) (delete-p t)) &body body)
  `(let* ((,file-name-variable ,(sb-posix:mktemp template-string))
           (body-result (unwind-protect
                          (progn
                            (with-open-file (,file-handler-variable ,file-name-variable :direction :output :if-exists :supersede)
                              (when ,content
                                (write-string ,content ,file-handler-variable)
                                (force-output ,file-handler-variable))  ; Flush the content to the file
                              (progn ,@body)))
                          nil)))
     ;; (unwind-protect
     ;;     body-result
     (progn (when ,delete-p (delete-file ,file-name-variable))
       ;; (format t "File ~a deleted: ~a~% " ,file-name-variable ,delete-p)
       (if ,delete-p
         body-result
         (list ,file-name-variable body-result)))));)

(defmacro with-content-tmp-file
  ((template-string file-name-variable file-handler-variable content) &body body)
  "Creates a temporary file with the provided template string and binds the
filename and file handler to the given variables. Writes the provided
content to the file on creation. After executing the body, the file is
deleted.

Parameters:
- template-string: Template string for temporary filename
- file-name-variable: Variable to bind filename to
- file-handler-variable: Variable to bind file handler to
- content: Content to write to the file on creation

Returns:
- Results of body execution"

  `(with-tmp-file-base (,template-string ,file-name-variable ,file-handler-variable :content ,content :delete-p t)
     ,@body))

(defmacro with-calc-tmp-file ((template-string file-name-variable file-handler-variable) &body body)
  "Creates a temporary file with the provided template string and binds the
 filename and file handler to the given variables. After executing the body,
 the file is not deleted. Returns a list containing the results of the body
 execution and the filename.

 Parameters:
 - template-string: Template string for temporary filename
 - file-name-variable: Variable to bind filename to
 - file-handler-variable: Variable to bind file handler to

 Returns:
 - List containing filename and results of body execution"
  `(list
     (with-tmp-file-base (,template-string ,file-name-variable ,file-handler-variable :content nil :delete-p nil)
       (progn ,@body))))

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


