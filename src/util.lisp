(in-package :cl-format-server)

;;;;; Define some utility functions

(defun read-sexps-from-string (input-string &optional &key (read-fun #'read))
  "Reads sexps (S-expressions) from a string and returns a list of the sexps read.
  The default read function is `read`, but you can pass any function that takes a
  stream as the first argument and returns a sexp.
  Args:
    input-string: The string containing the S-expressions.
    read-fun: The function used for reading the S-expressions.
  Returns:
    A list of S-expressions read from the string.
  "
  (handler-case
    (with-input-from-string (stream input-string)
      (loop
        :for sexp := (funcall read-fun stream nil :eof)
        :until (eq sexp :eof)
        :collect sexp))
    (error (e)
      (log:error "Error reading S-expressions from string: ~A" e)
      nil)))


(defun keywordify (ele &rest suffixes)
  "Converts a string and optional suffixes to a keyword.
  The string is converted to uppercase and concatenated with any suffixes, separated by `-`.
  Args:
    ele: The primary string to convert.
    suffixes: Additional strings to concatenate.
  Returns:
    A keyword generated from the string and suffixes.
  Example:
    (keywordify \"my-fun\" \"file\" \"replace\") => :MY-FUN-FILE-REPLACE "
  (handler-case
    (let* ((clean-str (lambda (str) (string-upcase (string-trim '(#\: #\-) (string str)))))
            (ele (funcall clean-str ele))
            (suffixes (mapcar clean-str suffixes)))
      (intern (concatenate 'string ele (apply #'concatenate 'string (mapcar (lambda (s) (concatenate 'string "-" s)) suffixes))) 'keyword))
    (error (e)
      (log:error "Error in keywordify: ~A" e)
      nil)))


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
  (when (or (string= old "") (< (length str) (length old)))
    (return-from str-replace str))
  (with-output-to-string (out)
    (loop
      :with old-len := (length old)
      :for i :from 0 :below (+ old-len (length str))
      :while (< i (length str))
      :if (and
            (<= i (- (length str)old-len ))
            (string= old (subseq str i (+ i old-len))))
      :do (progn
            (write-string new out)
            (incf i (1- old-len)))
      :else
      :do (when (< i (length str))
            (write-char (char str i) out)))))

(defun str-to-file (str file-path &key (if-exists :supersede))
  "Writes a string to a file at a given path.
  Args:
    str: The string to write.
    file-path: The file path where to write the string.
    if-exists: The action to take if the file already exists. Defaults to :supersede.
  Returns:
    NIL on success, or an error message on failure.
  "
  (handler-case
    (with-open-file (stream file-path :direction :output :if-exists if-exists)
      (write-string str stream)
      (force-output stream))
    (error (e)
      (log:error "Error writing string to file ~A: ~A" file-path e)
      (format nil "Error writing to file: ~A" e))))


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
    :collect (slot-value (car (sb-mop:method-specializers name) )'sb-pcl::object) ))

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

;; (defmacro with-tmp-file ((template-string file-name-variable file-handler-variable) &body body)
;;   `(let* ((,file-name-variable (sb-posix:mktemp ,template-string)))
;;      (unwind-protect
;;           (progn
;;             (with-open-file (,file-handler-variable ,file-name-variable :direction :output :if-exists :supersede)
;;               ,@body))
;;        (if nil
;;          (delete-file ,file-name-variable)
;;          (log-info "File ~a not deleted" ,file-name-variable)))))

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

;;;;; Define the generic function `handle` and a macro `defhandler` adding implementations to `handle`

(defgeneric handle (handler source-code-str)
  (:documentation "Handles the source-code-str with the handler"))

;; (defmethod handle ((handler string) source-code-str)
;;   "Allows specifying `handler' as a string. The string is converted to a keyword
;; and the method is called again."
;;   (handle (intern (string-upcase (string-trim '(#\: #\ ) handler)) 'keyword)
;;     source-code-str))

;; (defmethod handle ((handler (eql :print)) source-code-str)
;;   (print source-code-str))

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
           (progn
             ,@body)))
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


(defun generate-asdf-system-definition (system-name &optional (path "/tmp/"))
  (let ((case-corrected-system-name (string-downcase (concatenate 'string "" system-name)))
         (case-corrected-file-name (string-downcase (concatenate 'string "file" system-name))))
    (format nil "(defsystem ~S :pathname ~S :components ((:file ~S)))"
      case-corrected-system-name path case-corrected-file-name)))

(defmacro with-code-in-temporary-asdf-system (code in-asdf-sys loaded-code-file &body body)
  "Evaluate BODY in the context of a temporary ASDF system. The system is
created with the name IN-ASDF-SYS and the code is written to a file with the
name LOADED-CODE-FILE. The system is loaded and the code is loaded. The
temporary system is deleted after the body has been evaluated.
  Arguments:
    code: The code to be evaluated
    in-asdf-sys: The name of the temporary ASDF system
    loaded-code-file: The name of the file to which the code is written
  Example:
    (with-code-in-temporary-asdf-system
      \"(format nil \\\"Hello, world!\\\") (format nil \\\"dud\\\")\"
      my-temp-system tmp-code-file
      (log-info \"Temp system: ~A\" my-temp-system)
      (log-info \"Temp code: ~A\" tmp-code-file)
      (log-info \"Content Pre: ~A\" (file-content-as-str tmp-code-file))
      (trivial-formatter:fmt my-temp-system :supersede)
      (log-info \"Content Post: ~A\" (file-content-as-str tmp-code-file))
      (newline-str (file-content-as-str tmp-code-file)))
   Returns:
     The result of evaluating BODY"
  `(let* ((,in-asdf-sys (string-downcase (sb-posix:mktemp "tmpXXXXXX")))
           (temp-file-path (format nil "/tmp/~A.asd" ,in-asdf-sys))
           (system-definition (generate-asdf-system-definition ,in-asdf-sys))
           (,loaded-code-file (format nil "/tmp/file~A.lisp" (string-downcase ,in-asdf-sys))))
     (unwind-protect
       (progn
         (with-open-file (stream temp-file-path :direction :output :if-exists :supersede)
           (write-string system-definition stream)
           (force-output stream))
         (with-open-file (stream ,loaded-code-file :direction :output :if-exists :supersede)
           (write-string ,code stream)
           (force-output stream))
         ;; (log-info "Loading ASDF system: ~A" temp-file-path)
         (asdf:load-asd temp-file-path)
         ;; (log-info "Finding system: ~A" ,in-asdf-sys)
         ;; (asdf:find-system temp-file-path)
         ;; (log-info "Loading system: ~A" ,in-asdf-sys)
         ;; (asdf:load-system ,in-asdf-sys)
         (progn
           ,@body))
       ;; Cleanup code
       (progn
         (asdf:clear-system ,in-asdf-sys)
         (delete-file temp-file-path)
         (delete-file ,loaded-code-file)))))

;; (with-code-in-temporary-asdf-system
;;   "(format nil \"Hello, world!\") (format nil \"dud\")" my-temp-system tmp-code-file
;;   (log-info "Temp system: ~A" my-temp-system)
;;   (log-info "Temp code: ~A" tmp-code-file)
;;   (log-info "Content Pre: ~A" (file-content-as-str tmp-code-file))
;;   (trivial-formatter:fmt my-temp-system :supersede)
;;   (log-info "Content Post: ~A" (file-content-as-str tmp-code-file))
;;   (newline-str (file-content-as-str tmp-code-file)))


(defun print-sexps-as-code (input-string)
  "Reads sexps from a string and prints them as formatted Lisp code.
  Args:
    input-string: The string containing the sexps.
  Returns:
    A string with the formatted Lisp code.
  Example:
    (print-sexps-as-code \"(format nil \\\"Hello, world!\\\") (format nil \\\"dud\\\")\")
    => \"(format nil \\\"Hello, world!\\\")\"
       \"(format nil \\\"dud\\\")\""
  (handler-case
    (with-output-to-string (o-stream)
      (with-input-from-string (stream input-string)
        (loop
          :for sexp := (trivial-formatter:read-as-code stream nil :eof)
          :until (eq sexp :eof)
          :do (trivial-formatter:print-as-code sexp o-stream)
          :do (format o-stream "~%") ;; insert ~&?
          )))
    (error (e)
      (log:error "Error in print-sexps-as-code: ~A" e)
      (error "Error reading S-expressions from string"))))

(defun sexps-as-list (input-string)
  "Reads sexps (S-expressions) from a string and returns them as a list of Lisp objects.
  This function is useful for parsing multiple expressions from a single string.
  Args:
    input-string: The string containing the S-expressions.
  Returns:
    A list of Lisp objects corresponding to the S-expressions in the input string.
  Example:
    (sexps-as-list \"(format nil \\\"Hello, world!\\\") (format nil \\\"dud\\\")\")
    => ((FORMAT NIL \"Hello, world!\") (FORMAT NIL \"dud\"))
  "
  (handler-case
    (with-input-from-string (stream input-string)
      (loop
        :for sexp := (trivial-formatter:read-as-code stream nil :eof)
        :until (eq sexp :eof)
        :collect sexp))
    (error (e)
      (log:error "Error in sexps-as-list: ~A" e)
      (error "Error reading S-expressions from string"))))
