;; util.lisp
;;
;; written by R.Ueda (garaemon)

(in-package :warashi)

(defun probe-directory (path)
  "only supports sbcl. returns t if path indicates directory"
  #-sbcl (error "sorry, we does not support your implementation")
  (let ((type (sb-impl::native-file-kind (namestring path))))
    (eq type :directory)))

(defun newline->double-newline (str)
  "replace a newline with two newline."
  (cl-ppcre:regex-replace-all "
" str "

"))

(defun add-space-to-string (str)
  "add a space to the beggining of lines. it means replace a newline
with a space and a newline in string."
  ;; newline -> newline + space
  (cl-ppcre:regex-replace-all "
" str "
 "))

(defun list->string (list)
  "convert a list to string like (format nil \"~A\" string), however,
this function contracts the converted string does not has any newline."
  (cl-ppcre:regex-replace-all "
"
                              (format nil "~A" list)
                              " "))

(defun lambda-list (fun)
  "take a lambda-list from a function object.

 example::

  (lambda-list #'+) => (&REST SB-KERNEL::ARGS)"
  #+sbcl
  (SB-KERNEL:%simple-fun-arglist fun))
