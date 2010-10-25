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
  (cl-ppcre:regex-replace-all "
" str "

"))

(defun add-space-to-string (str)
  ;; newline -> newline + space
  (cl-ppcre:regex-replace-all "
" str "
 "))

(defun list->string (list)
  (cl-ppcre:regex-replace-all "
"
                              (format nil "~A" list)
                              " "))

(defun lambda-list (fun)
  "take a lambda-list of a function object"
  #+sbcl
  (SB-KERNEL:%FUN-LAMBDA-LIST fun))
