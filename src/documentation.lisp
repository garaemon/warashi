;; documentation.lisp
;;
;; written by R.Ueda (garaemon)

(in-package :warashi)

;; for function
;; (documentation sym 'function)

(defun generate-top-documentation (rst-file)
  (with-open-file (f rst-file :direction :output :if-exists :supersede)
    (format f "===
top
===

.. toctree::
    :maxdepth: 2

    functions
    macros
    variables
")))

(defun generate-functions-documentation (rst-file package)
  (with-open-file (f rst-file :direction :output :if-exists :supersede)
    (format f "=========
functions
=========

external functions
------------------

~A

internal functions
------------------

~A
"
            (generate-external-functions-documentation package)
            (generate-internal-functions-documentation package))))

(defun generate-external-functions-documentation (package)
  (let ((ss (make-string-output-stream)))
    (let ((functions (enumerate-external-functions package)))
      (dolist (f functions)
        (format ss "~%* ~A *~A* ~%~%" (symbol-name f)
                (list->string
                 (SB-KERNEL:%FUN-LAMBDA-LIST (symbol-function f))))
        (if (documentation f 'function)
            (format ss " ~A~%"
                    (documentation f 'cl:function))
          (format ss "~%"))))
    (get-output-stream-string ss)))

(defun generate-internal-functions-documentation (package)
  (let ((ss (make-string-output-stream)))
    (let ((functions (enumerate-internal-functions package)))
      (dolist (f functions)
        (format ss "~%* ~A *~A*~%~%"
                (symbol-name f)
                (list->string
                 (SB-KERNEL:%FUN-LAMBDA-LIST (symbol-function f))))
        (if (documentation f 'function)
            (format ss " ~A~%" 
                    (documentation f 'cl:function))
          (format ss "~%"))))
    (get-output-stream-string ss)))


