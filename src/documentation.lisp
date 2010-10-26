;; documentation.lisp
;;
;; written by R.Ueda (garaemon)

(in-package :warashi)

(defun generate-sphinx-conf-py (target package
                                &key
                                (developer nil)
                                (version nil))
  "generate conf.py required by sphinx."
  (with-open-file (f target :direction :output :if-exists :supersede)
    (format f "
import sys, os
extensions = []

templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'top'
project = u'~A'
copyright = u'~A'
version = '~A'
release = '0'

exclude_patterns = ['_build']
pygments_style = 'sphinx'
html_theme = 'default'
html_static_path = ['_static']
htmlhelp_basename = 'doc'
latex_documents = [
  ('index', '~A.tex', u'~A Documentation',
   u'~A', 'manual'),
]
man_pages = [
    ('index', '~A', u'~A Documentation',
     [u'~A'], 1)
]
"
            package package
            (if version version "0")
            package package package package package package)))

(defun generate-top-documentation (rst-file package)
  "generate a rst file for top page."
  (let* ((package-name (symbol-name package))
         (title-syntax (make-string (length package-name)
                                    :initial-element #\=)))
    (with-open-file (f rst-file :direction :output :if-exists :supersede)
      (format f "~A
~A
~A

.. toctree::
    :maxdepth: 2

    functions
    macros
    variables
" title-syntax package-name title-syntax))))

(defun generate-functions-documentation (rst-file package)
  "generate a rst file describing the functions defined in the
specified package."
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

(defun generate-macros-documentation (rst-file package)
  "generate a rst file describing the macros defined in the specified package."
  (with-open-file (f rst-file :direction :output :if-exists :supersede)
    (format f "======
macros
======

external macros
---------------

~A

internal macros
---------------

~A
"
            (generate-external-macros-documentation package)
            (generate-internal-macros-documentation package))))


(defun generate-variables-documentation (rst-file package)
  "generate a rst file describing the variables defined in the specified
package."
  (with-open-file (f rst-file :direction :output :if-exists :supersede)
    (format f "=========
variables
=========

external variables
------------------

~A

internal variables
------------------

~A
"
            (generate-external-variables-documentation package)
            (generate-internal-variables-documentation package))))

(defun generate-external-functions-documentation (package)
  "returns a string to describe externed functions
with reStructuredText syntax"
  (let ((ss (make-string-output-stream)))
    (let ((functions (enumerate-external-functions package)))
      (dolist (f functions)
        (format ss "~%* ~A *~A* ~%~%" (symbol-name f)
                (list->string (lambda-list (symbol-function f))))
        (if (documentation f 'function)
            (format ss " ~A~%" (add-space-to-string
                                (documentation f 'cl:function)))
          (format ss "~%"))))
    (get-output-stream-string ss)))

(defun generate-internal-functions-documentation (package)
  "returns a string to describe internal functions
with reStructuredText syntax"
  (let ((ss (make-string-output-stream)))
    (let ((functions (enumerate-internal-functions package)))
      (dolist (f functions)
        (format ss "~%* ~A *~A*~%~%"
                (symbol-name f)
                (list->string (lambda-list (symbol-function f))))
        (if (documentation f 'function)
            (format ss " ~A~%" (add-space-to-string
                                (documentation f 'cl:function)))
          (format ss "~%"))))
    (get-output-stream-string ss)))

(defun generate-external-macros-documentation (package)
  "returns a string to describe externed macros
with reStructuredText syntax"
  (let ((ss (make-string-output-stream)))
    (let ((functions (enumerate-external-macros package)))
      (dolist (f functions)
        (format ss "~%* ~A *~A* ~%~%" (symbol-name f)
                (list->string (lambda-list (symbol-function f))))
        (if (documentation f 'function)
            (format ss " ~A~%" (add-space-to-string
                                (documentation f 'cl:function)))
          (format ss "~%"))))
    (get-output-stream-string ss)))

(defun generate-internal-macros-documentation (package)
  "returns a string to describe internal macros
with reStructuredText syntax"
  (let ((ss (make-string-output-stream)))
    (let ((functions (enumerate-internal-macros package)))
      (dolist (f functions)
        (format ss "~%* ~A *~A*~%~%"
                (symbol-name f)
                (list->string
                 (lambda-list (symbol-function f))))
        (if (documentation f 'cl:function)
            (format ss " ~A~%" (add-space-to-string
                                (documentation f 'cl:function)))
          (format ss "~%"))))
    (get-output-stream-string ss)))

(defun generate-external-variables-documentation (package)
  "returns a string to describe externed variables
with reStructuredText syntax"
  (let ((ss (make-string-output-stream)))
    (let ((variables (enumerate-external-variables package)))
      (dolist (v variables)
        (format ss "~%* ~A~%~%" (symbol-name v))
        (if (documentation v 'cl:variable)
            (format ss "~A~%" (add-space-to-string
                               (documentation v 'cl:variable)))
            (format ss "~%"))))
    (get-output-stream-string ss)))

(defun generate-internal-variables-documentation (package)
    "returns a string to describe internal variables
with reStructuredText syntax"
  (let ((ss (make-string-output-stream)))
    (let ((variables (enumerate-internal-variables package)))
      (dolist (v variables)
        (format ss "~%* ~A~%~%" (symbol-name v))
        (if (documentation v 'cl:variable)
            (format ss "~A~%" (add-space-to-string
                               (documentation v 'cl:variable)))
            (format ss "~%"))))
    (get-output-stream-string ss)))
